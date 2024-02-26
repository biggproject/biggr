roundISOPeriods <- list(
  "P1W" = c("P7D"),
  "P2W" = c("P14D"),
  "P3W" = c("P21D"),
  "P1M" = c("P28D","P29D","P30D","P31D"),
  "P2M" = c("P59D","P60D","P61D","P62D"),
  "P3M" = c("P89D","P90D","P91D","P92D"),
  "P4M" = c("P120D","P121D","P122D","P123D"),
  "P5M" = c("P151D","P152D","P153D","P154D"),
  "P6M" = c("P181D","P182D","P183D","P184D"),
  "P1Y" = c("P364D","P365D","P366D","P367D","P368D")
)

#' Detects time step of a time series
#' 
#' The function infers, i.e. automatically deduce from the input data, the
#' minimum time step (frequency) that can be used for the input time series.
#'
#' @param data <data.frame> containing the input time series 
#' whose time step has to be detected (time column must be named 'time'),
#' or <array> of POSIXct or date objects.
#' @return <string> A string in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).

detect_time_step <- function(data){
  if (class(data)[1]=="POSIXct"){
    data <- data.frame("time"=data)
  }
  if(nrow(data)<=1) {return(NULL)}

  # Calculate iso8601 period between samples
  raw_timesteps <- difftime(sort(data$time),dplyr::lag(sort(data$time),1),units="secs")
  iso_timesteps <- lubridate::format_ISO8601(lubridate::as.period(raw_timesteps[is.finite(raw_timesteps)]))
  # Obtain most common iso8601 period via contingency table
  iso_period <- names(sort(table(iso_timesteps),T))[1]
 
  # Obtain rounded iso8601 period to bypass/support leap years, etc 
  roundISOPeriods_df <- do.call(rbind,lapply(1:length(roundISOPeriods),function(x){
    data.frame("rounded" = names(roundISOPeriods)[x],
               "original" = roundISOPeriods[[x]])}))
  if(iso_period %in% roundISOPeriods_df$original){
    iso_period <- roundISOPeriods_df$rounded[
      mapply(function(x)grepl(paste0("^",x),iso_period),roundISOPeriods_df$original)]
  }
  
  return(iso_period)
}

#' Time casting from hours to ISO 8601 timesteps
#'
#' Supported ISO 8601 timesteps:
#'   - PT1S
#'   - PT1M
#'   - PT1H
#'   - P1D
#'   - P1W
#'   - P1M
#'   - P1Y
#'
#' @param nHours <integer> Number of hours to obtain 
#' @param original_timestep <string> in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).
#' @return <integer> corresponding to the number of hours

hourly_timesteps <- function(nHours, original_timestep) {
  # timesteps_to_hour <- list(
  #   "PT1S" = 3600,
  #   "PT1M" = 60,
  #   "PT1H" = 1,
  #   "P1D" = 1/24,
  #   "P1W" = 1/168,
  #   "P1M" = 1/(30*24),
  #   "P1Y" = 1/(365*24)
  #   )
  ht <- 60*60/(as.numeric(lubridate::as.period(original_timestep)))*nHours
  if(ht<1.05){
    return(ht)
  } else {
    return(ceiling(ht))
  }
}

#' Resample time series to new ISO 8601 timestep
#' 
#' Time series resampled to the desired timestep and the values are aggregated
#' using the aggregation function defined. Timestep of the original series must be
#' shorter than the output timestep of the series.
#'
#' @param data <data.frame> describing the input time series to be resampled. 
#' Time column: 'time', value column: 'value'. If you use weighted average aggregation,
#' you must include a column in data named 'weights'.
#' @param timeStep <string> A string in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).
#' @param func <string> A string defining the function to use in aggregate (options 
#' are: "SUM" to sum up the values at each time step,"WEIGHTED-AVG" to calculate the 
#' weighted average of the values at each time step, "AVG" to calculate the mean of the values
#' at each time step).
#' @param minRatio <float> Minimum percentage of known values
#' in a timestep to calculate the desired aggregation. Default is 0\%. Unit is percentage.
#' @param estimate <boolean> defining if the output value should be an
#' estimated value (theoretical linear approximation considering the time step) 
#' or the real value (gaps are unconsidered, if they exist). 
#' 
#' @return <data.frame> corresponding to the resampled and aggregated time series

resample_and_aggregate <- function(data, timestep, func, minRatio = 0,
                                   estimate = T) {
  
  minRatio <- minRatio/100
  if(!(func %in% c("SUM", "AVG", "WEIGHTED-AVG"))){
    stop("Unknown function to aggregate in argument 'func'")
  }
  if(timestep!=""){
    origin_timestep <- detect_time_step(data$time)
    if(lubridate::as.period(timestep) < lubridate::as.period(origin_timestep)){
      stop(sprintf(
        "Timestep (%s) must be greater or equal to the original timestep (%s) of the series",
        timestep, origin_timestep))
    }
    n <- hourly_timesteps(as.numeric(lubridate::as.period(timestep))/3600, 
                          origin_timestep)
  } else {
    # When no timestep, calculate the indicators only considering the first 365 days
    data <- data %>% filter(time <= min(time,na.rm=T)+days(365))
    n <- nrow(data)
  }
  
  newdata <- data %>%  {
    if(timestep==""){
      group_by(., time = min(time))
    } else {
      group_by(., time = lubridate::floor_date(time, lubridate::as.period(timestep), 
                                                week_start = getOption("lubridate.week.start", 1)))
    }
  } %>% {
    if(func=="SUM") {
      summarise(., count = length(value) ,
                estimated = mean(value, na.rm = T)*n, real = sum(value, na.rm = T))
    } else if(func=="AVG"){
      summarise(., count = length(value) ,
                estimated = mean(value, na.rm = T), real = mean(value, na.rm = T))
    } else if(func=="WEIGHTED-AVG"){
      summarise(., count = length(value),
                estimated = weighted.mean(value, weights, na.rm = T), 
                real = weighted.mean(value, weights, na.rm = T))
    } else {
      .
    }
  } %>%
    filter(count > (n*minRatio)) %>%
    #select(-count) %>%
    mutate(
      value = if (estimate == T) { estimated } else { real }, 
      isReal = ifelse(!is.finite(value), NA, ifelse(count==n, T, F))
    )# %>% 
    #select(-real, -estimated)
  
  return(newdata)
}

#' Detect min-max outliers in time series
#' 
#' Detect elements of the time series outside a minimum and maximum range.
#' Additionally, with the minSeries and maxSeries arguments, this ranges can
#' be set differently along the period. When using this feature, the timestep
#' of both minSeries and maxSeries need to be resampled to the original
#' frequency of the data time series, applying forward fill if is needed
#' (Remember the time stamps of each time series element always represent the
#' begining of the time step).
#'
#' @param data <data.frame> describing the input time series whose outliers need 
#' to be detected. Time column: 'time', value column: 'value'.
#' @param min <float> describing the minimum value allowed for each element 
#' of the time series.
#' @param max <float> describing the maximum value allowed for each element 
#' of the time series.
#' @param minSeries <data.frame> defining the time series with minimum allowed 
#' values. Time column: 'time', value column: 'value'.
#' @param maxSeries <data.frame> defining the time series with maximum allowed 
#' values. Time column: 'time', value column: 'value'.
#' @return <data.frame> with boolean values representing whether a item is an 
#' outlier, or not.

detect_ts_min_max_outliers <- function(data, min, max, minSeries = NULL, maxSeries = NULL) {
  min_mask <- (data$value < min)
  max_mask <- (data$value > max)

  if (!is.null(minSeries)) {
    timestep <- detect_time_step(data)
    #minSeries <- resample(minSeries, timestep)
    mask <- data %>%
      left_join(minSeries, by = "time") %>%
      mutate(outlier = value.x < value.y)
    min_mask <- mask[["outlier"]]
  }
  if (!is.null(maxSeries)) {
    timestep <- detect_time_step(data)
    #maxSeries <- resample(maxSeries, timestep)
    mask <- data %>%
      left_join(maxSeries, by = "time") %>%
      mutate(outlier = value.x > value.y)
    max_mask <- mask[["outlier"]]
  }
  return(min_mask | max_mask)
}

#' Detect Z-score outliers in time series.
#' 
#' Detect elements of the time series out of a Z-score threshold,
#' applied on the whole timeseries or a rolling window of predefined width.
#' 
#' @param data <data.frame> describing the input time series whose outliers need 
#' to be detected. Time column: 'time', value column: 'value'.
#' @param zScoreThreshold <float> describing the threshold of the
#' Z-score calculation.
#' @param window <string> in ISO 8601 format representing the window
#' (e.g. "P7D","P1D", "PT168H" ,...). This is an optional argument setting
#' the width of the rolling window where the Z-normalization calculation
#' is considered. This argument allows to adapt the outlier filtering depending
#' the dynamics of the signal itself. Default value is "NULL", thus no rolling
#' window is considered.
#' @param zScoreExtremesSensitive <boolean> defining if the
#' aggregation function of the Zscore is the mean (true), or median(false).
#' The first one makes the Z-score sensitive to extreme values, and the
#' second no. Default is true.
#' @param na.rm <boolean> describing if NAN values should be removed 
#' in mean, median and sd functions
#' @return <data.frame> with boolean values representing whether a item is an 
#' outlier, or not.

detect_ts_zscore_outliers <- function(data, zScoreThreshold, window = NULL, zScoreExtremesSensitive = TRUE, na.rm=T) {
  func <- ifelse(zScoreExtremesSensitive == TRUE, mean, median)
  zScore <- ((data$value - func(data$value,na.rm=T)) / sd(data$value,na.rm=na.rm))
  if (!is.null(window)) {
    timestep <- detect_time_step(data)
    # timesteps_ <- invert(timesteps)
    secs <- as.integer(lubridate::duration(window, units = "seconds"))
    width <- secs / as.integer(lubridate::duration(timestep,units="secs"))
    zScore <- roll::roll_scale(data$value, width = width, min_obs = 1)
    # TEMPFIX
    zScore[1] <- zScore[2]
  }
  return(abs(zScore) >= zScoreThreshold)
}

#' Detect outliers in time series based on a calendar model (applied over a window of time)
#' 
#' Detect elements of the time series out of a confidence threshold based
#' on linear model of the calendar variables (month, weekday, hour).
#' Detection done in an specific window of data
#'
#' @param data timeSeries An argument containing the time series from which
#' the outliers need to be detected.
#' @param calendarFeatures: list of strings An optional argument set the
#' calendar features of the model. Default values are: ["HOL*intercept","H"]
#' @param mode: string An optional argument setting which outliers need to be
#' filtered, the ones upper, or the ones lower to the prediction.
#' Default is "upperAndLower".
#' @param upperModelPercentile: float An optional argument defining the
#' percentile used in the quantile regression model for the upper prediction.
#' Default is 90.
#' @param lowerModelPercentile: float An optional argument defining the
#' percentile used in the quantile regression model for the lower prediction.
#' Default is 10.
#' @param upperPercentualThreshold: float It sets the dynamic upper threshold
#' to detect outliers. It is an optional argument to define the percentage
#' of difference added to the upper model predition itself. Default is 30.
#' @param lowerPercentualThreshold: float It sets the dynamic lower threshold
#' to detect outliers. An optional argument to define the percentage of
#' difference substracted to the predition of the model. Default is 30.
#' @param holidaysCalendar list of dates An optional list giving the dates
#' where local or national holidays related to the location of the data
#' argument. Default is empty list.
#' @param daysThatAreOutliers list of outlier dates
#' @param outputPredictors: boolean. Include calendar regression model prediction
#' results as output
#' @param logValueColumn: boolean. Transform value column (log-transformation)
#' @return <data.frame> with boolean values representing whether a item is an 
#' outlier, or not. Additionally, the predition results of the model are also
#' presented.
detect_ts_calendar_model_outliers_window <- function(data,
                                                     calendarFeatures = c("HOL", "H"),
                                                     mode = "upperAndLower",
                                                     upperModelPercentile = 90,
                                                     lowerModelPercentile = 10,
                                                     upperPercentualThreshold = 30,
                                                     lowerPercentualThreshold = 30,
                                                     holidaysCalendar = c(),
                                                     daysThatAreOutliers = c(),
                                                     outputPredictors = F,
                                                     logValueColumn = F) {

  newdata <- data %>%
    mutate(
      H = (lubridate::hour(time)) / 24,
      U = (as.numeric(strftime(time,"%u"))-1) / 7,
      W = as.numeric(strftime(time,"%U")) / 53,
      m = (as.numeric(strftime(time,"%m"))-1) / 12,
      HOL = lubridate::as_date(time) %in% holidaysCalendar | strftime(time,"%u") %in% c("6","7"),
      intercept = 1
    )
  if (logValueColumn) newdata$value <- log(newdata$value+1)
  cols_to_fs <- c("H", "U", "W", "m")

  # Identify which calendar feature are included in the quantile regression model
  # used as baseline
  calendarFeatures <- ifelse(calendarFeatures %in% cols_to_fs, 
                             paste0("as.matrix(fs(", calendarFeatures, ",featureName=\"", 
                                    calendarFeatures,"\",nHarmonics=3))"), calendarFeatures)
  formula <- as.formula(paste("value", paste0("0 +", paste(calendarFeatures, collapse = ":")),
    sep = "~"
  ))
  newdata$value <- ifelse(is.finite(newdata$value),newdata$value,NA)

  # Use quantile regression to obtain expected values under specific quantiles
  model <- tryCatch(
    rq(
      formula,
      tau = c(lowerModelPercentile / 100.0, upperModelPercentile / 100.0),
      data = newdata[complete.cases(newdata),]),
    error=function(e)NULL)
  if(is.null(model)){
    prediction <- data.frame("lower"=newdata$value,"upper"=newdata$value)
  } else {
    prediction <- as.data.frame(predict.rq(model, newdata))
    names(prediction) <- c("lower", "upper")
  }
  if (logValueColumn) {
    prediction$lower <- exp(prediction$lower)-1
    prediction$upper <- exp(prediction$upper)-1
  }
  prediction$lower <- floor(prediction$lower)
  prediction$lower <- ifelse(prediction$lower>=0,prediction$lower,0)
  prediction$upper <- ceiling(prediction$upper)
  prediction$upper <- ifelse(prediction$upper>=0,prediction$upper,0)
  
  lowerPrediction <- (1 - lowerPercentualThreshold / 100.0) * prediction$lower
  upperPrediction <- (1 + upperPercentualThreshold / 100.0) * prediction$upper

  # Use percentual threshold in order to identify outliers over the quantile
  # baseline already obtained
  if (mode == "lower") {
    mask <- data$value < lowerPrediction
  } else if (mode == "upper") {
    mask <- data$value > upperPrediction
  } else if (mode == "upperAndLower") {
    mask <- (data$value < lowerPrediction) | (data$value > upperPrediction)
  } 
  mask <- (as.Date(data$time) %in% daysThatAreOutliers) | mask # No need to define tz, time is localtime.
  if (outputPredictors){
    return(data.frame("time" = data$time, "outliers" = mask, 
                      setNames(prediction,c("lowerPredCalendarModel","upperPredCalendarModel"))))
  } else {
    return(data.frame("time" = data$time, "outliers" = mask))
  }
}

#' Detect profiled days in data
#' 
#' This function detects if there are profiled days in a consumption 
#' time series.
#'
#' @param data <data.frame> with potential profiled content.
#' @param timeColumn <string> identifying the local time of the 
#' time series.
#' @param valueColumn <string> identifying the value of the time
#' series.
#' @return <array> of dates that have a high probability to be profiled. 

detect_profiled_data <- function(data, timeColumn = "localtime", valueColumn = "Qe"){
  if(lubridate::period("PT1H")<=detect_time_step(data[,timeColumn])){
    tz <- lubridate::tz(data[,timeColumn])
    data <- data.frame(
      "time" = data[,timeColumn],
      "value" = data[,valueColumn],
      "date" = as.Date(data[,timeColumn],tz=tz),
      "hour" = lubridate::hour(data[,timeColumn])
    )
    # Create wide data frame with aggregated hourly demand per
    # each of the days
    data_wide <- data %>% 
      mutate(
        "hourly_lt"=as.POSIXct(format(time,"%Y-%m-%d %H"),
                               format="%Y-%m-%d %H",
                               tz = tz)
      ) %>%
      group_by(hourly_lt) %>%
      summarise(value=sum(value,na.rm=T),
                date=dplyr::first(date),
                hour=dplyr::first(hour)) %>%
      left_join(
        data %>% 
          group_by(date) %>%
          summarise(daily = sum(value,na.rm=T),
                    dailymin = min(ifelse(is.finite(value),value,NA)),
                    dailymax = max(ifelse(is.finite(value),value,NA))),
        by="date"
      ) %>%
      mutate(value = round(value/daily,5)*100) %>%
      pivot_wider(id_cols = c(date,daily), names_from=hour, values_from = value, names_sort = T)
    data_wide$event <- apply(as.matrix(data_wide %>% select(-date)),1,paste,collapse="~")
    # Find duplicated days having same daily pattern. Same
    # consumption in each hour of the day
    date_duplicated <- data_wide %>% 
      filter(
        ((event == lag(event,1) | event == lag(event,7) | event == lag(event,3) )
         ) & (daily > quantile(daily,0.1,na.rm=T))
        ) %>%
      select(date) %>% unlist() 
    if(length(date_duplicated)>0){ 
      date_duplicated <- date_duplicated %>% 
        as.Date(tz=tz, origin=as.Date("1970-01-01",tz=tz))
    }
    return(date_duplicated)
  } else {
    warning("Profiled data can only be detected in hourly or quarterhourly time series")
    return(c())
  }
}

#' Detect outliers in time series based on a calendar model
#' 
#' Detect elements of the time series out of a confidence threshold based
#' on linear model of the calendar variables (month, weekday, hour).
#'
#' @param data <data.frame> An argument containing the time series from which
#' the outliers need to be detected.
#' @param localTimeColumn: string Local time column name
#' @param valueColumn: string Value column name
#' @param calendarFeatures: list of strings An optional argument set the
#' calendar features of the model. Default values are: ["HOL*intercept","H"]
#' @param mode: string An optional argument setting which outliers need to be
#' filtered, the ones upper, or the ones lower to the prediction.
#' Default is "upperAndLower".
#' @param upperModelPercentile: float An optional argument defining the
#' percentile used in the quantile regression model for the upper prediction.
#' Default is 90.
#' @param lowerModelPercentile: float An optional argument defining the
#' percentile used in the quantile regression model for the lower prediction.
#' Default is 10.
#' @param upperPercentualThreshold: float It sets the dynamic upper threshold
#' to detect outliers. It is an optional argument to define the percentage
#' of difference added to the upper model predition itself. Default is 30.
#' @param lowerPercentualThreshold: float It sets the dynamic lower threshold
#' to detect outliers. An optional argument to define the percentage of
#' difference substracted to the predition of the model. () Default is 30.
#' @param holidaysCalendar list of dates An optional list giving the dates
#' where local or national holidays related to the location of the data
#' argument. Default is empty list.
#' @param daysThatAreOutliers list of outlier dates
#' @param window string. A string in ISO 8601 format representing the
#' window (e.g. "P2M","P4M","P14D",...). This is an optional argument setting
#' the width of the window where the model is trained and evaluated
#' @param outputPredictor: boolean. Include calendar regression model prediction
#' results as output
#' @param logValueColumn: boolean. Transform value column (log-transformation)
#' @param autoDetectProfiled: boolean. Detect and ignore profiled days from
#' timeseries
#' @return timeSeries with time index outliers mask and optional calendar
#' regression model prediction

detect_ts_calendar_model_outliers <- function(data,
                                              localTimeColumn="localtime",
                                              valueColumn=outputName,
                                              calendarFeatures = c("HOL","H"),
                                              mode = "upperAndLower",
                                              upperModelPercentile = 90,
                                              lowerModelPercentile = 10,
                                              upperPercentualThreshold = 30,
                                              lowerPercentualThreshold = 30,
                                              holidaysCalendar = c(),
                                              daysThatAreOutliers = c(),
                                              window = NULL,
                                              outputPredictors = F,
                                              logValueColumn = F,
                                              autoDetectProfiled = T) {
  
  if(autoDetectProfiled==T)
    # Ignore days if are profiled
    daysThatAreOutliers <- c(daysThatAreOutliers, detect_profiled_data(data))
  
  if(class(window)=="numeric"){
    start <- as.integer(lubridate::seconds(data$time[1]))
    # Windowize data in order to obtain outliers in data samples
    windowsize <- as.integer(duration(window, units = "seconds"))
    data <- data %>%
      mutate(
        window = (as.integer(lubridate::seconds(time)) - start) %/% windowsize
      )
  } else if (class(window)=="character" && startsWith(window,"P")) {
    start <- as.integer(lubridate::seconds(data$time[1]))
    # Windowize data in order to obtain outliers in data samples
    windowsize <- as.integer(lubridate::as.duration(window))
    data <- data %>%
      mutate(
        window = (as.integer(lubridate::seconds(time)) - start) %/% windowsize
      )
  } else if (class(window)=="character") {
    data <- data %>% unite("window",window,sep="~")
  } else {
    data$window <- 0
  }
  data <- setNames(data[,c(localTimeColumn, valueColumn, "window")],c("time","value","window"))
  if (is.null(window)) {
    result <- detect_ts_calendar_model_outliers_window(
        data,
        calendarFeatures,
        mode,
        upperModelPercentile,
        lowerModelPercentile,
        upperPercentualThreshold,
        lowerPercentualThreshold,
        holidaysCalendar,
        daysThatAreOutliers,
        outputPredictors,
        logValueColumn
      )
    colnames(result)[1] <- localTimeColumn
    return(result)
  } else {
    newdata <- data
    windows <- unique(newdata$window)
    result <- lapply(windows, function(w) {
      tmp <- newdata[newdata$window == w,]
      result <- detect_ts_calendar_model_outliers_window(
        data=if(class(window)=="integer"){
          newdata[newdata$window %in% c((w-3):(w)),]
        } else {
          newdata[newdata$window == w,]
        },
        calendarFeatures,
        mode,
        upperModelPercentile,
        lowerModelPercentile,
        upperPercentualThreshold,
        lowerPercentualThreshold,
        holidaysCalendar,
        daysThatAreOutliers,
        outputPredictors,
        logValueColumn
      )
      merge(tmp, result, by="time")
    })
    result <- do.call(rbind,result)
    colnames(result)[1] <- localTimeColumn
    return(result)
  }
}

#' Detect min-max outliers in static data.
#' 
#' Detect which numerical elements are outside the min-max range.
#' 
#' @param data <array> of floats corresponding to the numerical elements to be evaluated
#' @param min <float> The minimum value of the range.
#' @param max <float> The maximum value of the range.
#' @param includedMin <boolean> An optional argument setting if the
#'  minimum value should be included as valid value (T), or not (F).
#'  Default is T.
#' @param includedMax <boolean> An optional argument setting if the maximum
#'  value should be included as valid value (T), or not (F).
#'  Default is T.
#' @return <array> of booleans, representing the outliers of input data argument.

detect_static_min_max_outliers <- function(data, min, max, includeMin = TRUE, includeMax = TRUE) {
  min_mask <- (data < min)
  max_mask <- (data > max)
  if (includeMin == FALSE) min_mask <- (data <= min)
  if (includeMax == FALSE) max_mask <- (data >= max)
  return(min_mask | max_mask)
}

#' Detect reg-exp in static data.
#' 
#' Detect which string element satisfy the regular expression
#'
#' @param data <array> of strings, representing the elements to be evaluated.
#' @param regExpValues <array> of regular expressions to be
#' checked.
#' @param negativeExp <boolean> An optional argument to evaluate the inverse
#' result of the regular expressions. Default is false, thus all elements
#' that are satified by any of the regular expressions are true.
#' @return <array> of booleans, representing the evaluation results made
#' for each data item.

detect_static_reg_exp <- function(data, regExpValues, negativeExp = FALSE) {
  mask <- sapply(regExpValues, function(p) {
    grepl(p, data)
  })
  values <- rowSums(mask) > 0
  if (negativeExp == TRUE) values <- rowSums(!mask) == length(regExpValues)
  return(values)
}

#' This function imputates values to Not Available (NA) elements of a time
#' series, based on the outliers estimation made the functions implemented
#' in Outlier Detection module block of this library.
#'
#' @param data <data.frame> with Not Available elements to be filled. 
#' Columns: 'time', 'value'.
#' @param outliersMinMax detect_ts_min_max_outliers() output.
#' @param outliersZScore detect_ts_zscore_outliers() output.
#' @param outliersCalendarModel detect_ts_calendar_model_outliers() output.
#' @param methodFillNA <string> argument specifying the methodology for
#' filling the NA elements. Possible values are:
#'    - calendarModel: The predicted values estimated by the calendar
#' model are used to fulfill the NA elements.
#'    - backward: The previous known element of the timeseries is
#' considered.
#'    - forward: The next known element of the timeseries is considered.
#'    - linearInterpolation: A linear interpolation is done between
#' using previous and next known elements regarding each data gap.
#' @param maxGap <string> in ISO 8601 format representing the
#' window (e.g. "PT4H", "PT30M", "PT72H", "P2D",...). It defines the maximum
#' period allowed. Therefore, gaps with greater period are not considered
#' in the imputation. By default, it doesn't exists a limitation of the
#' gap longitude.
#' @param fillMask <array> of booleans defining the time periods where the
#' imputation can be done. By default, all elements of the timeseries
#' can be filled.
#' @return <data.frame> with filled elements.

fill_ts_na <- function(data, outliersMinMax, outliersZScore, outliersCalendarModel, 
                       methodFillNA = "linearInterpolation", maxGap = NULL, fillMask = NULL) {
  
  newdata <- data
  if (!is.null(fillMask)) mask <- mask & fillMask
  if (!is.null(maxGap)) {
    missing <- data %>%
      drop_na(.) %>%
      mutate(
        time_lag1 = dplyr::lag(time, 1),
        secs = as.integer(
          difftime(time, time_lag1, units = "secs")
        ),
        valid = secs <= as.integer(duration(maxGap))
      )
    # TOBEFIXED: Last valid reading ignored
    mask <- data %>%
      left_join(missing, by = "time") %>%
      fill(valid, .direction = "up")
    newdata <- data[mask[["valid"]] == TRUE, ]
  }

  if (methodFillNA == "calendarModel") {
    stop("Not yet implemented")
  } else if (methodFillNA == "backward") {
    newdata <- newdata %>%
      fill(value, .direction = "up")
    colnames(newdata) <- c("time", "value.fill")
  } else if (methodFillNA == "forward") {
    newdata <- newdata %>%
      fill(value, .direction = "down")
    colnames(newdata) <- c("time", "value.fill")
  } else if (methodFillNA == "linearInterpolation") {
    newdata$value.fill <- as.vector(na.approx(newdata$value, rule = 2))
  } else {
    stop("Fill method not supported")
  }
  return(data %>%
    left_join(newdata) %>%
    mutate(value = coalesce(value, value.fill)) %>%
    select(time, value))
}

#' Round up value or array
#'
#' This function is used for rounding
#'
#' @param x <array> of floats to be rounded
#' @return <array> of floats corresponding to the rounded value
round_up <- function(x) 10^ceiling(log10(x))

#' Converts time series to instantaneous
#' 
#' The function converts a cumulative or on-change measurement
#' to instantaneous.
#'
#' @param data <data.frame> of the cumulative or on-change time series that has to
#' be converted to instantaneous. An instantaneous measurement is a gauge
#' metric, in which the value can increase or decrease and measures a specific
#' instant in time. Columns: 'time', 'value'.
#' @param measurementReadingType <string> that identifies used to define the
#' measurement reading type. Supported readiny types are:
#'     - onChange. Delta reading type
#'     - cumulative. Counter reading type
#' @return <data.frame> The cumulative or onChange time series with
#' the measurements converted to the instantaneous type.

clean_ts_integrate <- function(data, measurementReadingType = "onChange") {
  if (measurementReadingType == "onChange") {
    return(data %>% mutate(value = lead(value, 1)))
  } else if (measurementReadingType == "cumulative") {
    # WARNING: Not handling series with false-positive roll over
    return(data %>%
      mutate(
        value_lead1 = lead(value, 1),
        delta = value_lead1 - value,
        value = ifelse(delta < 0, round_up(value) - value + value_lead1, delta)
      ) %>%
      select(c(time, value)))
  } else {
    return(data)
  }
}

#' Align a time series to an specific time grid.
#' 
#' The function aligns the frequency of the input time series with the output
#' frequency given as an argument using the specified aggregation function.
#'
#' @param data <data.frame> containing the time series that has to be aligned 
#' with an output time step, i.e. with a specific frequency. If the
#' measurementReadingType of the series is not instantaneous, the data must
#' be converted first using the function clean_ts_integrate.
#' @param timeColumn <string> identifying time column in data time series
#' @param valueColumn <string> identifying value column in data time series
#' @param isRealColumn <string> identifying the column in data time series
#' that describes whether a value is real (T) or estimated (F).
#' @param outputFrequency <string>. The frequency used to resample the input
#' time series for the alignment. It must be a string in ISO 8601 format
#' representing the time step (e.g. "PT15M","PT1H", "P1M", "P1D",...).
#' @param aggregationFunctions <array> of strings. The aggregation 
#' functions to use when resampling the series. 
#' Supported aggregation functions: AVG, SUM, MIN, MAX, 
#' HDD (base temperature 20, 21, 22), CDD (base temperature 20, 21, 22).
#' @param aggregationFunctionsSuffix <string> added as a suffix in resultant
#' column values. Optional.
#' @param useEstimatedValued <boolean>. Do not ignore estimated values in input
#' time series.
#' @param tz <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). 
#' @return <data.frame>. The time series resampled with the specified
#' period and aggregation functions.

align_time_grid <- function(data, 
                            timeColumn="time", valueColumn="value",
                            isRealColumn="isReal", outputFrequency, 
                            aggregationFunctions=c("SUM","AVG","MIN","MAX"), 
                            aggregationFunctionsSuffix=NULL,
                            useEstimatedValues=F, tz="UTC") {
  
  data <- data %>% rename(
    time = timeColumn,
    value = valueColumn,
    isReal = isRealColumn
  )
  
  data$time <- lubridate::with_tz(data$time,tz)

  # Remove estimated values from time series
  if(useEstimatedValues==F && ("isReal" %in% colnames(data))){
    dataWithoutEstimates <- data %>% 
      filter(isReal==T)
    if(nrow(dataWithoutEstimates)==0){
      warning("The whole timeseries contains estimated values, 
           and they are not allowed during the alignment process. 
           Consider useEstimatedValues=T if you allow their usage.")
      return(NULL)
    }
    data <- dataWithoutEstimates
  }
  
  freq_in_secs <- as.numeric(lubridate::period(detect_time_step(data),units = "seconds"))

  # Check whether input time serie timestep is higher than requestd
  # frequency
  if(lubridate::period(detect_time_step(data)) >= lubridate::period(outputFrequency) ){
    # Input time serie timestep is higher than requested frequency
    # so no aggregation is applied
    results <- data %>%
      mutate(
        SUM = value,
        AVG = value,
        MIN = value,
        MAX = value,
        GAPS = 0,
        RATIO = 1
      )
  } else {
    # Input time serie timestep is lower than requested frequency
    # so multiple aggregation method are applied in order to
    # resample data
    results <- data %>%
      mutate(time = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                          week_start = getOption("lubridate.week.start", 1)
      )) %>%
      group_by(time) %>%
      summarize(
        SUM = sum(value, na.rm = TRUE),
        AVG = mean(value, na.rm = TRUE),
        MIN = min(value, na.rm = TRUE),
        MAX = max(value, na.rm = TRUE),
        GAPS = sum(is.na(value)) / length(value),
        RATIO = if(outputFrequency %in% c("P1M")){
          sum(!is.na(value))*freq_in_secs /
            (lubridate::days_in_month(time[1])*86400)
        } else {
          sum(!is.na(value))*freq_in_secs /
            as.numeric(lubridate::seconds(lubridate::period(outputFrequency)))
        }
      ) %>%
      ungroup()
  }
  results <- results %>%
    select(time, all_of(unlist(aggregationFunctions,use.names = F)[aggregationFunctions %in% c("AVG","SUM","MIN","MAX")]), 
           GAPS, RATIO)
  if (any(c("HDD","CDD") %in% unlist(aggregationFunctions,use.names = F))){
    daily_data <- data %>%
      mutate(time = lubridate::floor_date(time, lubridate::period("P1D"),
                                          week_start = getOption("lubridate.week.start", 1)
      )) %>%
      group_by(time) %>%
      summarize(
        AVG = mean(value, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(time, AVG)
    if ("HDD" %in% unlist(aggregationFunctions,use.names = F)){
      results_hdd <- degree_days(
        data = daily_data,
        temperatureFeature = "AVG",
        localTimeZone = tz,
        baseTemperature = c(20,21,22),
        mode = "heating",
        outputFrequency = outputFrequency,
        outputFeaturesName = "HDD")
      results <- merge(results, results_hdd, by = "time")
    }
    if ("CDD" %in% unlist(aggregationFunctions,use.names = F)){
      results_cdd <- degree_days(
        data = daily_data,
        temperatureFeature = "AVG",
        localTimeZone = tz,
        baseTemperature = c(20,21,22),
        mode = "cooling",
        outputFrequency = outputFrequency,
        outputFeaturesName = "CDD")
      results <- merge(results, results_cdd, by = "time")
    }
  }
  if(!is.null(aggregationFunctionsSuffix)){
    colnames(results) <- ifelse(!(colnames(results) %in% c("time","RATIO","GAPS")),
                                paste0(colnames(results),"_",aggregationFunctionsSuffix), 
                                colnames(results))
  }
  return(results)
}

#' Detect disruptive periods in consumption time series.
#' 
#' This function detects a disruptive period in a consumption time series. 
#' It evaluates different date ranges to find the most suitable one containing a 
#' disruptive period that should not be considered in the training phases of 
#' statistical models. An example of a disruptive period could be the Covid lockdown 
#' when building consumption radically changes due to the different occupancy and 
#' activity patterns. The model used to detect the disruptive period considers the 
#' consumption relationship between weekdays and temperature. 
#'
#' @param data <data.frame> Time series with potential anomalies in
#' values. It should contain a time column, a consumption column and a temperature column.
#' @param consumptionColumn <string> Consumption column in data time series
#' @param timeColumn <string> Time column in data time series
#' @param temperatureColumn <string> Temperature column in data time series
#' @param tz <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). 
#' @param minIniDate <date> Minimum date to start looking for anomalies
#' @param maxIniDate <date> Maximum date to start looking for anomalies
#' @param minEndDate <date> Minimum date to stop looking for anomalies
#' @param maxEndDate <date> Maximum date to stop looking for anomalies
#' @param checkFor <string> Which effect will the disruptive
#' event generate? e.g. the SARS-CoV2 pandemia decrease the energy consumption of tertiary buildings,
#' so in that case we should check for decrement in consumption 
#' Possible values: decrement, increment, incrementAndDecrement.
#' @param minDecrementPercentualAffectation <float> indicating the minimum
#' decrement of consumption (vs. baseline) to be considered as abnormal.
#' Default 30\%.
#' @param minIncrementPercentualAffectation <float> indicating the minimum
#' increment of consumption (vs. baseline) to be considered as abnormal.
#' Default 60\%.
#' @return <data.frame> with the period of time (min-max) with abnormal 
#' consumption.

detect_disruptive_period <- function(data, consumptionColumn, timeColumn, 
                                     temperatureColumn, tz, 
                                     minIniDate, maxIniDate,
                                     minEndDate, maxEndDate, checkFor="decrement",
                                     minDecrementPercentualAffectation = 30,
                                     minIncrementPercentualAffectation = 60){
  
  n_timesteps <- hourly_timesteps(720,detect_time_step(data$time))
  # In case of greater frequency than daily (thus monthly, yearly ...)
  if(lubridate::as.period(detect_time_step(data)) > lubridate::as.period("P1D")){
    return(data.frame("minDate"=NA,"maxDate"=NA))
  # In case of daily, hourly or quarterhourly data
  } else {
    data_consumption <- data %>% 
      group_by("time" = lubridate::floor_date(
        lubridate::with_tz(time,tz),
        unit = lubridate::as.period("P1D"),
        week_start = getOption("lubridate.week.start", 1))) %>%
      summarise(
        across(consumptionColumn, ~mean(.x*n_timesteps,na.rm=T),.names = "consumption")
      )
    data_hdd <- degree_days(
      data = data,
      temperatureFeature = temperatureColumn,
      localTimeZone = tz,
      baseTemperature = seq(14,20,by=2),
      mode = "heating",
      hysteresisBaseTemperature = 0,
      outputFrequency = "P1D",
      outputFeaturesName = "HDD")
    data_cdd <- degree_days(
      data = data,
      temperatureFeature = temperatureColumn,
      localTimeZone = tz,
      baseTemperature = seq(20,26,by=2),#seq(16,28,by=3),
      mode = "cooling",
      hysteresisBaseTemperature = 0,
      outputFrequency = "P1D",
      outputFeaturesName = "CDD")
    data_daily <- data_consumption %>% left_join(data_hdd,by="time") %>% 
      left_join(data_cdd,by="time")
    data_daily <- data_daily[complete.cases(data_daily),]
  }
  
  disruptive_period_model <- function(dataM, minDate, maxDate, checkFor,
                                      minIniDate, maxEndDate, 
                                      minDecrementPercentualAffectation,
                                      minIncrementPercentualAffectation){

    data_d <- tryCatch({
      data.frame(
        "time" = seq.Date(as.Date(min(dataM$time)), 
                          as.Date(max(dataM$time) + months(1) - lubridate::days(1)), 
                          by="days"))},
      error = function(e){
        data.frame(
          "time" = seq.Date(as.Date(min(dataM$time)), 
                            as.Date(max(dataM$time) + lubridate::days(30)), 
                            by="days"))})
    data_d$disruptive <- ifelse(data_d$time >= minDate & data_d$time <= maxDate,
                                1,0)
    data_d$testing <- ifelse(data_d$time >= minIniDate & data_d$time <= maxEndDate,
                                1,0)
    if( nrow(dataM)<(12*30) ){
      return(NA)
    } else {
      dataMD <- data_d %>% 
        group_by(
          "time" = lubridate::floor_date(time,
            unit = lubridate::as.period("P1D"),
            week_start = getOption("lubridate.week.start", 1))
        ) %>% 
        summarise(
                  "weekday" = factor(format(time, "%w")),
                  "disruptive" = sum(disruptive),
                  "testing" = sum(testing),
                  "disruptive_f" = as.factor(disruptive>0),
                  "testing_f"= as.factor(testing>0) )
      dataM <- dataM %>% left_join(dataMD,by="time")
      if(sum(dataM$disruptive_f==T) <= 30 ||
         sum(dataM$disruptive_f==F) < 70){
        return(NA)
      }
      cases <- expand.grid(HDD=colnames(dataM)[grepl("^HDD",colnames(dataM))],
                  CDD=colnames(dataM)[grepl("^CDD",colnames(dataM))])
      err <- mapply(1:nrow(cases), FUN = function(i){
        # i=1
        dataM$HDD <- unlist(dataM[cases$HDD[i]])
        dataM$CDD <- unlist(dataM[cases$CDD[i]])
        dataM$time_ <- as.numeric(dataM$time - dataM$time[1])
        if(mean(dataM$HDD)==0 || mean(dataM$CDD)==0) return(NA)
        if(nrow(dataM)>=(36*30)){
          mod <- lm(consumption ~ 0 + weekday:disruptive_f + HDD:disruptive_f + CDD:disruptive_f, data=dataM)
        } else if (nrow(dataM)>=(18*30)){
          mod <- lm(consumption ~ 0 + disruptive_f + HDD + CDD, data=dataM)
        } else if (nrow(dataM)<(18*30)){
          mod <- lm(consumption ~ 0 + disruptive_f + HDD + CDD, data=dataM)
        }
        dataNoEff <- dataM 
        dataNoEff$disruptive_f <- factor(F,levels=c(F,T))
        pred <- predict(mod,dataM)
        predNoEff <- predict(mod,dataNoEff)
        real <- dataM$consumption
        rmseR <- 
          RMSE(real[dataM$testing_f==T],pred[dataM$testing_f==T])
        return(if(checkFor=="decrement" && 
                  sum(pred[dataM$disruptive_f==T])<=
                   ((100-minDecrementPercentualAffectation)/100*sum(predNoEff[dataM$disruptive_f==T]))){
          rmseR
        } else if(checkFor=="increment" && 
                  sum(pred[dataM$disruptive_f==T])>=
                    ((100+minIncrementPercentualAffectation)/100*sum(predNoEff[dataM$disruptive_f==T]))){
          rmseR
        } else if(!(checkFor %in% c("increment","decrement")) && (
                  (sum(pred[dataM$disruptive_f==T])>=
                  ((100+minIncrementPercentualAffectation)/100*sum(predNoEff[dataM$disruptive_f==T]))) ||
                  (sum(pred[dataM$disruptive_f==T])<=
                  ((100-minDecrementPercentualAffectation)/100*sum(predNoEff[dataM$disruptive_f==T]))))) {
          rmseR
        } else {Inf})
      })
      return(err[which.min(err)])
    }
  }
  # Evaluate multiple time periods in order to find the best one
  # fitting a model which considers the relationship between
  # cooling/heating and consumption
  
  params = list(
    "ini"=list(
       "datatype"="integer",
       "min"=0,
       "max"=as.numeric(maxIniDate - minIniDate),
       "nlevels"=as.numeric(maxIniDate - minIniDate)
    ),
    "end"=list(
       "datatype"="integer",
       "min"=0,
       "max"=as.numeric(maxEndDate - minEndDate),
       "nlevels"=as.numeric(maxEndDate - minEndDate)
    )
  )
  opt_function <- function(X, dataM, ...) {
    args <- list(...)
    
    minDate <- minIniDate + lubridate::days(X$ini) 
    maxDate <- minEndDate + lubridate::days(X$end)
    
    if (minDate > maxDate) return(Inf)
    value <- disruptive_period_model(
      dataM=dataM,
      minDate=minDate,
      maxDate=maxDate,
      args$checkFor,
      args$minIniDate,
      args$maxEndDate,
      args$minDecrementPercentualAffectation,
      args$minIncrementPercentualAffectation
    )
    if (!(is.finite(value))) {
      value <- Inf
    }

    # Naïve discrimination criteria based on duration
    days<- as.numeric(maxDate - minDate)
    value <- value + (days / 1000.0)
    return(value)
  }

  best_params <- hyperparameters_tuning(
    opt_criteria = "minimise",
    opt_function = opt_function,
    features = params,
    maxiter = 6,
    popSize = 24,
    dataM = data_daily,
    checkFor = checkFor,
    minIniDate = minIniDate,
    maxEndDate = maxEndDate,
    minDecrementPercentualAffectation = minDecrementPercentualAffectation,
    minIncrementPercentualAffectation = minIncrementPercentualAffectation
  )

  best_ini <- best_params$ini
  best_end <- best_params$end
  return(if(all(mapply(function(i)!is.finite(i),best_params))){
    data.frame("minDate"=NA,"maxDate"=NA)
  } else {
    data.frame( 
      "minDate"=(minIniDate + lubridate::days(best_ini)),
      "maxDate"=(minEndDate + lubridate::days(best_end)))
  })

}

#' Detect holidays in consumption time series
#' 
#' The function detects holiday periods in buildings with highly seasonal patterns.
#' An analysis of turnpoints in density function is performed to identify
#' "over represented" low consumption dates. Additional post-processing is done
#' in order to take care of additional constraints related to holidays period
#'
#' @param data <data.frame> Time series with potential anomalies in
#' values
#' @param consumptionColumn <string> Value column
#' @param timeColumn <string> Time column
#' @param plotDensity <boolean> Plot density function used to obtain
#'  holidays consumption threshold
#' @param ignoreDates <list> Ignore list of dates in time series.
#' @param tz <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). 
#' @return <array> of dates with classified as holidays.

detect_holidays_in_tertiary_buildings <- function(data, consumptionColumn, timeColumn, plotDensity=T, 
                                                  ignoreDates=c(), tz="UTC"){
  
  #data <- data[is.finite(data[,consumptionColumn]),]
  n_timesteps <- hourly_timesteps(24,detect_time_step(data))
  data <- aggregate(setNames(data.frame(data[,consumptionColumn]),consumptionColumn),
                    by=setNames(list(as.Date(data[,timeColumn], tz=tz)),timeColumn),
                    FUN=function(x)mean(x,na.rm=T)*n_timesteps)
  data$dayweek <- strftime(data[,timeColumn],"%u")
  data_ini <- data
  data <- data[!(data$time %in% ignoreDates),]
  
  # Estimate the cons_limit using the density function
  summarise_per_day <- data %>% group_by(dayweek) %>%
    summarise(across(consumptionColumn,list(~mean(.x,na.rm=T),
                                      ~quantile(.x,0.9,na.rm=T))))
  low_consumption_day <- min(summarise_per_day[,2])
  d <- density(data[!(data$dayweek %in% c("6","7")),consumptionColumn],na.rm=T)
  # calculate the local extremes
  tp<-pastecs::turnpoints(ts(d$y))
  # defines the local maximums and minimums
  importance <- d$y[tp$tppos]
  cons <- d$x[tp$tppos]
  shifted_importance <- c(0,dplyr::lag(importance,1)[is.finite(dplyr::lag(importance,1))])
  min_max <- ifelse(importance-shifted_importance>0,"max","min")
  
  ## Estimate the cons_limit based on the day with minimum consumption of the week 
  data_dayweek <-
    mapply(function(i){
      na.locf(zoo::rollapply(ifelse(data_ini$dayweek==i,data_ini[,consumptionColumn],NA),
                     width=30*3,partial=T,align="center",fill=c(NA,NA,NA),
                     FUN=function(x){max(x,na.rm=T)}))
    },sort(unique(data$dayweek)))
  cons_min <- matrixStats::rowMins(data_dayweek)
  cons_max <- matrixStats::rowMaxs(data_dayweek)
  cons_limit <- ifelse(((cons_max-cons_min)/cons_min) > 0.5, cons_min, 0)
   
  days_detected <- as.Date(data_ini[
    !(as.Date(data_ini[,timeColumn],tz=tz) %in% ignoreDates) &
      data_ini[,consumptionColumn] <= cons_limit & is.finite(data_ini[,consumptionColumn]), timeColumn], tz=tz)
  
  # Add common holidays in ignored days.
  if(length(ignoreDates)>0 && any(data_ini$time %in% ignoreDates)){
    data_ini[,"holidays"] <- ifelse(data_ini$time %in% days_detected, 1, 0)
    data_ini <- calendar_components(data_ini,localTimeZone = tz)
    days_detected <- c(
      days_detected,
      data_ini$time[data_ini$time %in% ignoreDates][
        ranger::predictions(
          predict(
            ranger::ranger( holidays~.,
              data = data_ini[!(data_ini$time %in% ignoreDates), 
                            c("holidays","month","dayYear","weekday")]),
              data_ini[(data_ini$time %in% ignoreDates), 
                       c("holidays","month","dayYear","weekday")]
        ))>0.5])
  }
  
  # plot the PDF and the local extremes
  if(plotDensity==T){
    plot(d, main="",xlab="Consumption [kWh]")
    points(d$x[tp$tppos],d$y[tp$tppos],col="red",cex=2)
    abline(v = cons_limit,col=3)
  }
  if(length(days_detected)==0){
    return(NULL)
  } else {
    return(unique(days_detected))
  }
}

get_holidays_NAGER <- function(year, country, region){
  res = GET("https://date.nager.at",
            path = list("api/v3/publicholidays", year, country))
  data = fromJSON(rawToChar(res$content))
  if (length(region) != 0){ # length of var is 0 if var is NULL, != 0 if var is defined
    data_region <- data %>% filter(data$counties %in% list(paste(year, region, sep="-"), NULL))
    for (el in 1:nrow(data))
      if (typeof(data$counties[el]) == "list" && paste(year, region, sep="-") %in% data$counties[[el]])
        data_region = rbind(data_region, data[el,])
  } else {
    data_region <- data
  }
  return(data_region)
}

holidaysNAGER <- function(y, country, region=NULL){
  df <- sapply(y, get_holidays_NAGER, country, region)
  dates <- as.Date(c("2000-03-02")) # Irene's birthday as culprit
  
  for (i in 1:length(y)){
    # i = 1
    for (ii in 1:length(df["date", i][[1]])){
      # ii = 6
      d <- as.Date(df[["date", i]][[ii]])
      dates <- append(dates, d)
    }
  }
  return(dates[-1])
}
