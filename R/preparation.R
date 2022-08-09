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

#' The function infers, i.e. automatically deduce from the input data, the
#' minimum time step (frequency) that can be used for the input time series.
#'
#' @param data <timeSeries> Input time series whose time step has to be detected.
#' @return timeStep <string> A string in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).

detect_time_step <- function(data){
  if (class(data)[1]=="POSIXct"){
    data <- data.frame("time"=data)
  }
  if(nrow(data)<=1) {return(NULL)}

  # Calculate iso8601 period between samples
  raw_timesteps <- difftime(sort(data$time),lag(sort(data$time),1),units="secs")
  iso_timesteps <- lubridate::format_ISO8601(as.period(raw_timesteps[is.finite(raw_timesteps)]))
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
#' @param timeStep <string> A string in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).
#' @return Amount of ISO 8601 timesteps corresponding to nHours

hourly_timesteps <- function(nHours, original_timestep) {
  timesteps_to_hour <- list(
    "PT1S" = 3600,
    "PT1M" = 60,
    "PT1H" = 1,
    "P1D" = 1/24,
    "P1W" = 1/168,
    "P1M" = 1/(30*24),
    "P1Y" = 1/(365*24)
    )
  return(round(timesteps_to_hour[[original_timestep]]*nHours))
}


#' Resample time serie to new ISO 8601 timestep
#'
#' Time serie is upsampled creating synthetic forward fill time serie
#' in case of higher frequency timestep required
#' Time serie is downsampled maintaing curent values but
#' repeated indexes in case of lower frequency timestep required
#'
#' @param data <timeSeries> Input time series to be resampled
#' @param timeStep <string> A string in ISO 8601 format representing the period
#' or timestep (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).
#' @return Resampled time serie

resample <- function(data, timestep) {
  # Resample (upsample) by creating synthetic comple serie
  data$time_ <- floor_date(data$time, iso8601_period_to_text(timestep),
    week_start = getOption("lubridate.week.start", 1)
  )
  start <- data$time_[1]
  end <- data$time[length(data$time)]
  timeseq <- seq(start, end, by = iso8601_period_to_text(timestep))

  # TEMPFIX: seq.Date not properly adding months
  if ((timestep == "P1M") & (day(start) == 31)) {
    mask <- day(timeseq) == 1
    timeseq[mask] <- timeseq[mask] - 60 * 60 * 24
  }

  # Create synthetic serie mixing timeserie and ffill values
  newdata <- data.frame(time_ = timeseq) %>%
    left_join(data, by = "time_") %>%
    fill(value, .direction = "down") %>%
    mutate(time = time_) %>%
    select(-time_)
  return(newdata)
}



#' Detect elements of the time series outside a minimum and maximum range.
#'
#' Additionally, with the minSeries and maxSeries arguments, this ranges can
#' be set differently along the period. When using this feature, the timestep
#' of both minSeries and maxSeries need to be resampled to the original
#' frequency of the data time series, applying forward fill if is needed
#' (Remember the time stamps of each time series element always represent the
#' begining of the time step).
#'
#' @param data <timeSeries> An argument containing the time series from which
#' the outliers need to be detected.
#' @param min: float The minimum value allowed for each element of the time
#' series.
#' @param max: float The maximum value allowed for each element of the time
#' series.
#' @param minSeries: timeSeries. An optional argument defining a time series
#' with minimum allowed values.
#' @param maxSeries: timeSeries. An optional argument defining a time series
#' with maximum allowed values.
#' @return outliers <boolean> timeSeries object using the original time period
#' and frequency, only assigning True values when an element should be
#' considered as an outlier.

detect_ts_min_max_outliers <- function(data, min, max, minSeries = NULL, maxSeries = NULL) {
  min_mask <- (data$value < min)
  max_mask <- (data$value > max)

  if (!is.null(minSeries)) {
    timestep <- detect_time_step(data)
    minSeries <- resample(minSeries, timestep)
    mask <- data %>%
      left_join(minSeries, by = "time") %>%
      mutate(outlier = value.x < value.y)
    min_mask <- mask[["outlier"]]
  }
  if (!is.null(maxSeries)) {
    timestep <- detect_time_step(data)
    maxSeries <- resample(maxSeries, timestep)
    mask <- data %>%
      left_join(maxSeries, by = "time") %>%
      mutate(outlier = value.x > value.y)
    max_mask <- mask[["outlier"]]
  }
  return(min_mask | max_mask)
}

#' Detect elements of the time series out of a Z-score threshold,
#' applied on the whole timeseries or a rolling window of predefined width.

#' @param data <timeSeries> An argument containing the time series from which
#' the outliers need to be detected.
#' @param zScoreThreshold <float> An argument defining the threshold of the
#' Z-score calculation.
#' @param window <string> A string in ISO 8601 format representing the window
#' (e.g. "7D","1D", "72H" ,...). This is an optional argument setting
#' the width of the rolling window where the Z-normalization calculation
#' is considered. This argument allows to adapt the outlier filtering depending
#' the dynamics of the signal itself. Default value is "NULL", thus no rolling
#' window is considered.
#' @param zScoreExtremesSensitive <boolean> An optional argument to define if the
#' aggregation function of the Zscore is the mean (true), or median(false).
#' The first one makes the Z-score sensitive to extreme values, and the
#' second no. Default is true.
#' @param na.rm <boolean> Remove NAN values in mean, medin and sd functions
#' @return outliers boolean timeSeries object using the original time period
#' and frequency, only assigning true values when an element should be
#' considered as an outlier.

detect_ts_zscore_outliers <- function(data, zScoreThreshold, window = NULL, zScoreExtremesSensitive = TRUE, na.rm=T) {
  func <- ifelse(zScoreExtremesSensitive == TRUE, mean, median)
  zScore <- ((data$value - func(data$value,na.rm=T)) / sd(data$value,na.rm=na.rm))
  if (!is.null(window)) {
    timestep <- detect_time_step(data)
    # timesteps_ <- invert(timesteps)
    secs <- as.integer(duration(window, units = "seconds"))
    width <- secs / as.integer(duration(timestep,units="secs"))
    zScore <- roll_scale(data$value, width = width, min_obs = 1)
    # TEMPFIX
    zScore[1] <- zScore[2]
  }
  return(abs(zScore) >= zScoreThreshold)
}

#' Obtain the components of the Fourier Series, in sine-cosine form
#'
#' It is useful for linearising the relationship of a seasonal input
#' time series (e.g. solar azimuth, solar elevation, calendar features, ...)
#' to some output (energy consumption, indoor temperatures, ...).
#' It basically decomposes a cyclic time series into a set of sine-cosine
#' components that are used as inputs for the modelling of some output,
#' each of the components linearly depends to the output.
#' @param X <timeSeries> timeSeries containing the series to transform.
#' This series must have a cyclic behaviour (e.g. hour of the day, day
#' of the week, solar azimuth, day of the year, ...) in order to be
#' correctly transformed. Optionally, other variables that are not
#' declared in featuresNames can be bypassed to the output..
#' @param featureNames <list> list of strings selecting the series
#' to transform
#' @param nHarmonics <integer> number of harmonics considered in
#' the Fourier Series. A high number allows to model more precisely
#' the relation, but it considerably increase the cost of computation.
#' The number of harmonics is related with the number of features in
#' the output matrix (2 * nHarmonics) + 1
#' @return <timeSeries> same initial information of data input argument,
#' plus the sine-cosine components of the Fourier Series as new columns

fs <- function(X, featureName, nHarmonics) {
  cbind(do.call(cbind,lapply(1:nHarmonics, function(i) {
    value <- list(sin(i * X * 2 * pi), cos(i * X * 2 * 
                                             pi))
    names(value) <- paste0(featureName, c("_sin_", "_cos_"), 
                           i)
    return(as.data.frame(value))
  })), setNames(data.frame(rep(1,length(X))),paste0(featureName,"_fs_int")))
}

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
#' difference substracted to the predition of the model. () Default is 30.
#' @param holidaysCalendar list of dates An optional list giving the dates
#' where local or national holidays related to the location of the data
#' argument. Default is empty list.
#' @param daysThatAreOutliers list of outlier dates
#' @param outputPredictors: boolean. Include calendar regression model prediction
#' results as output
#' @param logValueColumn: boolean. Transform value column (log-transformation)
#' @return timeSeries with time index outliers mask and optional calendar
#' regression model prediction
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

  # WARNING: First naive approach
  newdata <- data %>%
    mutate(
      H = (hour(time)) / 24,
      U = (as.numeric(strftime(time,"%u"))-1) / 7,
      W = as.numeric(strftime(time,"%U")) / 53,
      m = (as.numeric(strftime(time,"%m"))-1) / 12,#as.numeric(strftime(time,"%j")) / 366,
      HOL = as_date(time) %in% holidaysCalendar | strftime(time,"%u") %in% c("6","7"),
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
  
  # plot(prediction$upper,type="l",col="blue")
  # lines(prediction$lower,col="yellow")
  # lines(newdata$value,col="black")
  
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
#' @param data timeSeries Data time serie with potential profiled content 
#' @return list <date> List of dates with profiled data 

detect_profiled_data <- function(data, valueColumn="value", localTimeColumn="localtime") {
  if(period("PT1H")<=detect_time_step(data)){
    # Create wide data frame with aggregated hourly demand per
    # each of the days
    data_wide <- data %>%
      rename(Qe = !!valueColumn) %>%
      rename(localtime = !!localTimeColumn) %>%
      mutate(
        hourly_lt=as.POSIXct(format(localtime,"%Y-%m-%d %H"),
                              format="%Y-%m-%d %H",
                              tz = attr(localtime[1],"tzone")),
        date = lubridate::as_date(hourly_lt),
        hour = lubridate::hour(hourly_lt)
      ) %>%
      group_by(hourly_lt) %>%
      summarise(Qe=sum(Qe),date=first(date),hour=first(hour)) %>%
      pivot_wider(id_cols=date,names_from=hour,values_from = Qe,names_sort = T)
    # Find duplicated days having same daily pattern. Same
    # consumption in each hour of the day
    check_duplicated <- duplicated(apply(data_wide %>% select(-date),1,paste,collapse=""))
    date_duplicated <- data_wide$date[check_duplicated]
    if(length(date_duplicated)>0){
      # Detect consecutive dates that are profiled.
      date_duplicated <- sort(unique(
        c(date_duplicated[(date_duplicated - lag(date_duplicated,1))==1],
        date_duplicated[(date_duplicated - lag(date_duplicated,1))==1]-days(1))
      ))
      date_duplicated <- date_duplicated[is.finite(date_duplicated)]
    }
    return(date_duplicated)
  } else {
    warning("Profiled data can only be detected in hourly or quarterhourly time series")
    return(c())
  }
}

#' Detect elements of the time series out of a confidence threshold based
#' on linear model of the calendar variables (month, weekday, hour).
#'
#' @param data timeSeries An argument containing the time series from which
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
#' window (e.g. "2m","4m","14D",...). This is an optional argument setting
#' the width of the window where the model is trained and evaluated
#' @param outputPredictor: boolean. Include calendar regression model prediction
#' results as output
#' @param logValueColumn: boolean. Transform value column (log-transformation)
#' @param autoDetectProfiled: boolean. Detect and ignore profiled days from
#' timeserie
#' @return timeSeries with time index outliers mask and optional calendar
#' regression model prediction
detect_ts_calendar_model_outliers <- function(data,
                                              localTimeColumn="localtime",
                                              valueColumn=outputName,
                                              calendarFeatures = c("HOL", "H"),
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
    daysThatAreOutliers <- c(daysThatAreOutliers, detect_profiled_data(data, valueColumn, localTimeColumn))
  if(class(window)=="numeric"){
    start <- as.integer(seconds(data$time[1]))
    # Windowize data in order to obtain outliers in data samples
    windowsize <- as.integer(duration(window, units = "seconds"))
    data <- data %>%
      mutate(
        window = (as.integer(seconds(time)) - start) %/% windowsize
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
        data=if(class(window)=="numeric"){
          newdata[newdata$window %in% c(w-1,w,w+1),]
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

#' Detect which numerical elements are outside the min-max range.
#' @param data <float/integer> list The numerical elements to be evaluated
#' @param min <float/integer> The minimum value of the range.
#' @param max <float/integer> The maximum value of the range.
#' @param includedMin <boolean> An optional argument setting if the
#'  minimum value should be included as valid value (true), or not (false).
#'  Default is true.
#' @param includedMax <boolean> An optional argument setting if the maximum
#'  value should be included as valid value (true), or not (false).
#'  Default is true.
#' @return Mask of timeserie entries matching criteria

detect_static_min_max_outliers <- function(data, min, max, includeMin = TRUE, includeMax = TRUE) {
  min_mask <- (data < min)
  max_mask <- (data > max)
  if (includeMin == FALSE) min_mask <- (data <= min)
  if (includeMax == FALSE) max_mask <- (data >= max)
  return(min_mask | max_mask)
}

#' Detect which string element satisfy the regular expression
#'
#' @param data <string> list The elements to be evaluated.
#' @param regExpValues <string list> A list of regular expressions to be
#' checked.
#' @param negativeExp <boolean> An optional argument to evaluate the inverse
#' result of the regular expressions. Default is false, thus all elements
#' that are satified by any of the regular expressions are true.
#' @return outliers <boolean list> considering if each element of the data
#' list satisfy any of the regular expression, or the contrary
#' (if negativeExp is true).
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
#' @param data <timeSeries> with Not Available elements to be filled.
#' @param outliersMinMax detect_ts_min_max_outliers() output
#' @param outliersZScore detect_ts_zscore_outliers() output
#' @param outliersCalendarModel detect_ts_calendar_model_outliers() output
#' @param methodFillNA <string> argument specifying the methodology for
#' filling the NA elements. Possible values are:
#'    calendarModel: The predicted values estimated by the calendar
#' model are used to fulfill the NA elements.
#'    backward: The previous known element of the timeseries is
#' considered.
#'    forward: The next known element of the timeseries is considered.
#'    linearInterpolation: A linear interpolation is done between
#' using previous and next known elements regarding each data gap.
#' @param maxGap: string in ISO 8601 format representing the
#' window (e.g. "4H", "30M", "72H", "2D",...). It defines the maximum
#' period allowed. Therefore, gaps with greater period are not considered
#' in the imputation. By default, it doesn't exists a limitation of the
#' gap longitude.
#' @param fillMask: boolean timeSeries defining the time periods where the
#' imputation can be done. By default, all elements of the timeseriesxi
#' can be filled.
#' @return filledData timeSeries with filled elements.

fill_ts_na <- function(data, outliersMinMax, outliersZScore, outliersCalendarModel, methodFillNA = "linearInterpolation", maxGap = NULL, fillMask = NULL) {
  # NOTE: outliersMinMax and outliersZScore not used

  newdata <- data
  if (!is.null(fillMask)) mask <- mask & fillMask
  if (!is.null(maxGap)) {
    missing <- data %>%
      drop_na(.) %>%
      mutate(
        time_lag1 = lag(time, 1),
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
#' @param x <value|array> Value to be rounded
#' @return Rounded value
round_up <- function(x) 10^ceiling(log10(x))

#' The function converts a cumulative (counter) or onChange (delta) measurement
#' to instantaneous.
#'
#' @param data <timeSeries> The cumulative or on-change time series that has to
#' be converted to instantaneous. An instantaneous measurement is a gauge
#' metric, in which the value can increase or decrease and measures a specific
#' instant in time.
#' @param measurementReadingType <enumeration> An argument used to define the
#' measurement reading type. Supported readiny types are:
#'     - onChange. Delta reading type
#'     - cumulative. Counter reading type
#' @return data <timeSeries> The cumulative or onChange time series with
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

#' The function aligns the frequency of the input time series with the output
#' frequency given as an argument using the specified aggregation function.
#'
#' @param data <timeSeries> The time series that has to be aligned with an
#' output time step, i.e. with a specific frequency. If the
#' measurementReadingType of the series is not instantaneous, the data must
#' be converted first using the function clean_ts_integrate.
#' @param timeColumn <string> Time column in data time serie
#' @param valueColumn <string> Value column in data time serie
#' @param isRealColumn <string> isReal boolean column in data time serie
#' describing estimated value 
#' @param outputFrequency <string> The frequency used to resample the input
#' time series for the alignment. It must be a string in ISO 8601 format
#' representing the time step (e.g. "PT15M","PT1H", "P1M", "P1D",...).
#' @param aggregationFunctions <list The aggregation functions to use when
#'  resampling the series. Supported aggregation functions
#'     - AVG
#'     - SUM
#'     - MIN
#'     - MAX
#'     - HDD. Heating degree days. Base temperature 20, 21, 22
#'     - CDD. Cooling degree days. Base temperature 20, 21, 22
#' @param useEstimatedValued <boolean> Do not ignore estimated values in input
#' time serie 
#' @return data <timeSeries> The time series resampled with the specified
#' period and aggregation function.

align_time_grid <- function(data, 
                            timeColumn="time", valueColumn="value",
                            isRealColumn="isReal", outputFrequency, 
                            aggregationFunctions=c("SUM","AVG","MIN","MAX"), 
                            useEstimatedValues=F, tz="UTC") {
  
  data <- data %>% rename(
    time = timeColumn,
    value = valueColumn,
    isReal = isRealColumn
  )
  
  data$time <- lubridate::with_tz(data$time,tz)

  # Remove estimated values from time serie  
  if(useEstimatedValues==F && ("isReal" %in% colnames(data))){
    dataWithoutEstimates <- data %>% 
      filter(isReal==T)
    if(nrow(dataWithoutEstimates)==0){
      stop("The whole timeseries contains estimated values, 
           and they are not allowed during the alignment process. 
           Consider useEstimatedValues=T if you allow their usage.")
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
  
  return(results)
}

#' The function detects periods of time with anomalous low consumption
#' Evaluates multiple time periods in order to find the best one
#' fitting a model which considers the relationship between
#' cooling/heating and consumption
#'
#' @param data <timeSeries> Time serie with potential anomalies in
#' values
#' @param consumptionValue <string> Consumption column in data time serie
#' @param timeColumn <string> Time column in data time serie
#' @param temperatureColumn <string> Temperature column in data time serie
#' @param tz <string> Timezone
#' @param minIniDate <date> Minimum date to start looking for anomalies
#' @param maxIniDate <date> Maximum date to start looking for anomalies
#' @param minEndDate <date> Minimum date to stop looking for anomalies
#' @param maxEndDate <date> Maximum date to stop looking for anomalies
#' @return Period of time <start,end> with anomalous low consumption. Period
#' of time is within the InitDate and EndDate limits

detect_disruptive_period <- function(data, consumptionColumn, timeColumn, 
                                     temperatureColumn, tz, 
                                     minIniDate, maxIniDate,
                                     minEndDate, maxEndDate){
  
  # In case of daily or greater frequency
  if(lubridate::as.period(detect_time_step(data)) >= lubridate::as.period("P1D")){
    data_monthly <- data %>% 
      group_by("time" = lubridate::floor_date(lubridate::with_tz(time,tz),
                                              unit = lubridate::as.period("P1M"),
                                              week_start = getOption("lubridate.week.start", 1))) %>%
      summarise(
        across(c(consumptionColumn,temperatureColumn), sum,
               .names = c("consumption",temperatureColumn))
      )
  # In case of hourly or quarterhourly data
  } else {
    data_consumption <- data %>% 
      group_by("time" = lubridate::floor_date(lubridate::with_tz(time,tz),
                                              unit = lubridate::as.period("P1M"),
                                              week_start = getOption("lubridate.week.start", 1))) %>%
      summarise(
        across(consumptionColumn, sum,.names = "consumption")
      )
    data_hdd <- degree_days(
      data = data,
      temperatureFeature = temperatureColumn,
      localTimeZone = tz,
      baseTemperature = seq(5,20,by=3),
      mode = "heating",
      outputFrequency = "P1M",
      outputFeaturesName = "HDD")
    data_cdd <- degree_days(
      data = data,
      temperatureFeature = temperatureColumn,
      localTimeZone = tz,
      baseTemperature = seq(13,28,by=3),
      mode = "cooling",
      outputFrequency = "P1M",
      outputFeaturesName = "CDD")
    data_monthly <- data_consumption %>% left_join(data_hdd,"time") %>% left_join(data_cdd,"time")
    data_monthly <- data_monthly[complete.cases(data_monthly),]
  }
  disruptive_period_model <- function(dataM, minDate, maxDate){
    data_d <- data.frame(
      "time" = seq.Date(as.Date(min(dataM$time)), 
                        as.Date(max(dataM$time) + months(1) - days(1)), 
                        by="days")
    )
    data_d$disruptive <- ifelse(data_d$time >= minDate & data_d$time <= maxDate,
                                1,0)
    dataMD <- data_d %>% 
      group_by(
        "time" = floor_date(time,
          unit = lubridate::as.period("P1M"),
          week_start = getOption("lubridate.week.start", 1))
      ) %>% 
      summarise("disruptive" = sum(disruptive),
                "disruptive_f" = as.factor(sum(disruptive)>0))
    dataM <- dataM %>% left_join(dataMD)
    cases <- expand.grid(HDD=colnames(dataM)[grepl("^HDD",colnames(dataM))],
                CDD=colnames(dataM)[grepl("^CDD",colnames(dataM))])
    err <- mapply(1:nrow(cases), FUN = function(i){
      dataM$HDD <- unlist(dataM[cases$HDD[i]])
      dataM$CDD <- unlist(dataM[cases$CDD[i]])
      mod <- penalized(response = dataM$consumption, 
                penalized = ~ HDD:disruptive_f + CDD:disruptive_f,
                unpenalized = ~ 0 + disruptive_f + disruptive,
                data = dataM,positive=T,trace = F)
      RMSE(mod@fitted,dataM$consumption)
    })
    err[which.min(err)]
  }
  cases_dates <- expand.grid(
    minDate=seq.Date(minIniDate, maxIniDate, by = "14 days"),
    maxDate=seq.Date(minEndDate, maxEndDate, by="14 days")
  )
  # Evaluate multiple time periods in order to find the best one
  # fitting a model which considers the relationship between
  # cooling/heating and consumption
  results <- mapply(1:nrow(cases_dates),
    FUN = function(k){
      disruptive_period_model(dataM = data_monthly,minDate = cases_dates$minDate[k], 
                              maxDate=cases_dates$maxDate[k])
    }
  )
  return(cases_dates[which.min(results),])
}

#' The function detects holidays period in tertiary building time serie.
#' Analysis of turnpoints in density distribution in order to identify
#' "over represented" low consumption dates. Additional postprocessing is done
#' in order to take care of additional constraints related to holidays period
#'
#' @param data <timeSeries> Time serie with potential anomalies in
#' values
#' @param valueColumn <string> Value column
#' @param timeColumn <string> Time column
#' @param plotDensiry <boolean> Plot density function used to obtain
#'  holidays consumption threshold
#' @param ignoreDates <list> Ignore list of dates in time serie
#' @return List of dates with classified as holidays

detect_holidays_in_tertiary_buildings <- function(data, valueColumn, timeColumn, plotDensity=T, 
                                                  ignoreDates=c(), tz="UTC"){
  
  data <- data[is.finite(data[,valueColumn]),]
  data <- aggregate(setNames(data.frame(data[,valueColumn]),valueColumn),
                    by=setNames(list(as.Date(data[,timeColumn], tz=tz)),timeColumn),
                    FUN=function(x)sum(x,na.rm=T))
  data$dayweek <- strftime(data[,timeColumn],"%u")
  
  # Estimate the cons_limit using the density function
  summarise_per_day <- data %>% group_by(dayweek) %>%
    summarise(across(valueColumn,list(~mean(.x,na.rm=T),
                                      ~quantile(.x,0.9,na.rm=T))))
  low_consumption_day <- min(summarise_per_day[,2])
  d <- density(data[!(data$dayweek %in% c("6","7")),valueColumn],na.rm=T)
  # calculate the local extremes
  tp<-pastecs::turnpoints(ts(d$y))
  # defines the local maximums and minimums
  importance <- d$y[tp$tppos]
  cons <- d$x[tp$tppos]
  shifted_importance <- c(0,shift(importance,1)[is.finite(shift(importance,1))])
  min_max <- ifelse(importance-shifted_importance>0,"max","min")
  # cons_limit = min(cons[cons >= low_consumption_day & min_max == "min"])
  # cons_i <- which(cons %in% cons_limit)
  # cons_limit <- mean(cons[(cons_i-1):cons_i],na.rm=T)
  
  ## Estimate the cons_limit based on the day with minimum consumption of the week 
  data_dayweek <-
    mapply(function(i){
      na.locf(zoo::rollapply(ifelse(data$dayweek==i,data[,valueColumn],NA),
                     width=30*3,partial=T,align="center",fill=c(NA,NA,NA),
                     FUN=function(x){max(x,na.rm=T)}))
    },sort(unique(data$dayweek)))
  cons_limit <- matrixStats::rowQuantiles(data_dayweek,probs = 0.01)
  #cons_limit <- matrixStats::rowMins(data_dayweek)
  
  days_detected <- as.Date(data[
    !(as.Date(data[,timeColumn],tz=tz) %in% ignoreDates) &
      data[,valueColumn] <= cons_limit & is.finite(data[,valueColumn]), timeColumn], tz=tz)
  
  # Estimate the cons_limit based on the minimum standard deviation
  # data_dayweek <-
  #   mapply(function(i){
  #     zoo::rollapply(ifelse(data$dayweek==i,data[,valueColumn],NA),
  #                    width=60,partial=T,align="center",fill=c(NA,NA,NA),
  #                    FUN=function(x){sd(x,na.rm=T)})
  #   },sort(unique(data$dayweek)))
  # cons_limit <- matrixStats::rowMins(data_dayweek)
  # 
  # days_detected <- as.Date(data[data[,valueColumn] <= cons_limit & is.finite(data[,valueColumn]), timeColumn], tz=tz)
  
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
