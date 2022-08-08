# timesteps <- list(
#   "1" = "PT1S",
#   "60" = "PT1M",
#   "3600" = "PT1H",
#   "86400" = "P1D",
#   "604800" = "P1W",
#   "2419200" = "P1M",
#   "2505600" = "P1M",
#   "2592000" = "P1M",
#   "2678400" = "P1M",
#   "31536000" = "P1Y",
#   "31622400" = "P1Y"
# )

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

# seqsteps <- list(
#   "PT1S" = "sec",
#   "PT1M" = "min",
#   "PT1H" = "hour",
#   "P1D" = "day",
#   "P1W" = "week",
#   "P1M" = "month",
#   "P1Y" = "year"
# )
# 
# roundsteps <- list(
#   "PT1S" = "second",
#   "PT1M" = "minute",
#   "PT1H" = "hour",
#   "P1D" = "day",
#   "P1W" = "week",
#   "P1M" = "month",
#   "P1Y" = "year"
# )

# approx_timesteps <- function(x) {
#   # NOTE: Tricky fix to manage non-exact scenarios. Find closest timestep
#   # Review close interval per each of the timesteps
#   approx <- list(
#     "PT1S" = c(0, 0), # 0s
#     "PT1M" = c(-2, 2), # 2s
#     "PT1H" = c(-2 * 60, -2 * 60), # -+2min
#     "P1D" = c(-1 * 60 * 60, 1 * 60 * 60), # -+1hour
#     "P1W" = c(-1 * 60 * 60, 1 * 60 * 60), # -+1hour
#     "P1M" = c(-1 * 24 * 60 * 60, 1 * 24 * 60 * 60), # -+1day
#     "P1Y" = c(-1 * 24 * 60 * 60, 1 * 24 * 60 * 60) # -+1day
#   )
#   for (name in names(x))
#   {
#     step <- timesteps[[name]]
#     min_secs <- approx[[step]][1]
#     max_secs <- approx[[step]][2]
#     secs <- as.integer(name) + seq(min_secs, max_secs, 1)
#     steps <- rep(step, length(secs))
#     names(steps) <- secs
#     x <- append(x, as.list(steps))
#   }
#   return(x)
# }

# invert <- function(x) {
#   v <- as.integer(names(x))
#   names(v) <- as.character(x)
#   return(v)
# }

#' The function infers, i.e. automatically deduce from the input data, the
#' minimum time step (frequency) that can be used for the input time series.
#'
#' @param data <timeSeries> Input time series whose time step has to be detected.
#' @param maxMissingTimeSteps <int> Optional tolerance threshold specifying the
#' maximum tolerable percentage of missing values on the total length of the time
#' series.
#' @return timeStep <string> A string in ISO 8601 format representing the period or timestep
#' (e.g. "PT15M","PT1H", "P3M", "P1D" ,...).

detect_time_step <- function(data){#, maxMissingTimeSteps = 0, approxTimeSteps = FALSE) {
  #if (approxTimeSteps == TRUE) timesteps <- approx_timesteps(timesteps)
  if (class(data)[1]=="POSIXct"){
    data <- data.frame("time"=data)
  }
  if(nrow(data)<=1) {return(NULL)}
  raw_timesteps <- difftime(sort(data$time),lag(sort(data$time),1),units="secs")
  iso_timesteps <- lubridate::format_ISO8601(as.period(raw_timesteps[is.finite(raw_timesteps)]))
  iso_period <- names(sort(table(iso_timesteps),T))[1]
  
  roundISOPeriods_df <- do.call(rbind,lapply(1:length(roundISOPeriods),function(x){
    data.frame("rounded" = names(roundISOPeriods)[x],
               "original" = roundISOPeriods[[x]])}))
  if(iso_period %in% roundISOPeriods_df$original){
    iso_period <- roundISOPeriods_df$rounded[
      mapply(function(x)grepl(paste0("^",x),iso_period),roundISOPeriods_df$original)]
  }
  
  return(iso_period)
  # # Use contingency table to identify
  # # most common frequency
  # crosstab <- data %>%
  #   mutate(
  #     time_lag1 = lag(time, 1),
  #     secs = as.integer(
  #       difftime(time, time_lag1, units = "secs")
  #     )
  #   ) %>%
  #   group_by(secs) %>%
  #   summarize(freq = n()) %>%
  #   arrange(
  #     ifelse(maxMissingTimeSteps > 0, desc(freq), secs)
  #   ) %>% filter(secs>=0)
  # 
  # secs <- as.character(crosstab$secs[1])
  # # WARNING: Expecting significative amount of samples in
  # # standard frequency (seconds interval) otherwise fails
  # if (!secs %in% names(timesteps)) {
  #   return(NULL)
  # }
  # timestep <- as.character(timesteps[secs])
  # 
  # # Identify missing entries and weight missing data
  # if (maxMissingTimeSteps > 0) {
  #   # Create synthetic timeserie with no missing point in order to
  #   # compare it against the one provided.
  #   timesteps_ <- unique(
  #     as.character(
  #       timesteps[as.character(sort(as.integer(names(timesteps))))]
  #     )
  #   )
  #   timesteps_index <- min(which(timesteps_ == timestep))
  #   timesteps_ <- tail(
  #     timesteps_[seq(timesteps_index, timesteps_index + 3)]
  #   )
  #   failed <- TRUE
  # 
  #   # Iterate over all frequencies lower than the highest frequency
  #   # one previously identified in order to check the one fulfilling
  #   # cover requirements
  #   for (timestep_ in timesteps_)
  #   {
  #     # WARNING: Using first timestamp as reference
  #     # in order to identify periodicity
  #     start <- floor_date(data$time[1], roundsteps[timestep_],
  #       week_start = getOption("lubridate.week.start", 1)
  #     )
  #     end <- floor_date(data$time[length(data$time)], roundsteps[timestep_],
  #       week_start = getOption("lubridate.week.start", 1)
  #     )
  #     if (timestep_ %in% c("P1M", "P1Y")) {
  #       start <- data$time[1]
  #       end <- data$time[length(data$time)]
  #     }
  #     timeseq <- seq(start, end, by = as.character(seqsteps[timestep_]))
  #     # TEMPFIX: seq.Date not properly adding months
  #     if ((timestep_ == "P1M") & (day(start) == 31)) {
  #       mask <- day(timeseq) == 1
  #       timeseq[mask] <- timeseq[mask] - 60 * 60 * 24
  #     }
  # 
  #     # Obtain number of missing comparing against synthetic
  #     # complete serie
  #     newdata <- data.frame(time = timeseq)
  #     n_total <- dim(newdata)[1]
  #     n_missing <- as.integer(
  #       newdata %>%
  #         left_join(data, by = "time") %>%
  #         select(value) %>%
  #         summarise(sum(is.na(.)))
  #     )
  #     ratio_missing <- n_missing / n_total
  #     if (ratio_missing < maxMissingTimeSteps) {
  #       timestep <- timestep_
  #       failed <- FALSE
  #       break
  #     }
  #   }
  #   if (failed == TRUE) {
  #     return(NULL)
  #   }
  # }
  # 
  # # Specific year handler in order to discriminate between
  # # start of the year or end of the year
  # if (timestep == "P1M") {
  #   tmp <- data %>%
  #     mutate(
  #       totaldays = as.integer(days_in_month(time)),
  #       days = mday(time),
  #       daystoend = totaldays - days,
  #       daysfromstart = day(time)
  #     )
  #   timestep <- ifelse(
  #     all(tmp$daystoend == 0), "P1M",
  #     ifelse(
  #       all(tmp$daysfromstart == 1), "MS", NULL
  #     )
  #   )
  # } else if (timestep == "Y") {
  #   # Specific year handler in order to discriminate between
  #   # start of the year or end of the year
  #   sof_mask <- (
  #     month(data$time) == 1 &
  #       day(data$time) == 1 &
  #       hour(data$time) == 0
  #   )
  #   eof_mask <- (
  #     month(data$time) == 12 &
  #       day(data$time) == 31 &
  #       hour(data$time) == 23
  #   )
  #   timestep <- ifelse(all(eof_mask), "Y",
  #     ifelse(all(sof_mask), "YS", NULL)
  #   )
  # }
  # return(timestep)
}

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

fs <- function(X, featureName, nHarmonics) {
  cbind(do.call(cbind,lapply(1:nHarmonics, function(i) {
    value <- list(sin(i * X * 2 * pi), cos(i * X * 2 * 
                                             pi))
    names(value) <- paste0(featureName, c("_sin_", "_cos_"), 
                           i)
    return(as.data.frame(value))
  })), setNames(data.frame(rep(1,length(X))),paste0(featureName,"_fs_int")))
}

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
  calendarFeatures <- ifelse(calendarFeatures %in% cols_to_fs, 
                             paste0("as.matrix(fs(", calendarFeatures, ",featureName=\"", 
                                    calendarFeatures,"\",nHarmonics=3))"), calendarFeatures)
  formula <- as.formula(paste("value", paste0("0 +", paste(calendarFeatures, collapse = ":")),
    sep = "~"
  ))
  newdata$value <- ifelse(is.finite(newdata$value),newdata$value,NA)
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

detect_profiled_data <- function(data){
  if(period("PT1H")<=detect_time_step(data)){
    data_wide <- data %>% 
      mutate(
        "hourly_lt"=as.POSIXct(format(localtime,"%Y-%m-%d %H"),
                               format="%Y-%m-%d %H",
                               tz = attr(localtime[1],"tzone"))
      ) %>%
      group_by(hourly_lt) %>%
      summarise(Qe=sum(Qe),date=first(date),hour=first(hour)) %>%
      pivot_wider(id_cols=date,names_from=hour,values_from = Qe,names_sort = T)
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
#' @param window string. A string in ISO 8601 format representing the
#' window (e.g. "2m","4m","14D",...). This is an optional argument setting
#' the width of the window where the model is trained and evaluated
#' @return outliers boolean timeSeries object using the original time period
#' and frequency, only assigning true values when an element should be
#' considered as an outlier.
#' @return predicted timeSeries of the predicted values of the original
#' timeSeries based on the calendar regression model.
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
    daysThatAreOutliers <- c(daysThatAreOutliers, detect_profiled_data(data))
  if(class(window)=="numeric"){
    start <- as.integer(seconds(data$time[1]))
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
#' @param <includedMax> boolean An optional argument setting if the maximum
#'  value should be included as valid value (true), or not (false).
#'  Default is true.

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

round_up <- function(x) 10^ceiling(log10(x))

#' The function converts a cumulative (counter) or onChange (delta) measurement
#' to instantaneous.
#'
#' @param data <timeSeries> The cumulative or on-change time series that has to
#' be converted to instantaneous. An instantaneous measurement is a gauge
#' metric, in which the value can increase or decrease and measures a specific
#' instant in time.
#' @param measurementReadingType <enumeration> An argument used to define the
#' measurement reading type
#'
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
#' @param outputTimeStep <string> The frequency used to resample the input
#' time series for the alignment. It must be a string in ISO 8601 format
#' representing the time step (e.g. "PT15M","PT1H", "P1M", "P1D",...).
#' @param aggregationFunction <string> The aggregation function to use when
#  resampling the series (AVG, SUM, MIN, MAX)
#'
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
  if(lubridate::period(detect_time_step(data)) >= lubridate::period(outputFrequency) ){
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
  results <- mapply(1:nrow(cases_dates),
    FUN = function(k){
      disruptive_period_model(dataM = data_monthly,minDate = cases_dates$minDate[k], 
                              maxDate=cases_dates$maxDate[k])
    }
  )
  return(cases_dates[which.min(results),])
}

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
