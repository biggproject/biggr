timesteps <- list(
  "1" = "S",
  "60" = "T",
  "3600" = "H",
  "86400" = "D",
  "604800" = "W",
  "2419200" = "M",
  "2505600" = "M",
  "2592000" = "M",
  "2678400" = "M",
  "31536000" = "Y",
  "31622400" = "Y"
)

seqsteps <- list(
  "S" = "sec",
  "T" = "min",
  "H" = "hour",
  "D" = "day",
  "W" = "week",
  "M" = "month",
  "MS" = "month",
  "Y" = "year",
  "YS" = "year"
)

roundsteps <- list(
  "S" = "second",
  "T" = "minute",
  "H" = "hour",
  "D" = "day",
  "W" = "week",
  "M" = "month",
  "MS" = "month",
  "Y" = "year",
  "YS" = "year"
)

approx_timesteps <- function(x) {
  # NOTE: Tricky fix to manage non-exact scenarios. Find closest timestep
  # Review close interval per each of the timesteps
  approx <- list(
    "S" = c(0, 0), # 0s
    "T" = c(-2, 2), # 2s
    "H" = c(-2 * 60, -2 * 60), # -+2min
    "D" = c(-1 * 60 * 60, 1 * 60 * 60), # -+1hour
    "W" = c(-1 * 60 * 60, 1 * 60 * 60), # -+1hour
    "M" = c(-1 * 24 * 60 * 60, 1 * 24 * 60 * 60), # -+1day
    "Y" = c(-1 * 24 * 60 * 60, 1 * 24 * 60 * 60) # -+1day
  )
  for (name in names(x))
  {
    step <- timesteps[[name]]
    min_secs <- approx[[step]][1]
    max_secs <- approx[[step]][2]
    secs <- as.integer(name) + seq(min_secs, max_secs, 1)
    steps <- rep(step, length(secs))
    names(steps) <- secs
    x <- append(x, as.list(steps))
  }
  return(x)
}

invert <- function(x) {
  v <- as.integer(names(x))
  names(v) <- as.character(x)
  return(v)
}

#' The function infers, i.e. automatically deduce from the input data, the
#' minimum time step (frequency) that can be used for the input time series.
#'
#' @param data <timeSeries> Input time series whose time step has to be detected.
#' @param maxMissingTimeSteps<int> Optional tolerance threshold specifying the
#' maximum tolerable percentage of missing values on the total length of the time
#' series.
#' @return timeStep <string> A string in ISO 8601 format representing the time
#' step (e.g. "15T","1H", "3M", "1D" ,...). If no frequency can be detected,
#' the function will return None.
detect_time_step <- function(data, maxMissingTimeSteps = 0, approxTimeSteps = FALSE) {
  if (approxTimeSteps == TRUE) timesteps <- approx_timesteps(timesteps)
  if (class(data)[1]=="POSIXct"){
    data <- data.frame("time"=data)
  }
  # Use contingency table to identify
  # most common frequency
  crosstab <- data %>%
    mutate(
      time_lag1 = lag(time, 1),
      secs = as.integer(
        difftime(time, time_lag1, units = "secs")
      )
    ) %>%
    group_by(secs) %>%
    summarize(freq = n()) %>%
    arrange(
      ifelse(maxMissingTimeSteps > 0, desc(freq), secs)
    ) %>% filter(secs>=0)
  
  secs <- as.character(crosstab$secs[1])
  # WARNING: Expecting significative amount of samples in
  # standard frequency (seconds interval) otherwise fails
  if (!secs %in% names(timesteps)) {
    return(NULL)
  }
  timestep <- as.character(timesteps[secs])

  # Identify missing entries and weight missing data
  if (maxMissingTimeSteps > 0) {
    # Create synthetic timeserie with no missing point in order to
    # compare it against the one provided.
    timesteps_ <- unique(
      as.character(
        timesteps[as.character(sort(as.integer(names(timesteps))))]
      )
    )
    timesteps_index <- min(which(timesteps_ == timestep))
    timesteps_ <- tail(
      timesteps_[seq(timesteps_index, timesteps_index + 3)]
    )
    failed <- TRUE

    # Iterate over all frequencies lower than the highest frequency
    # one previously identified in order to check the one fulfilling
    # cover requirements
    for (timestep_ in timesteps_)
    {
      # WARNING: Using first timestamp as reference
      # in order to identify periodicity
      start <- floor_date(data$time[1], roundsteps[timestep_],
        week_start = getOption("lubridate.week.start", 1)
      )
      end <- floor_date(data$time[length(data$time)], roundsteps[timestep_],
        week_start = getOption("lubridate.week.start", 1)
      )
      if (timestep_ %in% c("M", "Y")) {
        start <- data$time[1]
        end <- data$time[length(data$time)]
      }
      timeseq <- seq(start, end, by = as.character(seqsteps[timestep_]))
      # TEMPFIX: seq.Date not properly adding months
      if ((timestep_ == "M") & (day(start) == 31)) {
        mask <- day(timeseq) == 1
        timeseq[mask] <- timeseq[mask] - 60 * 60 * 24
      }

      # Obtain number of missing comparing against synthetic
      # complete serie
      newdata <- data.frame(time = timeseq)
      n_total <- dim(newdata)[1]
      n_missing <- as.integer(
        newdata %>%
          left_join(data, by = "time") %>%
          select(value) %>%
          summarise(sum(is.na(.)))
      )
      ratio_missing <- n_missing / n_total
      if (ratio_missing < maxMissingTimeSteps) {
        timestep <- timestep_
        failed <- FALSE
        break
      }
    }
    if (failed == TRUE) {
      return(NULL)
    }
  }

  # Specific year handler in order to discriminate between
  # start of the year or end of the year
  if (timestep == "M") {
    tmp <- data %>%
      mutate(
        totaldays = as.integer(days_in_month(time)),
        days = mday(time),
        daystoend = totaldays - days,
        daysfromstart = day(time)
      )
    timestep <- ifelse(
      all(tmp$daystoend == 0), "M",
      ifelse(
        all(tmp$daysfromstart == 1), "MS", NULL
      )
    )
  } else if (timestep == "Y") {
    # Specific year handler in order to discriminate between
    # start of the year or end of the year
    sof_mask <- (
      month(data$time) == 1 &
        day(data$time) == 1 &
        hour(data$time) == 0
    )
    eof_mask <- (
      month(data$time) == 12 &
        day(data$time) == 31 &
        hour(data$time) == 23
    )
    timestep <- ifelse(all(eof_mask), "Y",
      ifelse(all(sof_mask), "YS", NULL)
    )
  }
  return(timestep)
}

hourly_timesteps <- function(nHours, original_timestep) {
  timesteps_to_hour <- list(
    "S" = 3600,
    "T" = 60,
    "H" = 1,
    "D" = 1/24,
    "W" = 1/168,
    "M" = 1/(30*24),
    "MS" = 1/(30*24),
    "Y" = 1/(365*24),
    "YS" = 1/(365*24)
  )
  return(ceiling(timesteps_to_hour[[original_timestep]]*nHours))
}

resample <- function(data, timestep) {
  # Resample (upsample) by creating synthetic comple serie
  data$time_ <- floor_date(data$time, roundsteps[timestep],
    week_start = getOption("lubridate.week.start", 1)
  )
  start <- data$time_[1]
  end <- data$time[length(data$time)]
  timeseq <- seq(start, end, by = as.character(seqsteps[timestep]))

  # TEMPFIX: seq.Date not properly adding months
  if ((timestep == "M") & (day(start) == 31)) {
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
    timesteps_ <- invert(timesteps)
    secs <- as.integer(duration(window, units = "seconds"))
    width <- secs / as.integer(timesteps_[timestep])
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
  if (logValueColumn) newdata$value <- log(newdata$value)
  cols_to_fs <- c("H", "U", "W", "m")
  calendarFeatures <- ifelse(calendarFeatures %in% cols_to_fs, 
                             paste0("as.matrix(fs(", calendarFeatures, ",featureName=\"", 
                                    calendarFeatures,"\",nHarmonics=3))"), calendarFeatures)
  formula <- as.formula(paste("value", paste0("0 +", paste(calendarFeatures, collapse = ":")),
    sep = "~"
  ))
  newdata$value <- ifelse(is.finite(newdata$value),newdata$value,NA)
  model <- rq(
    formula,
    tau = c(lowerModelPercentile / 100.0, upperModelPercentile / 100.0),
    data = newdata[complete.cases(newdata),]
  )
  prediction <- as.data.frame(predict.rq(model, newdata))
  names(prediction) <- c("lower", "upper")
  if (logValueColumn) {
    prediction$lower <- exp(prediction$lower)
    prediction$upper <- exp(prediction$upper)
  }
  
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
  if (outputPredictors){
    return(data.frame("time" = data$time, "outliers" = mask, 
                      setNames(prediction,c("lowerPredCalendarModel","upperPredCalendarModel"))))
  } else {
    return(data.frame("time" = data$time, "outliers" = mask))
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
                                              window = NULL,
                                              outputPredictors = F,
                                              logValueColumn = F) {
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
        if(class(window)=="numeric"){
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
    stop("Not implemented")
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
#' representing the time step (e.g. "15T","1H", "M", "D",...).
#' @param aggregationFunction <string> The aggregation function to use when
#  resampling the series (avg, sum, min, max)
#'
#' @return data <timeSeries> The time series resampled with the specified
#' period and aggregation function.
align_time_grid <- function(data, measurementReadingType, outputTimeStep, aggregationFunction) {
  data <- clean_ts_integrate(data, measurementReadingType)

  return(data %>%
    mutate(time = floor_date(data$time, outputTimeStep,
      week_start = getOption("lubridate.week.start", 1)
    )) %>%
    group_by(time) %>%
    summarize(
      sum = sum(value, na.rm = TRUE),
      avg = mean(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    ) %>%
    mutate(value = !!as.name(aggregationFunction)) %>%
    ungroup() %>%
    select(time, value))
}
