#' This function shift in time a set of features in order to be used in
#' the training and prediction of the models. It is an important step
#' for the multi-step prediction of Autoregressive models, where the estimated
#' output is directly used in the subsequent predictions.
#'
#' @param data <timeSeries> Containing the series to transform. Optionally,
#' other variables that are not declared in featuresNames can be bypassed
#' to the output
#' @param maxLag <integer> Describing the maximum lags to be considered. One
#' feature will be generated for each lag
#' @param featuresNames <list> selecting the series to transform.
#' @param predictMode <integer> predictMode
#' @param forceInitPerFeature <list> Supported "global", "input", "output"
#' @return data <timeSeries> containing the same initial information of the data
#' input argument, plus the lagged components as new columns.
lag_components <- function(data, maxLag, featuresNames = NULL, predictMode = 0, forceInitPerFeature = NULL) {
  # NOTE: Implementation by @gmor (copy&paste)

  predictionStep <- predictMode
  forceGlobalInputFeatures <- ifelse(
    "global" %in% names(forceInitPerFeature),
    forceInitPerFeature[["global"]],
    NULL
  )
  forceInitInputFeatures <- ifelse(
    "input" %in% names(forceInitPerFeature),
    forceInitPerFeature[["input"]],
    NULL
  )
  forceInitOutputFeatures <- ifelse(
    "output" %in% names(forceInitPerFeature),
    forceInitPerFeature[["output"]],
    NULL
  )

  if (is.null(featuresNames)) {
    if (ncol(data) > 2) {
      stop("Data do not contain only one series. Please, use the featuresNames argument to select the features to transform.")
    }
    if (!("time" %in% colnames(data))) {
      stop("No time column has been found.")
    }
    featuresNames <- colnames(data)[colnames(data) != "time"]
  }
  # Change the inputs if are specified in forceGlobalInputFeatures
  if (!is.null(forceGlobalInputFeatures)) {
    for (f in names(forceGlobalInputFeatures)) {
      if (!(length(forceGlobalInputFeatures[[f]]) == 1 || length(forceGlobalInputFeatures[[f]]) == nrow(data))) {
        stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 or equal to the number of rows of data argument (%s).", f, nrow(data)))
      }
      data[, f] <- forceGlobalInputFeatures[[f]]
    }
  }
  # Calculate the lag components in training mode
  if (is.null(predictionStep)) {
    for (f in featuresNames) {
      if (f %in% colnames(data)) {
        for (l in 1:maxLag) {
          data[, paste0(f, as.character(l))] <- zoo::na.fill(shift(data[, f], l), "extend")
        }
      } else {
        for (l in 1:maxLag) {
          data[, paste0(f, as.character(l))] <- NA
        }
      }
    }
    # Calculate the lag components in prediction mode
  } else {
    for (f in featuresNames) {
      # Initialise the variables Input and Output when predictionStep==0
      if (!is.null(forceInitOutputFeatures) && predictionStep == 0) {
        if (f %in% names(forceInitOutputFeatures)) {
          # Version coarced by the length of the initialization vector
          # if(!(length(forceInitOutputFeatures[[f]])==1|length(forceInitOutputFeatures[[f]])==maxLag)){
          #   stop(sprintf("forceInitOutputFeatures[[%s]] needs to have a length of 1 or maxLag (%s).",f, maxLag))
          # }
          # data[1,mapply(function(l){paste0(f,as.character(l))},1:maxLag)] <- rev(forceInitOutputFeatures[[f]])
          initItem <- forceInitOutputFeatures[[f]]
          initItem <- c(rep(initItem[1], max(0, maxLag - length(initItem))), tail(initItem, maxLag))
          data[1, mapply(function(l) {
            paste0(f, as.character(l))
          }, 1:maxLag)] <- rev(initItem)
        }
      }
      if (!is.null(forceInitInputFeatures) && predictionStep == 0) {
        if (f %in% names(forceInitInputFeatures)) {
          # Version coarced by the length of the initialization vector
          # if(!(length(forceInitInputFeatures[[f]])==1|length(forceInitInputFeatures[[f]])==maxLag)){
          #   stop(sprintf("forceInitInputFeatures[[%s]] needs to have a length of 1 or maxLag (%s).",f, maxLag))
          # }
          # data[1,mapply(function(l){paste0(f,as.character(l))},1:maxLag)] <- rev(forceInitInputFeatures[[f]])
          initItem <- forceInitInputFeatures[[f]]
          initItem <- c(rep(initItem[1], maxLag - length(initItem)), initItem)
          data[1, mapply(function(l) {
            paste0(f, as.character(l))
          }, 1:maxLag)] <- rev(initItem)
        }
      }
      # Generate the lag component for the next prediction
      if (predictionStep > 0 && nrow(data) >= (predictionStep + 1)) {
        if (maxLag > 1) {
          data[predictionStep + 1, mapply(function(l) {
            paste0(f, as.character(l))
          }, 1:maxLag)] <-
            data[predictionStep, c(f, mapply(function(l) {
              paste0(f, as.character(l))
            }, 1:(maxLag - 1)))]
        } else {
          data[predictionStep + 1, paste0(f, as.character(1))] <-
            data[predictionStep, f]
        }
      }
    }
  }
  return(data)
}

#' This function computes the first-order low pass filter for smoothing
#' a time series
#'
#' @param data <timeSeries> containing the series to transform. Optionally,
#' other variables that are not declared in featuresNames can be bypassed to
#' the output
#' @param featuresNames <string> selecting the series to transform
#' @param smoothingTimeScaleParameter <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.
#' @return data <timeSeries> containing the same initial information of the
#' data input argument, but updating the featuresNames variables to the
#' low-pass filtered version
lpf_ts <- function(data, featuresNames, smoothingTimeScaleParameter) {
  # NOTE: Implementation by @gmor (copy&paste)
  lpf <- function(x, alpha) {
    y <- numeric(length(x))
    for (i in 1:length(x)) {
      if (is.na(y[i - 1]) || i == 1) {
        y[i] <- (alpha) * x[i]
      } else {
        y[i] <- (1 - alpha) * y[i - 1] + (alpha) * x[i]
      }
    }
    ## Return (afterwards the init value y[1], must be handled)
    return(y)
  }
  return(data %>% mutate_at(featuresNames, lpf, smoothingTimeScaleParameter))
}

#' Physical transformation of the smoothing time scale parameter to consider
#' no affectance to the output variable after a known number of hours
#'
#' @param data <timeSeries> to filter
#' @param timeConstantInHours <float> It means the number of hours that are
#' affected by a certain change in the input time series (data argument).
#' For instance, when outdoor temperature is considered, this time constant
#' in hours correspond to the thermal inertia of the building.
#' @return smoothingTimeScaleParameter <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.
get_lpf_smoothing_time_scale <- function(data, timeConstantInHours) {
  # NOTE: Implementation by @gmor (copy&paste)
  steps <- list(
    "S" = 60,
    "T" = 60,
    "H" = 1
  )
  timestep <- detect_time_step(data)
  return(1 - (exp(1)^(-steps[[timestep]] / ((2 * pi) * timeConstantInHours / 24))))
}

#' Decompose the time in date, day of the year, day of the week, day of the
#' weekend, working day, non-working day, season, month, hour, minute, ...
#'
#' @param data <timeSeries> containing the series to transform. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones). This argument is
#' optional, by default no transformation to local time zone is done.
#' @return data <timeSeries> containing the same initial information of data
#' input argument, plus the calendar components as new columns.
calendar_components <- function(data, localTimeZone = NULL, holidays = NULL) {
  getSeason <- function(dates) {
    # NOTE: This function was taken from the following stackoverflow post:
    # http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to.
    # 2012 is a good year to which to convert all of the dates;
    # since it is a leap year
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15", format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15", format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15", format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(dates, format = "2012-%m-%d"))

    ifelse(d >= WS | d < SE, "Winter",
      ifelse(d >= SE & d < SS, "Spring",
        ifelse(d >= SS & d < FE, "Summer", "Fall")
      )
    )
  }
  return(data %>%
    mutate(
      localtime = with_tz(time, localTimeZone),
      date = lubridate::date(localtime),
      weekday = wday(localtime,
        week_start = getOption("lubridate.week.start", 1)
      ),
      is_weekend = wday(localtime) %in% c(6, 7),
      is_holidays = ifelse(is.null(holidays), NA, ifelse(date %in% holidays, TRUE, FALSE)),
      year = year(localtime),
      quarter = quarter(localtime),
      semester = semester(localtime),
      season = getSeason(localtime),
      month = month(localtime),
      day = day(localtime),
      hour = hour(localtime),
      minute = minute(localtime),
      second = second(localtime),
    ) %>%
    select(-localtime))
}

#' Obtain the components of the Fourier Series, in sine-cosine form.
#' It is useful for linearising the relationship of a seasonal input
#' time series (e.g. solar azimuth, solar elevation, calendar
#' features, ...) to some output (energy consumption, indoor
#' temperatures, ...). It basically decomposes a cyclic time series
#' into a set of sine-cosine components that are used as inputs for
#' the modelling of some output, each of the components linearly depends
#' to the output.
#'
#' @param data <timeSeries> containing the series to transform.
#' This series must have a cyclic behaviour (e.g. hour of the day, day of
#' the week, solar azimuth, day of the year, ...) in order to be correctly
#' transformed. Optionally, other variables that are not declared in
#' featuresNames can be bypassed to the output..
#' @param featuresNames <list string> selecting the series to transform.
#' @param mask <boolean serie> containing the timestamps that should be
#' accounted for the transformation. The timestamps set to false will
#' consider 0's for all their related sine-cosine components. By default,
#' all elements of the time series are considered.
#' @param nHarmonics <integer> defines the number of harmonics considered
#' in the Fourier Series. A high number allows to model more precisely
#' the relation, but it considerably increase the cost of computation.
#' The number of harmonics is related with the number of features in
#' the output matrix
#' @return data <timeSeries> containing the same initial information of data input argument, plus the sine-cosine components of the Fourier Series as new columns.
fs_components <- function(data, featuresNames, mask, nHarmonics) {
  # NOTE: Implementation by @gmor (copy&paste)
  fs <- function(X, featureName, nHarmonics) {
    lapply(1:nHarmonics, function(i) {
      value <- list(
        sin(i * X * 2 * pi),
        cos(i * X * 2 * pi)
      )
      names(value) <- paste0(featureName, c("_sin_", "_cos_"), i)
      return(as.data.frame(value))
    })
  }

  tmpdata <- data
  if (!is.null(mask)) {
    tmpdata[mask, featuresNames] <- 0
  }
  for (featureName in featuresNames) {
    tmp <- fs(tmpdata[[featureName]], featureName, nHarmonics)
    data <- cbind(data, tmp)
  }
  return(data)
}
