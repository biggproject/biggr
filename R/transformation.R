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
#' @param forceGlobalInputFeatures
#' @param forceInitInputFeatures
#' @param forceInitOutputFeatures
#' @return data <timeSeries> containing the same initial information of the data
#' input argument, plus the lagged components as new columns.
lag_components <- function(data, maxLag, featuresNames = NULL, predictMode = 0,
                           forceGlobalInputFeatures = NULL, forceInitInputFeatures = NULL,
                           forceInitOutputFeatures = NULL) {
  # NOTE: Implementation by @gmor (copy&paste)

  predictionStep <- predictMode

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
      is_weekend = weekday %in% c(6, 7),
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


#' Calculate the difference between outdoor temperature and a base temperature,
#' without considering the frequency of the original data.
#'
#' @param temperature <timeSeries> of outdoor temperature of a location. Maximum
#' input frequency is daily ("D").
#' @param baseTemperature <float> describing the Balance Point Temperature (BPT)
#' used in the calculation. Below BPT in heating mode, heat would be required by
#' the building. The contrary in the case of cooling, over BPT in cooling mode
#' @param mode: string describing the calculation mode, which could be "cooling"
#' or "heating". By default, "heating" is configured.
#' @return series timeSeries of the difference between the temperature argument
#' and the selected base temperature, mantaining the original frequency of
#' temperature.
degree_raw <- function(temperature, baseTemperature, mode = "heating") {
  return(data.frame(
    time = temperature$time,
    dd = (
      if (mode == "heating") {
        pmax(0, baseTemperature - temperature$value)
      } else {
        pmax(0, temperature$value - baseTemperature)
      }
    )
  ))
}

#' Calculate the degree-days with a desired output frequency and considering
#' cooling or heating mode.
#'
#' @param temperature <timeSeries> of outdoor temperature of a location.
#' Maximum input frequency is daily ("D") or higher ("H","15T",...).
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). This
#' argument is optional, by default no transformation to local time zone is
#' done.
#' @param baseTemperature <float> describing the Balance Point
#' Temperature (BPT) used in the calculation. Below BPT in heating mode,
#' heat would be required by the building. The contrary in the case of
#' cooling, over BPT in cooling mode
#' @param mode <string> describing the calculation mode, which could be
#' "cooling" or "heating". By default, "heating" is configured.
#' @param outputTimeStep <string> The frequency used to resample the daily
#' degree days. It must be a string in ISO 8601 format representing the
#' time step. Only yearly ("Y"), monthly ("M"), daily ("D") output time
#' steps are allowed.
#' @return degreeDays <timeSeries> in the outputTimeStep of the heating or
#' cooling degree days.
degree_days <- function(temperature, localTimeZone, baseTemperature,
                        mode = "heating", outputTimeStep = "D") {
  tmp <- temperature %>%
    mutate(
      localtime = with_tz(time, localTimeZone),
      time = as_datetime(lubridate::date(localtime))
    ) %>%
    group_by(time) %>%
    summarize(
      value = mean(value, na.rm = TRUE)
    )
  dd <- degree_raw(tmp, baseTemperature, mode)
  return(dd %>%
    mutate(group = floor_date(time, roundsteps[outputTimeStep],
      week_start = getOption("lubridate.week.start", 1)
    )) %>%
    group_by(group) %>%
    summarize(
      dd = sum(dd, na.rm = TRUE)
    ) %>%
    rename(time = group) %>%
    mutate(time = with_tz(time, "UTC")))
}

#' Range normalization method
#'
#' @param data <serie> containing serie to normalize
#' @param lower <float> lower value
#' @param upper <float> upper value
#' @param lowerThreshold <float> lower threshold value
#' @param upperThreshold <float> upper threshold value
#' @return normalized serie
normalize_range <- function(data, lower = 0, upper = 1, lowerThreshold = NULL, upperThreshold = NULL) {
  if (!(is.null(lower) & is.null(upper))) {
    if (lower > upper) stop("Lower > upper not supported")
  }
  normalize <- function(x, lower, upper, lowerThreshold, upperThreshold) {
    lowervalue <- min(x)
    uppervalue <- max(x)
    if (!is.null(lower)) lowervalue <- lower
    if (!is.null(upper)) uppervalue <- upper

    minValue <- min(x, na.rm = TRUE)
    maxValue <- max(x, na.rm = TRUE)
    if ((minValue == maxValue) || (lowervalue == uppervalue)) {
      result <- rep(lower, length(x))
    } else {
      result <- ((x - minValue) / (maxValue - minValue)) * (upper - lower) + lower
    }
    if (!is.null(lowerThreshold)) {
      mask <- result < lowerThreshold
      result[mask] <- lowerThreshold
    }
    if (!is.null(upperThreshold)) {
      mask <- result > upperThreshold
      result[mask] <- upperThreshold
    }
    return(result)
  }
  return(data %>%
    mutate_all(normalize, lower, upper, lowerThreshold, upperThreshold))
}

#' Daily normalization method
#'
#' @param data <timeSeries> containing serie to normalize
#' @param method <string> Normalization method. Supported methods
#'   relative
#' @return daily normalized timeserie
normalize_daily <- function(data, method = "relative", localTimeZone) {
  if (!(method == "relative")) stop("Method not supported")
  time <- data$time
  return(data %>%
    mutate(
      date = lubridate::date(with_tz(time, localTimeZone))
    ) %>%
    group_by(date) %>%
    summarize(value = value / sum(value)) %>%
    ungroup() %>%
    mutate(time = time) %>%
    select(time, value))
}

#' Zscore normalization method
#'
#' @param data <timeSeries> containing serie to normalize
#' @return normalized timserie
normalize_zscore <- function(data) {
  return(data %>%
    mutate(value = (value - median(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)))
}

#' Normalize load
#'
#' @param <timeserie> data
#' @param localTimeZone <string> timezone
#' @param method <string> normalization method
#' @param nDayParts <int> number of part days
#' @param holidays <list date> holidays dates
#' @return normalized load
normalize_load <- function(data, localTimeZone, transformation = "relative", inputVars = c("load_curves"), nDayParts = 24, holidays = NULL) {
  if (class(holidays) == "character") {
    holidays <- read_csv(holidays, col_names = FALSE)
    holidays <- holidays$X1
  }
  tmp <- data %>%
    mutate(
      localtime = with_tz(time, localTimeZone),
      date = lubridate::date(localtime),
      hour = hour(localtime),
      daypart = ceiling(hour(localtime) / (24 / nDayParts)),
    ) %>%
    distinct(date, hour, .keep_all = TRUE)

  tmp_daily <- tmp %>%
    mutate(
      weekday = wday(date,
        week_start = getOption("lubridate.week.start", 1)
      ),
      is_weekend = weekday %in% c(6, 7),
      is_holidays = date %in% holidays
    ) %>%
    group_by(date, weekday, is_weekend, is_holidays) %>%
    summarize(
      value = mean(value, na.rm = TRUE) * 24,
      temperature = mean(temperature, na.rm = TRUE)
    ) %>%
    ungroup()

  tmp_spread <- tmp %>%
    group_by(date, daypart) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    spread(daypart, value)

  if (transformation == "relative") {
    columns <- names(tmp_spread)
    columns <- columns[!(columns %in% "date")]
    tmp_spread[, columns] <- tmp_spread[columns] / rowSums(tmp_spread[columns], na.rm = TRUE)
    # tmp_spread[columns] <- tmp_spread[columns] / rowSums(tmp_spread[columns], na.rm=TRUE)
  }

  tmp_spread <- tmp_spread %>%
    inner_join(tmp_daily, by = "date")
  dates <- tmp_spread$date
  tmp_spread$date <- NULL
  values <- normalize_range(tmp_spread, lower = 0, upper = 1)

  vars <- list(
    "load_curves" = names(tmp_spread)[!(names(tmp_spread) %in% c("is_weekend", "is_holidays", "weekday", "value", "temperature"))],
    "days_weekend" = c("is_weekend"),
    "daily_holidays" = c("is_holidays"),
    "days_of_the_week" = c("weekday"),
    "daily_cons" = c("value"),
    "daily_temp" = c("temperature")
  )

  vars <- as.vector(unlist(vars[inputVars]))
  return(list(
    dates = dates,
    values = values[vars]
  ))
}

#' Calculate affinity
#'  Code from https://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
#' Done applying a k-nearest neighboor filter to build a representation of a graph
#' connecting just the closest dataset points. However, to be symmetric, if
#' Aij is selected as a nearest neighboor, so will Aji.
#'
#' @param S <matrix> Matrix to evaluate
#' @param n.neighboors <int> Number of neighboors
#' @return Affinity matrix
make.affinity <- function(S, n.neighboors = 2) {
  N <- length(S[, 1])

  if (n.neighboors >= N) { # fully connected
    A <- S
  } else {
    A <- matrix(rep(0, N^2), ncol = N)
    for (i in 1:N) { # for each line
      # only connect to those points with larger similarity
      best.similarities <- sort(S[i, ], decreasing = TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i, ] == s)
        A[i, j] <- S[i, j]
        A[j, i] <- S[i, j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A
}

#' Calculate similarity
#' Exponential Euclidean distance (Frobenius norm) with weighting factor
#'
#' @param x1 <vector> First vector
#' @param x2 <vector> Second vector
#' @param alpha <float> Weighting factor
#' @return Distance between vectors
similarity <- function(x1, x2, alpha = 1) {
  exp(-alpha * norm(as.matrix(x1 - x2), type = "F"))
}

#' Calculate similarity matrix
#' Code from https://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
#' Similarity Matrix, S, as the matrix that at Sij=s(xi,xj) gives the
#' similarity between observations xi and xj.
#' Common similarity measures are euclidean (default) an gaussian
#'
#' @param my.data <matrix> Matrix to evaluate
#' @param similariry <function> Similarity function
#' @return Similarity matrix
make.similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA, N^2), ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j) {
        S[i, j] <- similarity(my.data[i, ], my.data[j, ])
      } else {
        S[i, j] <- 0
      }
    }
  }
  S
}

#' Cluster similar daily load curves based on the load curves itself, calendar
#' variables and outdoor temperature
#'
#' @param consumption <timeSeries> containing the total energy consumption
#' of a building.
#' @param temperature <timeSeries> containing the outdoor temperature of
#' the related building.
#' @param localTimeZone <string> specifying the local time zone related
#' to the building in analysis
#' @param kMax <integer> defining the maximum number of allowed groups in
#' the clustering proceed.
#' @param inputVars <list of strings> Select a number of features as an input
#' of the clustering.
#'   loadCurve: use the transformed version of the consumption daily load curve
#'     based on loadCurveTransformation and nDayParts arguments
#'   dailyTemperature: use the average daily outdoor temperature
#'   dailyConsumption: use the total daily consumption
#'   daysOfTheWeek: use a discrete value to represent the days of the week
#'   daysWeekend: use a boolean representing whether is weekend or not
#'   holidays: use a boolean representing whether is holiday or not
#' @param loadCurveTransformation <string> that defines the transformation
#' procedure over the consumption load curves. Possible values are:
#'   relative: All daily load curves are relative to their total daily
#'     consumption. It is the default mode.
#'   absolute: The absolute consumption is used to define each daily
#'     load curves.
#' @param nDayParts <integer> defining the parts of day used to. Possible
#'   values: 24, 12, 8, 6, 4, 3, 2.
#' @param ignoreDate <listr dates> list of dates to ifnore (holidays, weather, ..)
#' @return dailyClassification <timeSeries> in daily frequency, containing
#' the classification of each daily load curve.
#' @return absoluteLoadCurvesCentroids <matrix> with row names as the
#' identifier of the cluster, and column names as the day hours. This matrix
#' is filled with the hourly average consumption of each cluster detected.
#' @return clusteringCentroids <matrix> with row names as the identifier of
#' the cluster, and column names as the input variables used by the clustering
#' algorithm. This matrix is filled with the average values of each input
#' variable and cluster.
#' @return classificationModel <object> containing a simple classification
#' model to predict a load curve based on calendar features.
#' @return opts: dictionary
#'   normSpecs: matrix of aggregations used in the Z-score normalisation of
#'     the clustering inputs. The different types of aggregations (mean,
#'     median, std, ...) are specified as row names, and the different
#'     input features are related with column names.
#'   loadCurveTransformation: object
#'   inputVars: object
#'   nDayParts: object

clustering_dlc <- function(consumption, temperature, localTimeZone, kMax,
                           inputVars, loadCurveTransformation, nDayParts, ignoreDates = c()) {
  stop("Still under development")
  tmp <- consumption %>%
    left_join(temperature, by = "time") %>%
    filter(!(lubridate::date(time) %in% ignoreDates))

  tmp_norm <- normalize_load(tmp, localTimeZone, loadCurveTransformation, inputVars, nDayParts, ignoreDates)

  # Clustering
  # Identify best K to be used in spectral clustering
  # Calculate eigenvalues/eigenvector and find the one
  # with the highest eigenvalue decrease
  S <- make.similarity(tmp_norm$values, similariry)
  A <- make.affinity(S, 3) # use 3 neighbors (includes self)
  D <- diag(apply(A, 1, sum))
  U <- D - A
  evL <- eigen(U, symmetric = TRUE)
  evSpectrum <- log(rev(evL$values)[1:kMax] + 1e-12)
  evSpectrum <- evSpectrum - lag(evSpectrum, 1)
  if ((which.max(evSpectrum) - 1) <= 3) {
    evSpectrum[1:which.max(evSpectrum)] <- NA
  }
  k <- which.max(evSpectrum) - 1

  # Calculate spectral clustering using previous K
  # Using polynomial spectral clustering
  spectral_clust <- tryCatch(
    {
      specc(tmp_norm$values,
        centers = k,
        kernel = "polydot", kpar = list(degree = 2),
        nystrom.red = T, nystrom.sample = nrow(tmp_norm$values)[1] / 6,
        iterations = 400, mod.sample = 0.75, na.action = na.omit
      )
    },
    error = function(e) {
      1
    }
  )

  spectral_clust_valid <- (!(class(spectral_clust)[1] == "numeric"))
  s <- spectral_clust
  if (spectral_clust_valid == TRUE) s <- spectral_clust@.Data
  cluster_results <- data.frame(
    "date" = tmp_norm$dates,
    "s" = s
  )

  tmp <- tmp %>%
    mutate(
      localtime = with_tz(time, localTimeZone),
      hour = hour(localtime),
      date = lubridate::date(localtime),
      weekday = wday(date,
        week_start = getOption("lubridate.week.start", 1)
      ),
      is_weekend = weekday %in% c(6, 7)
    ) %>%
    distinct(date, hour, .keep_all = TRUE) %>%
    arrange(time)
  cluster_struct <- merge(tmp, cluster_results, by = "date")

  # Read the clustering centroids
  if (spectral_clust_valid == TRUE) {
    cluster_centroids <- spectral_clust@centers
    cluster_centroids <- data.frame(
      "s" = 1:nrow(cluster_centroids),
      cluster_centroids
    )
  } else {
    cluster_centroids <- tmp_norm$values %>%
      summarize_all(mean, na.rm = T)
    cluster_centroids <- data.frame(
      "s" = 1,
      t(cluster_centroids)
    )
  }

  # Reformat the cluster integer numbers to factors of 2 digits
  cluster_struct$s <- sprintf("%02i", as.integer(as.character(cluster_struct$s)))
  cluster_centroids <- cluster_centroids %>%
    mutate(s = sprintf("%02i", as.integer(as.character(s)))) %>%
    remove_rownames() %>%
    column_to_rownames(var = "s")

  tmp_centroids <- cluster_struct %>%
    group_by(hour, s) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(s) %>%
    spread(hour, value) %>%
    remove_rownames() %>%
    column_to_rownames(var = "s")

  return(list(
    dailyClassification = cluster_results,
    absoluteLoadCurvesCentroids = tmp_centroids,
    clusteringCentroids = cluster_centroids,
    classificationModel = NULL,
    opts = list(
      "normSpecs" = NULL,
      "loadCurveTransformation" = loadCurveTransformation,
      "inputVars" = inputVars,
      "nDayParts" = nDayParts
    )
  ))
}
