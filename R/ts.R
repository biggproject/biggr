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
detect_time_step <- function(data, maxMissingTimeSteps=0) {
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
        "31622400"= "Y"
    )
    seqsteps <- list(
        "S" = "sec",
    "T" = "min",
    "H" = "hour",
    "D" = "day",
    "W" = "week",
    "M" = "month",
    "Y" = "year"
    )
    roundsteps <- list(
        "S" = "second",
    "T" = "minute",
    "H" = "hour",
    "D" = "day",
    "W" = "week",
    "M" = "month",
    "Y" = "year"
    )

    # Use contingency table to identify
    # most common frequency
    crosstab <- data %>%
        mutate(
            time_lag1 = lag(time),
            secs = as.integer(
                difftime(time, time_lag1, units="secs")
            )
        ) %>%
        group_by(secs) %>%
        summarize(freq = n()) %>%
        arrange(
            ifelse(maxMissingTimeSteps > 0, desc(freq), secs)
        )

    secs <- as.character(crosstab$secs[1])
    # WARNING: Expecting significative amount of samples in
    # standard frequency (seconds interval) otherwise fails
    if (!secs %in% names(timesteps)) {
        return(NULL)
    }
    timestep <- as.character(timesteps[secs])

    # Identify missing entries and weight missing data
    if (maxMissingTimeSteps > 0)
    {
    # Create synthetic timeserie with no missing point in order to
    # compare it against the one provided.
    timesteps_ <- unique(
        as.character(
            timesteps[as.character(sort(as.integer(names(timesteps))))]
        ))
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
            start <- round_date(data$time[1], roundsteps[timestep_])
        end <- round_date(data$time[length(data$time)], roundsteps[timestep_])
        if (timestep_ %in% c("M", "Y")) {
            start <- data$time[1]
            end <- data$time[length(data$time)]
            }
            timeseq = seq(start, end, by=as.character(seqsteps[timestep_]))
            # TEMPFIX: seq.Date not properly adding months
        if ((timestep_ == "M") & (day(start) == 31)) {
                mask <- day(timeseq) == 1
            timeseq[mask] <- timeseq[mask] - 60*60*24
        }

        # Obtain number of missing comparing against synthetic
        # complete serie
            newdata <- data.frame(time = timeseq)
            n_total <- dim(newdata)[1]
            n_missing <- as.integer(
                newdata %>%
                    left_join(data, by="time") %>%
                    select(value) %>%
                    summarise(sum(is.na(.)))
            )
        ratio_missing = n_missing / n_total
        if (ratio_missing < maxMissingTimeSteps)
        {
            timestep = timestep_
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
    if (timestep == "M")
    {
        tmp <- data %>%
            mutate(
                totaldays = as.integer(days_in_month(time)),
                days = mday(time),
                daystoend =  totaldays - days,
                daysfromstart = day(time)
            )
        timestep <- ifelse(
        all(tmp$daystoend == 0), "M",
        ifelse(
            all(tmp$daysfromstart == 1), "MS", NULL
        ))
    }else if (timestep == "Y")
    {
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
        ifelse(all(sof_mask), "YS", NULL
        ))
    }
    return(timestep)
}


#' Detect elements of the time series outside a minimum and maximum range.
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
detect_ts_min_max_outliers <- function(data, min, max, minSeries=NULL, maxSeries=NULL) {
    min_mask <- (data$value < min)
    max_mask <- (data$value > max)

    if (!is.null(minSeries))
    {
        mask <- data %>%
            left_join(minSeries, by="time") %>%
        mutate(outlier = value.x < value.y)
        min_mask <- mask[["outlier"]]
    }
    if (!is.null(maxSeries))
    {
        mask <- data %>%
            left_join(maxSeries, by="time") %>%
        mutate(outlier = value.x > value.y)
        min_mask <- mask[["outlier"]]
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

detect_ts_zscore_outliers <- function(data, zScoreThreshold, window=NULL, zScoreExtremesSensitive=NULL) {
}

detect_ts_calendar_model_outliers <- function() {
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

detect_static_min_max_outliers <- function(data, min, max, includeMin=TRUE, includeMax=TRUE) {
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
detect_static_reg_exp <- function(data, regExpValues, negativeExp=FALSE) {
    mask <- sapply(regExpValues, function(p) { grepl(p, data) })
    values <- rowSums(mask) > 0
    if (negativeExp == TRUE)  values <- rowSums(!mask) == length(regExpValues)
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

fill_ts_na <- function(data, outliersMinMax, outliersZScore, outliersCalendarModel, methodFillNA="linearInterpolation", maxGap=NULL, fillMask=NULL) {
    # NOTE: outliersMinMax and outliersZScore not used

    newdata <- data
    if (!is.null(fillMask)) mask <- mask & fillMask
    if (!is.null(maxGap)) {
        missing <- data %>%
        drop_na(.) %>%
            mutate(
                time_lag1 = lag(time),
                secs = as.integer(
                    difftime(time, time_lag1, units="secs")
                ),
        valid = secs <= as.integer(duration(maxGap))
    )
    # TOBEFIXED: Last valid reading ignored
    mask <- data %>%
        left_join(missing, by="time") %>%
            fill(valid, .direction="up")
    newdata <- data[mask[["valid"]] == TRUE, ]
    }

    if (methodFillNA == "calendarModel") {
        stop("Not implemented")
    }
    else if (methodFillNA == "backward") {
    newdata <- newdata %>%
            fill(value, .direction="up")
        colnames(newdata) <- c("time", "value.fill")
    }
    else if (methodFillNA == "forward") {
    newdata <- newdata %>%
            fill(value, .direction="down")
        colnames(newdata) <- c("time", "value.fill")
    }
    else if (methodFillNA == "linearInterpolation") {
        newdata$value.fill <- as.vector(na.approx(newdata$value, rule = 2))
    }
    else {
        stop("Fill method not supported")
    }
    return( data %>%
        left_join(newdata) %>%
    mutate(value = coalesce(value, value.fill)) %>%
    select(time, value))
}

