library(lubridate)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

readdata <- function(filename) {
  columns <- c("time", "value")
  data <- read_delim(filename, ";",
    col_names = columns,
    col_types = cols(
      time = col_character(),
      value = col_double()
    )
  )
  data$time <- ymd_hms(data$time)
  return(data)
}

b2backtest <- function(filename, expected, maxMissingTimeSteps = 0, approxTimeSteps = FALSE) {
  testdata <- readdata(filename)
  obtained <- detect_time_step(testdata, maxMissingTimeSteps, approxTimeSteps)
  expect(
    obtained == expected,
    "Expected and obtained are different"
  )
}

b2backtest_missing <- function(filename, expected, maxMissingTimeSteps, missing) {
  testdata <- readdata(filename)
  if (maxMissingTimeSteps > 0) {
    testdata <- testdata %>%
      filter(!row_number() %in% missing)
  }
  obtained <- detect_time_step(testdata, maxMissingTimeSteps)
  expect(
    obtained == expected,
    "Expected and obtained are different"
  )
}

test_that("Detect secondly time step. No missing second", {
  filename <- "test_data/test_ts_secondly.csv"
  expected <- "S"
  b2backtest(filename, expected)
})

test_that("Detect minutely time step. No missing minute", {
  filename <- "test_data/test_ts_minutely.csv"
  expected <- "T"
  b2backtest(filename, expected)
})

test_that("Detect hourly time step. No missing hour", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "H"
  b2backtest(filename, expected)
})

test_that("Detect daily time step. No missing day", {
  filename <- "test_data/test_ts_daily.csv"
  expected <- "D"
  b2backtest(filename, expected)
})

test_that("Detect weekly time step. No missing week", {
  filename <- "test_data/test_ts_weekly.csv"
  expected <- "W"
  b2backtest(filename, expected)
})

test_that("Detect month start step. No missing month start", {
  filename <- "test_data/test_ts_month_start.csv"
  expected <- "MS"
  b2backtest(filename, expected)
})

test_that("Detect month end step. No missing month end", {
  filename <- "test_data/test_ts_month_end.csv"
  expected <- "M"
  b2backtest(filename, expected)
})

test_that("Detect year start step. No missing year start", {
  filename <- "test_data/test_ts_year_start.csv"
  expected <- "YS"
  b2backtest(filename, expected)
})

test_that("Detect year end step. No missing year end", {
  filename <- "test_data/test_ts_year_end.csv"
  expected <- "Y"
  b2backtest(filename, expected)
})

test_that("Detect secondly time step. No missing second. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_secondly.csv"
  expected <- "S"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect minutely time step. No missing minute. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_minutely.csv"
  expected <- "T"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect hourly time step. No missing hour. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "H"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect daily time step. No missing day. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_daily.csv"
  expected <- "D"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect weekly time step. No missing week. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_weekly.csv"
  expected <- "W"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect month start step. No missing month start. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_month_start.csv"
  expected <- "MS"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect month end step. No missing month end. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_month_end.csv"
  expected <- "M"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect year end step. No missing year end. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_year_end.csv"
  expected <- "Y"
  b2backtest(filename, expected, 0.05)
})

test_that("Detect hourly time step. Missing hours. maxMissingTimeSteps not set", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "H"
  b2backtest_missing(filename, expected, 0, c(2, 3, 4, 5))
})

test_that("Detect hourly time step. Missing hours. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "H"
  data <- readdata(filename)
  mask <- hour(data$time) != 0
  missing <- head(which(mask == TRUE), 10)
  b2backtest_missing(filename, expected, 0.5, missing)
})

test_that("Detect hourly time step. Missing hours. maxMissingTimeSteps set", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  mask <- ((hour(data$time) != 0) & (day(data$time) %in% c(3, 4)))
  missing <- which(mask == TRUE)
  expected <- "D"
  b2backtest_missing(filename, expected, 0.4, missing)
})

test_that("Detect hourly time step. No missing hour. Approx. timesteps", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "H"
  b2backtest(filename, expected)
})

create_serie <- function(n, values, timestep = "hours", start=ymd_hms("2020-01-01 00:00:00")) {
  funcs <- list(
    "mins" = minutes,
    "hours" = hours,
    "days" = days,
    "months" = months,
    "years" = years
  )
  func <- funcs[[timestep]]
  return(
    data.frame(
      time = seq(start, start + func(n - 1), by = timestep),
      value = values
    )
  )
}

create_serie_days <- function(n_days, values) {
  start <- ymd_hms("2020-01-01 00:00:00")
  return(
    data.frame(
      time = seq(start, start + days(n_days - 1), by = "days"),
      value = values
    )
  )
}


test_that("Detect min max outliers. No outlier", {
  testdata <- create_serie(10, rep(10, 10))
  expected <- rep(FALSE, 10)
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers. Min max outliers", {
  testdata <- create_serie(10, rep(10, 10))
  # outliers
  testdata$value[c(2, 4)] <- 1
  testdata$value[c(6, 8)] <- 20
  expected <- rep(FALSE, 10)
  expected[c(2, 4, 6, 8)] <- TRUE
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers series. No outlier", {
  testdata <- create_serie(10, rep(10, 10))
  minSeries <- create_serie(10, rep(10, 10))
  maxSeries <- create_serie(10, rep(10, 10))
  expected <- rep(FALSE, 10)
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15, minSeries, maxSeries)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers series. No outlier", {
  testdata <- create_serie(10, rep(10, 10))
  minSeries <- create_serie(10, rep(10, 10))
  maxSeries <- create_serie(10, rep(10, 10))
  expected <- rep(FALSE, 10)
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15, minSeries, maxSeries)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})


test_that("Detect min max outliers series. No outlier ", {
  testdata <- create_serie(10, rep(10, 10))
  minSeries <- create_serie(10, c(10, 10, 10, NA, 10, NA, 10, 10, 10, 10))
  maxSeries <- create_serie(10, c(10, 10, 10, NA, 10, NA, 10, 10, 10, 10))
  expected <- rep(FALSE, 10)
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15, minSeries, maxSeries)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers series with different freq. No outlier ", {
  testdata <- create_serie(48, rep(10, 48))
  minSeries <- create_serie_days(3, c(10, 10, 12))
  maxSeries <- create_serie_days(3, c(10, 10, 12))
  expected <- rep(FALSE, 48)
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15, minSeries, maxSeries)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers series with different freq. No outlier ", {
  testdata <- create_serie(48, rep(10, 48))
  minSeries <- create_serie_days(3, c(10, 10, 12))
  maxSeries <- create_serie_days(3, c(8, 10, 12))
  expected <- c(rep(TRUE, 24), rep(FALSE, 24))
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15, minSeries, maxSeries)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers. Min max outliers", {
  testdata <- create_serie(10, rep(10, 10))
  # outliers
  testdata$value[c(2, 4)] <- 1
  testdata$value[c(6, 8)] <- 20
  expected <- rep(FALSE, 10)
  expected[c(2, 4, 6, 8)] <- TRUE
  obtained <- detect_ts_min_max_outliers(testdata, min = 5, max = 15)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect z-score outliers series. No outlier ", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(5, 12), rep(10, 12)))
  expected <- rep(FALSE, 48)
  zScoreThreshold <- 2
  obtained <- detect_ts_zscore_outliers(testdata, zScoreThreshold)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect z-score outliers series. With outlier ", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(5, 12), rep(10, 12)))
  expected <- rep(TRUE, 48)
  zScoreThreshold <- 0.5
  obtained <- detect_ts_zscore_outliers(testdata, zScoreThreshold)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect outlier elements based on calendar model. No window", {
  # WARNING: Naive test to check it launches
  # No quality analysis done
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  obtained <- detect_ts_calendar_model_outliers(
    data,
    holidaysCalendar = as_date("2020-03-02")
  )
  expected <- rep(FALSE, 97)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect outlier elements based on calendar model. With window", {
  # WARNING: Naive test to check it launches
  # No quality analysis done
  # set.seed(123); testdata <- create_serie(144, sample(seq(1000), 144))
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  obtained <- detect_ts_calendar_model_outliers(
    data,
    calendarFeatures = c("H"),
    holidaysCalendar = as_date("2020-03-02"),
    window = "10H"
  )
  expected <- rep(FALSE, 97)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})


test_that("Detect z-score outliers series. With window and outlier ", {
  set.seed(123)
  testdata <- create_serie(48, sample(seq(100), 48))
  expected <- c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
    TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
    FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
    TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE
  )
  zScoreThreshold <- 0.5
  window <- "3H"
  obtained <- detect_ts_zscore_outliers(testdata, zScoreThreshold, window)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers in vector. No outliers", {
  testdata <- c(10, 10, 15, 15, 10, 10)
  expected <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  obtained <- detect_static_min_max_outliers(testdata, min = 5, max = 20)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers in vector. Not outliers (min max included)", {
  testdata <- c(10, 10, 15, 15, 10, 10)
  expected <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  obtained <- detect_static_min_max_outliers(testdata, min = 10, max = 15)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers in vector. With outliers (min max not included)", {
  testdata <- c(10, 10, 15, 15, 10, 10)
  expected <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  obtained <- detect_static_min_max_outliers(testdata, min = 10, max = 15, includeMin = FALSE, includeMax = FALSE)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect min max outliers in vector. With outliers (min max not included)", {
  testdata <- c(5, 5, 20, 20, 5, 5)
  expected <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  obtained <- detect_static_min_max_outliers(testdata, min = 10, max = 15)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect expressions in vector. Positive match", {
  testdata <- c("abc", "def", "ghi", "jkl")
  regexs <- c("ab.", ".ef")
  expected <- c(TRUE, TRUE, FALSE, FALSE)
  obtained <- detect_static_reg_exp(testdata, regexs)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect expressions in vector. Negative match", {
  testdata <- c("abc", "def", "ghi", "jkl")
  regexs <- c("ab.", ".ef")
  expected <- c(FALSE, FALSE, TRUE, TRUE)
  obtained <- detect_static_reg_exp(testdata, regexs, negativeExp = TRUE)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect expressions in vector. Negative match", {
  testdata <- c("testabc", "dtestf", "test1", "ghi", "jkltest0")
  regexs <- c("^test+", "test0$")
  expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
  obtained <- detect_static_reg_exp(testdata, regexs, negativeExp = TRUE)
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Fill ts na. No missing hour", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  expected <- (data)

  outliersMinMax <- rep(0, dim(data)[1])
  outliersZScore <- rep(0, dim(data)[1])
  outliersCalendarModel <- rep(0, dim(data)[1])
  methodFillNA <- "linearInterpolation"
  maxGap <- "5H"
  obtained <- fill_ts_na(
    data, outliersMinMax, outliersZScore,
    outliersCalendarModel, methodFillNA, maxGap
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

fill_test <- function(data, methodFillNA, maxGap, expected) {
  outliersMinMax <- rep(0, dim(data)[1])
  outliersZScore <- rep(0, dim(data)[1])
  outliersCalendarModel <- rep(0, dim(data)[1])
  obtained <- fill_ts_na(
    data, outliersMinMax, outliersZScore,
    outliersCalendarModel, methodFillNA, maxGap
  )
  expect(
    all(obtained == expected, na.rm = TRUE),
    "Expected and obtained are different"
  )
}

test_that("Fill ts na. One missing hour. Imputation done. Backward", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  data$value <- seq(dim(data)[1])
  expected <- data
  data$value[3] <- NA
  methodFillNA <- "linearInterpolation"
  maxGap <- "2H"
  fill_test(data, methodFillNA, maxGap, expected)
})

test_that("Fill ts na. One missing hour. Imputation done. Forward", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  data$value <- seq(dim(data)[1])
  expected <- data
  data$value[3] <- NA
  expected$value[3] <- 2
  methodFillNA <- "forward"
  maxGap <- "2H"

  fill_test(data, methodFillNA, maxGap, expected)
})

test_that("Fill ts na. One missing hour. Imputation done. Interpolation", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  data$value <- seq(dim(data)[1])
  expected <- data
  data$value[3] <- NA
  expected$value[3] <- 4
  methodFillNA <- "backward"
  maxGap <- "2H"

  fill_test(data, methodFillNA, maxGap, expected)
})

test_that("Fill ts na. One missing hour. Imputation not done. Backward", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  data$value <- seq(dim(data)[1])
  data$value[3] <- NA
  data$value[4] <- NA
  data$value[5] <- NA
  expected <- data
  methodFillNA <- "backward"
  maxGap <- "2H"
  fill_test(data, methodFillNA, maxGap, expected)
})

test_that("Clean ts integrate using onChange", {
  testdata <- create_serie(7, c(5, 7, 8, 10, 11, 16, 20))
  expected <- create_serie(7, c(7, 8, 10, 11, 16, 20, NA))
  obtained <- clean_ts_integrate(testdata, measurementReadingType = "onChange")
  n_rows <- dim(obtained)[1]
  expect(
    all(obtained[0:(n_rows - 1), ] == expected[0:(n_rows - 1), ] & is.na(obtained$value[n_rows])),
    "Expected and obtained are different"
  )
})


test_that("Clean ts integrate using cumulative. No roll over", {
  testdata <- create_serie(7, c(5, 6, 8, 12, 13, 15, 18))
  expected <- create_serie(7, c(1, 2, 4, 1, 2, 3, NA))
  obtained <- clean_ts_integrate(testdata, measurementReadingType = "cumulative")
  n_rows <- dim(obtained)[1]
  expect(
    all(obtained[0:(n_rows - 1), ] == expected[0:(n_rows - 1), ] & is.na(obtained$value[n_rows])),
    "Expected and obtained are different"
  )
})

test_that("Clean ts integrate using cumulative", {
  testdata <- create_serie(7, c(5, 6, 8, 2, 3, 5, 8))
  expected <- create_serie(7, c(1, 2, 4, 1, 2, 3, NA))
  obtained <- clean_ts_integrate(testdata, measurementReadingType = "cumulative")
  n_rows <- dim(obtained)[1]
  expect(
    all(obtained[0:(n_rows - 1), ] == expected[0:(n_rows - 1), ] & is.na(obtained$value[n_rows])),
    "Expected and obtained are different"
  )
})

test_that("Clean ts integrate using cumulative multiple", {
  testdata <- create_serie(10, c(5, 6, 8, 2, 3, 5, 800, 3, 5, 8))
  expected <- create_serie(10, c(1, 2, 4, 1, 2, 795, 203, 2, 3, NA))
  obtained <- clean_ts_integrate(testdata, measurementReadingType = "cumulative")
  n_rows <- dim(obtained)[1]
  expect(
    all(obtained[0:(n_rows - 1), ] == expected[0:(n_rows - 1), ] & is.na(obtained$value[n_rows])),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D sum", {
  testdata <- create_serie(48, c(rep(1, 24), rep(2, 24)))
  expected <- create_serie(2, c(24, 48), timestep = "days")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "1D", aggregationFunction = "sum"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D avg", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  expected <- create_serie(2, c(7.5, 2), timestep = "days")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "1D", aggregationFunction = "avg"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D min", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  expected <- create_serie(2, c(5, 2), timestep = "days")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "1D", aggregationFunction = "min"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D max", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  expected <- create_serie(2, c(10, 2), timestep = "days")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "1D", aggregationFunction = "max"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})
test_that("Align time grid 15T sum. Minutes", {
  testdata <- create_serie(120, c(rep(1, 60), rep(2, 60)), timestep = "mins")
  expected <- create_serie(2, c(60, 120), timestep = "hours")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "H", aggregationFunction = "sum"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})
test_that("Align time grid 1D sum. Days", {
  testdata <- create_serie(60, rep(1, 60), timestep = "days")
  expected <- create_serie(2, c(31, 29), timestep = "months")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "m", aggregationFunction = "sum"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})
test_that("Align time grid 1D sum. Months", {
  testdata <- create_serie(24, rep(1, 24), timestep = "months")
  expected <- create_serie(2, c(12, 12), timestep = "years")
  obtained <- align_time_grid(testdata,
    measurementReadingType = "",
    outputTimeStep = "y", aggregationFunction = "sum"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})
test_that("Lowpass filtering", {
  testdata <- create_serie(10, rep(1, 10), timestep = "hours")
  testdata$value1 <- seq(1, 10)
  testdata$value2 <- seq(1, 10)
  testdata$value3 <- rev(seq(1, 10))
  testdata$value4 <- rev(seq(1, 10))
  smoothingTimeScaleParameter <- 0.5
  expected <- testdata
  expected$value2 <- c(
    0.500000,
    1.250000,
    2.125000,
    3.062500,
    4.031250,
    5.015625,
    6.007812,
    7.003906,
    8.001953,
    9.000977
  )
  expected$value4 <- c(
    5.000000,
    7.000000,
    7.500000,
    7.250000,
    6.625000,
    5.812500,
    4.906250,
    3.953125,
    2.976562,
    1.988281
  )
  columns <- c("value2", "value4")
  obtained <- lpf_ts(testdata, columns, smoothingTimeScaleParameter)
  expect(
    all.equal(obtained, expected, tolerance=0.00001),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Winter, Without holidays", {
  start=ymd_hms("2020-01-01 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-01-01"),
    weekday = 3,
    is_weekend = FALSE,
    #is_holidays = NA,
    year = 2020,
    quarter = 1,
    semester = 1,
    season = "Winter",
    month = 1,
    day = 1,
    hour = 1,
    minute = 0,
    second = 0
  ) 
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Calendar components. Spring, Without holidays", {
  start=ymd_hms("2020-04-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-04-01"),
    weekday = 3,
    is_weekend = FALSE,
    #is_holidays = NA,
    year = 2020,
    quarter = 2,
    semester = 1,
    season = "Spring",
    month = 4,
    day = 1,
    hour = 23,
    minute = 59,
    second = 59
  ) 
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Calendar components. Summer, Without holidays", {
  start=ymd_hms("2020-07-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-07-01"),
    weekday = 3,
    is_weekend = FALSE,
    #is_holidays = NA,
    year = 2020,
    quarter = 3,
    semester = 2,
    season = "Summer",
    month = 7,
    day = 1,
    hour = 23,
    minute = 59,
    second = 59
  ) 
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Calendar components. Winter, Without holidays", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-12-27"),
    weekday = 7,
    is_weekend = FALSE,
    #is_holidays = NA,
    year = 2020,
    quarter = 4,
    semester = 2,
    season = "Winter",
    month = 12,
    day = 27,
    hour = 1,
    minute = 0,
    second = 0
  ) 
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Calendar components. Winter, With holidays", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
  holidays <- c(ymd("2020-12-27"))
  obtained <- (
    calendar_components(testdata, "Europe/Madrid", holidays)
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-12-27"),
    weekday = 7,
    is_weekend = FALSE,
    is_holidays = TRUE,
    year = 2020,
    quarter = 4,
    semester = 2,
    season = "Winter",
    month = 12,
    day = 27,
    hour = 1,
    minute = 0,
    second = 0
  ) 
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(18,19,20,21,22,23,24), timestep = "days")
  baseTemperature <- 21
  expected <- create_serie(7, c(3,2,1,0,0,0,0), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating under", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10,11,12,13,14,15,16), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, c(10,9,8,7,6,5,4), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating over", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(21,22,23,24,25,26,27), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, rep(0, 7), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(22,23,24,25,26,27,28), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(0,0,0,0,1,2,3), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling under", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10,11,12,13,14,15,16), timestep = "days")
  baseTemperature <- 25 
  expected <- create_serie(7, rep(0, 7), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling over", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(26,27,28,29,30,31,32), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(1,2,3,4,5,6,7), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(18,19,20,21,22,23,24), timestep = "days")
  baseTemperature <- 21
  expected <- create_serie(7, c(3,2,1,0,0,0,0), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "heating", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating under. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10,11,12,13,14,15,16), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, c(10,9,8,7,6,5,4), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "heating", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Heating over. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(21,22,23,24,25,26,27), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, rep(0, 7), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "heating", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(22,23,24,25,26,27,28), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(0,0,0,0,1,2,3), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling under. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10,11,12,13,14,15,16), timestep = "days")
  baseTemperature <- 25 
  expected <- create_serie(7, rep(0, 7), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling over. infreq=D, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(26,27,28,29,30,31,32), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(1,2,3,4,5,6,7), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling over. infreq=H, outfreq=D", {
  start=ymd_hms("2020-12-27 00:00:00")
  serie <- c(rep(25,12), rep(27,12), rep(26, 12), rep(28, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  baseTemperature <- 25
  expected <- create_serie(2, c(1,2), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 

test_that("Degree raw. Cooling over. infreq=D, outfreq=M", {
  start=ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(31, c(rep(25, 10), rep(26, 10), rep(28, 11)),
    timestep = "days")
  testdata$time <- with_tz(force_tz(as_datetime(testdata$time), "Europe/Madrid"), "UTC")
  baseTemperature <- 25
  expected <- create_serie(1, 0 + 10*1 + 3*11, timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "M")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
}) 
