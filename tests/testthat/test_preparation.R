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

b2backtest <- function(filename, expected) {
  testdata <- readdata(filename)
  obtained <- detect_time_step(testdata)
  expect(
    obtained == expected,
    "Expected and obtained are different"
  )
}

b2backtest_missing <- function(filename, expected, missing) {
  testdata <- readdata(filename)
  testdata <- testdata %>%
    filter(!row_number() %in% missing)
  obtained <- detect_time_step(testdata)
  expect(
    obtained == expected,
    "Expected and obtained are different"
  )
}

test_that("Detect secondly time step. No missing second", {
  filename <- "test_data/test_ts_secondly.csv"
  expected <- "PT1S"
  b2backtest(filename, expected)
})

test_that("Detect minutely time step. No missing minute", {
  filename <- "test_data/test_ts_minutely.csv"
  expected <- "PT1M"
  b2backtest(filename, expected)
})

test_that("Detect hourly time step. No missing hour", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "PT1H"
  b2backtest(filename, expected)
})

test_that("Detect daily time step. No missing day", {
  filename <- "test_data/test_ts_daily.csv"
  expected <- "P1D"
  b2backtest(filename, expected)
})

test_that("Detect weekly time step. No missing week", {
  filename <- "test_data/test_ts_weekly.csv"
  expected <- "P1W"
  b2backtest(filename, expected)
})

test_that("Detect month start step. No missing month start", {
  filename <- "test_data/test_ts_month_start.csv"
  expected <- "P1M"
  b2backtest(filename, expected)
})

test_that("Detect month end step. No missing month end", {
  filename <- "test_data/test_ts_month_end.csv"
  expected <- "P1M"
  b2backtest(filename, expected)
})

test_that("Detect year start step. No missing year start", {
  filename <- "test_data/test_ts_year_start.csv"
  expected <- "P1Y"
  b2backtest(filename, expected)
})

test_that("Detect year end step. No missing year end", {
  filename <- "test_data/test_ts_year_end.csv"
  expected <- "P1Y"
  b2backtest(filename, expected)
})

test_that("Detect hourly time step. Missing hours", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "PT1H"
  b2backtest_missing(filename, expected, c(2, 3, 4, 5))
})

test_that("Detect hourly time step. Missing hours", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "PT1H"
  data <- readdata(filename)
  mask <- hour(data$time) != 0
  missing <- head(which(mask == TRUE), 10)
  b2backtest_missing(filename, expected, missing)
})

test_that("Detect hourly time step. Missing hours", {
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename)
  mask <- ((hour(data$time) != 0) & (day(data$time) %in% c(3, 4)))
  missing <- which(mask == TRUE)
  expected <- "PT1H"
  b2backtest_missing(filename, expected, missing)
})

test_that("Detect hourly time step. No missing hour. Approx. timesteps", {
  filename <- "test_data/test_ts_hourly.csv"
  expected <- "PT1H"
  b2backtest(filename, expected)
})

create_serie <- function(n, values, timestep = "hours", start = ymd_hms("2020-01-01 00:00:00")) {
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
  data <- readdata(filename) %>%
    mutate(localtime = lubridate::with_tz(time, "Europe/Madrid"))
  obtained <- detect_ts_calendar_model_outliers(
    data,
    localTimeColumn = "localtime",
    valueColumn = "value",
    holidaysCalendar = as_date("2020-03-02"),
    autoDetectProfiled = FALSE
  )
  expected <- rep(FALSE, 97)
  expect(
    all(obtained$outliers == expected),
    "Expected and obtained are different"
  )
})

test_that("Detect outlier elements based on calendar model. With window", {
  # WARNING: Naive test to check it launches
  # No quality analysis done
  # set.seed(123); testdata <- create_serie(144, sample(seq(1000), 144))
  filename <- "test_data/test_ts_hourly.csv"
  data <- readdata(filename) %>%
    mutate(localtime = lubridate::with_tz(time, "Europe/Madrid"))
  obtained <- detect_ts_calendar_model_outliers(
    data,
    localTimeColumn = "localtime",
    valueColumn = "value",
    calendarFeatures = c("H"),
    holidaysCalendar = as_date("2020-03-02"),
    autoDetectProfiled = FALSE
  )
  expected <- rep(FALSE, 97)
  expect(
    all(obtained$outliers == expected),
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
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(24, 48), timestep = "days") %>%
    rename(SUM = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1D", aggregationFunction = "SUM"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D avg", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(7.5, 2), timestep = "days") %>%
    rename(AVG = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1D", aggregationFunction = "AVG"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D min", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(5, 2), timestep = "days") %>%
    rename(SUM = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1D", aggregationFunction = "MIN"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D max", {
  testdata <- create_serie(48, c(rep(5, 12), rep(10, 12), rep(2, 24)))
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(10, 2), timestep = "days") %>%
    rename(MAX = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1D", aggregationFunction = "MAX"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 15T sum. Minutes", {
  testdata <- create_serie(120, c(rep(1, 60), rep(2, 60)), timestep = "mins")
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(60, 120), timestep = "hours") %>%
    rename(SUM = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1H", aggregationFunction = "SUM"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Align time grid 1D sum. Days", {
  testdata <- create_serie(60, rep(1, 60), timestep = "days")
  testdata$isReal <- TRUE
  expected <- create_serie(2, c(31, 29), timestep = "months") %>%
    rename(SUM = value)
  expected$GAPS <- c(0, 0)
  expected$RATIO <- c(1, 1)
  obtained <- align_time_grid(testdata,
    outputFrequency = "P1M", aggregationFunction = "SUM"
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})
