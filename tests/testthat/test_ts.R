library(lubridate)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

set.seed(123)

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

b2backtest <- function(filename, expected, maxMissingTimeSteps = 0) {
  testdata <- readdata(filename)
  obtained <- detect_time_step(testdata, maxMissingTimeSteps)
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

create_serie <- function(n_hours, values) {
  start <- ymd_hms("2020-01-01 00:00:00")
  return(
    data.frame(
      time = seq(start, start + hours(n_hours - 1), by = "hours"),
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
