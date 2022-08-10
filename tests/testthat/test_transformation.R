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


create_serie <- function(n, values, timestep = "hours", start = ymd_hms("2020-01-01 00:00:00"), featuresName = c("value")) {
  funcs <- list(
    "mins" = minutes,
    "hours" = hours,
    "days" = days,
    "months" = months,
    "years" = years
  )
  func <- funcs[[timestep]]
  tmp <- (
    data.frame(
      time = seq(start, start + func(n - 1), by = timestep),
      value = values
    )
  )
  colnames(tmp) <- c("time", featuresName)
  return(tmp)
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


test_that("Lowpass filtering", {
  testdata <- create_serie(10, rep(1, 10), timestep = "hours")
  testdata$value1 <- seq(1, 10)
  testdata$value2 <- seq(1, 10)
  testdata$value3 <- rev(seq(1, 10))
  testdata$value4 <- rev(seq(1, 10))
  smoothingTimeScaleParameter <- 0.5
  expected <- testdata
  expected$value2_lpf <- c(
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
  expected$value4_lpf <- c(
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
    all.equal(obtained, expected, tolerance = 0.00001),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Winter, Without holidays", {
  start <- ymd_hms("2020-01-01 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(localtime, isHolidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-01-01"),
    weekday = 3,
    dayYear = 1,
    timestamp = 1577836800,
    isWeekend = FALSE,
    # isHolidays = NA,
    year = 2020,
    quarter = 1,
    semester = 1,
    season = "Winter",
    monthInt = 1,
    month = 1,
    day = 1,
    hour = 1,
    hourBy3 = 1,
    hourBy4 = 1,
    hourBy6 = 1,
    hourBy8 = 1,
    weekhour = 1,
    minute = 0,
    second = 0
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Spring, Without holidays", {
  start <- ymd_hms("2020-04-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(localtime, isHolidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-04-01"),
    weekday = 3,
    dayYear = 92,
    timestamp = 1585778399,
    isWeekend = FALSE,
    # isHolidays = NA,
    year = 2020,
    quarter = 2,
    semester = 1,
    season = "Spring",
    monthInt = 4,
    month = 4,
    day = 1,
    hour = 23,
    hourBy3 = 8,
    hourBy4 = 6,
    hourBy6 = 4,
    hourBy8 = 3,
    weekhour = 23,
    minute = 59,
    second = 59
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Summer, Without holidays", {
  start <- ymd_hms("2020-07-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(localtime, isHolidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-07-01"),
    weekday = 3,
    dayYear = 183,
    timestamp = 1593640799,
    isWeekend = FALSE,
    # isHolidays = NA,
    year = 2020,
    quarter = 3,
    semester = 2,
    season = "Summer",
    monthInt = 7,
    month = 7,
    day = 1,
    hour = 23,
    hourBy3 = 8,
    hourBy4 = 6,
    hourBy6 = 4,
    hourBy8 = 3,
    weekhour = 23,
    minute = 59,
    second = 59
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Winter, Without holidays", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(localtime, isHolidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-12-27"),
    weekday = 7,
    dayYear = 362,
    timestamp = 1609027200,
    isWeekend = TRUE,
    # isHolidays = NA,
    year = 2020,
    quarter = 4,
    semester = 2,
    season = "Winter",
    monthInt = 12,
    month = 12,
    day = 27,
    hour = 1,
    hourBy3 = 1,
    hourBy4 = 1,
    hourBy6 = 1,
    hourBy8 = 1,
    weekhour = 1,
    minute = 0,
    second = 0
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Winter, With holidays", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  holidays <- c(ymd("2020-12-27"))
  obtained <- (
    calendar_components(testdata, "Europe/Madrid", holidays) %>% select(-c(localtime))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-12-27"),
    weekday = 7,
    dayYear = 362,
    timestamp = 1609027200,
    isWeekend = TRUE,
    isHolidays = TRUE,
    year = 2020,
    quarter = 4,
    semester = 2,
    season = "Winter",
    monthInt = 12,
    month = 12,
    day = 27,
    hour = 1,
    hourBy3 = 1,
    hourBy4 = 1,
    hourBy6 = 1,
    hourBy8 = 1,
    weekhour = 1,
    minute = 0,
    second = 0
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(18, 19, 20, 21, 22, 23, 24), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating")
  baseTemperature <- 21
  expected <- testdata
  expected$heating <- c(3, 2, 1, 0, 0, 0, 0)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating under", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating")
  baseTemperature <- 20
  expected <- testdata
  expected$heating <- c(10, 9, 8, 7, 6, 5, 4)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating over", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(21, 22, 23, 24, 25, 26, 27), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating")
  baseTemperature <- 20
  expected <- testdata
  expected$heating <- rep(0, 7)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(22, 23, 24, 25, 26, 27, 28), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling")
  baseTemperature <- 25
  expected <- testdata
  expected$cooling <- c(0, 0, 0, 0, 1, 2, 3)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling under", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling")
  baseTemperature <- 25
  expected <- testdata
  expected$cooling <- rep(0, 7)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling")
  baseTemperature <- 25
  expected <- testdata
  expected$cooling <- c(1, 2, 3, 4, 5, 6, 7)
  obtained <- (
    degree_raw(testdata, featuresName, baseTemperature, outputFeaturesName, mode = "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(18, 19, 20, 21, 22, 23, 24), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating21")
  baseTemperature <- 21
  expected <- create_serie(7, c(3, 2, 1, 0, 0, 0, 0), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "heating", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating under. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating20")
  baseTemperature <- 20
  expected <- create_serie(7, c(10, 9, 8, 7, 6, 5, 4), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "heating", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating over. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(21, 22, 23, 24, 25, 26, 27), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("heating20")
  baseTemperature <- 20
  expected <- create_serie(7, rep(0, 7), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "heating", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(22, 23, 24, 25, 26, 27, 28), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling25")
  baseTemperature <- 25
  expected <- create_serie(7, c(0, 0, 0, 0, 1, 2, 3), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling under. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling25")
  baseTemperature <- 25
  expected <- create_serie(7, rep(0, 7), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days", featuresName = featuresName)
  outputFeaturesName <- c("cooling25")
  baseTemperature <- 25
  expected <- create_serie(7, c(1, 2, 3, 4, 5, 6, 7), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over. infreq=H, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  serie <- c(rep(25, 12), rep(27, 12), rep(26, 12), rep(28, 12))
  featuresName <- c("temperature")
  testdata <- create_serie(48, serie, timestep = "hours", featuresName = featuresName)
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  outputFeaturesName <- c("cooling25")
  baseTemperature <- 25
  expected <- create_serie(2, c(1, 2), timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over. infreq=D, outfreq=M", {
  start <- ymd_hms("2020-12-27 00:00:00")
  featuresName <- c("temperature")
  testdata <- create_serie(31, c(rep(25, 10), rep(26, 10), rep(28, 11)),
    timestep = "days", featuresName = featuresName
  )
  testdata$time <- with_tz(force_tz(as_datetime(testdata$time), "Europe/Madrid"), "UTC")
  outputFeaturesName <- c("heating25")
  baseTemperature <- 25
  expected <- create_serie(1, 0 + 10 * 1 + 3 * 11, timestep = "days", featuresName = outputFeaturesName)
  expected$time <- lubridate::force_tz(expected$time, "Europe/Madrid")
  obtained <- (
    degree_days(testdata, featuresName, "Europe/Madrid", baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1M")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Normalize range int", {
  testdata <- data.frame(
    value1 = c(0, 0, 25, 25, 100, 100),
    value2 = c(0, 0, 50, 50, 100, 100)
  )
  obtained <- normalise_range(testdata, lower = 0, upper = 1)
  expected_values <- data.frame(
    value1 = c(0, 0, 0.25, 0.25, 1.0, 1.0),
    value2 = c(0, 0, 0.50, 0.50, 1.0, 1.0)
  )
  expected_scaling <- data.frame(
    value1 = c(0, 100),
    value2 = c(0, 100)
  )
  row.names(expected_scaling) <- c("min", "max")
  expect(
    (all(obtained$values == expected_values) &
      all(obtained$scalingAttr == expected_scaling)),
    "Expected and obtained are different"
  )
})

test_that("Normalize range int with thresholds", {
  testdata <- data.frame(
    value1 = c(0, 0, 25, 25, 100, 100),
    value2 = c(0, 0, 50, 50, 100, 100)
  )
  obtained <- normalise_range(
    testdata,
    lower = 0,
    upper = 1,
    lowerThreshold = c(0, 0),
    upperThreshold = c(100, 100)
  )
  expected_values <- data.frame(
    value1 = c(0, 0, 0.25, 0.25, 1.0, 1.0),
    value2 = c(0, 0, 0.5, 0.5, 1.0, 1.0)
  )
  expect(
    all(obtained$values == expected_values),
    "Expected and obtained are different"
  )

  expected_scaling <- data.frame(
    value1 = c(0, 100),
    value2 = c(0, 100)
  )
  expect(
    all(obtained$scalingAttr == expected_scaling),
    "Expected and obtained are different"
  )
})

test_that("normalize daily", {
  serie <- c(rep(10, 12), rep(20, 12), rep(5, 12), rep(10, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  obtained <- normalise_daily(testdata, localTimeZone = "Europe/Madrid")
  serie <- c(
    rep(0.027777, 12), rep(0.0555555, 12),
    rep(0.027777, 12), rep(0.0555555, 12)
  )
  expected <- create_serie(48, serie, timestep = "hours")
  expected$time <- with_tz(force_tz(expected$time, "Europe/Madrid"), "UTC")
  expect(
    all.equal(as.data.frame(obtained), expected, tolerance = 0.0001),
    "Expected and obtained are different"
  )
})

test_that("normalize zscore", {
  serie <- c(rep(10, 12), rep(20, 12), rep(5, 12), rep(10, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  obtained <- normalise_zscore(testdata$value)
  serie <- c(
    rep(-0.2270134, 12), rep(1.5890939, 12),
    rep(-1.1350670, 12), rep(-0.2270134, 12)
  )
  expect(
    (all.equal(obtained$values, serie, tolerance = 0.0001)) &
      (all.equal(obtained$scalingAttr["mean", 1], 11.250000, tolerance = 0.0001)) &
      (all.equal(obtained$scalingAttr["sd", 1], 5.506283, tolerance = 0.0001)),
    "Expected and obtained are different"
  )
})

# test_that("normalize load relative", {
#  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
#  testdata <- create_serie(48, serie, timestep = "hours")
#  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
#  testdata$temperature <- serie
#  obtained <- normalise_load(testdata, "Europe/Madrid")
#  serie <- c(rep(0.0, 12), rep(1, 12), rep(1, 12), rep(0, 12))
#  values <- data.frame(t(matrix(serie, ncol = 2)))
#  names(values) <- as.character(seq(0, 23))
#  expected <- list(
#    dates = ymd(c("2020-01-01", "2020-01-02")),
#    values = as_tibble(values)
#  )
#  expect(
#    all.equal(expected, obtained),
#    "Expected and obtained are different"
#  )
# })
#
# test_that("normalize load absolute", {
#  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
#  testdata <- create_serie(48, serie, timestep = "hours")
#  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
#  testdata$temperature <- serie
#  obtained <- normalize_load(testdata, "Europe/Madrid", "absolute")
#  serie <- c(rep(0.0, 12), rep(1, 12), rep(0, 12), rep(0, 12))
#  values <- data.frame(t(matrix(serie, ncol = 2)))
#  names(values) <- as.character(seq(0, 23))
#  expected <- list(
#    dates = ymd(c("2020-01-01", "2020-01-02")),
#    values = as_tibble(values)
#  )
#  expect(
#    all.equal(expected, obtained),
#    "Expected and obtained are different"
#  )
# })
#
# test_that("normalize load relative inputvars", {
#  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
#  testdata <- create_serie(48, serie, timestep = "hours")
#  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
#  testdata$temperature <- serie
#  inputVars <- c(
#    "load_curves",
#    "days_weekend",
#    "days_of_the_week",
#    "daily_cons",
#    "daily_temp"
#  )
#  obtained <- normalize_load(testdata, "Europe/Madrid", inputVars = inputVars, holidays = "../../R/holidays.csv")
#  serie <- c(rep(0.0, 12), rep(1, 12), rep(1, 12), rep(0, 12))
#  values <- data.frame(t(matrix(serie, ncol = 2)))
#  names(values) <- as.character(seq(0, 23))
#  values$is_weekend <- c(0, 0)
#  values$weekday <- c(0, 1)
#  values$value <- c(1, 0)
#  values$temperature <- c(1, 0)
#  expected <- list(
#    dates = ymd(c("2020-01-01", "2020-01-02")),
#    values = as_tibble(values)
#  )
#  expect(
#    all.equal(expected, obtained),
#    "Expected and obtained are different"
#  )
# })
