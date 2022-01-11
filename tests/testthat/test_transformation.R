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
    all.equal(obtained, expected, tolerance = 0.00001),
    "Expected and obtained are different"
  )
})

test_that("Calendar components. Winter, Without holidays", {
  start <- ymd_hms("2020-01-01 00:00:00")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-01-01"),
    weekday = 3,
    is_weekend = FALSE,
    # is_holidays = NA,
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
  start <- ymd_hms("2020-04-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-04-01"),
    weekday = 3,
    is_weekend = FALSE,
    # is_holidays = NA,
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
  start <- ymd_hms("2020-07-01 21:59:59")
  testdata <- create_serie(1, rep(1, 1), timestep = "hours", start = start)
  obtained <- (
    calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
  )
  expected <- data.frame(
    time = testdata$time,
    value = testdata$value,
    date = ymd("2020-07-01"),
    weekday = 3,
    is_weekend = FALSE,
    # is_holidays = NA,
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

#test_that("Calendar components. Winter, Without holidays", {
# start=ymd_hms("2020-12-27 00:00:00")
# testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
# obtained <- (
#   calendar_components(testdata, "Europe/Madrid") %>% select(-c(is_holidays))
# )
# expected <- data.frame(
#   time = testdata$time,
#   value = testdata$value,
#   date = ymd("2020-12-27"),
#   weekday = 7,
#   is_weekend = FALSE,
#   #is_holidays = NA,
#   year = 2020,
#   quarter = 4,
#   semester = 2,
#   season = "Winter",
#   month = 12,
#   day = 27,
#   hour = 1,
#   minute = 0,
#   second = 0
# )
# expect(
#   all(obtained == expected),
#   "Expected and obtained are different"
# )
#})
#
#test_that("Calendar components. Winter, With holidays", {
# start=ymd_hms("2020-12-27 00:00:00")
# testdata <- create_serie(1, rep(1, 1), timestep = "hours", start=start)
# holidays <- c(ymd("2020-12-27"))
# obtained <- (
#   calendar_components(testdata, "Europe/Madrid", holidays)
# )
# expected <- data.frame(
#   time = testdata$time,
#   value = testdata$value,
#   date = ymd("2020-12-27"),
#   weekday = 7,
#   is_weekend = FALSE,
#   is_holidays = TRUE,
#   year = 2020,
#   quarter = 4,
#   semester = 2,
#   season = "Winter",
#   month = 12,
#   day = 27,
#   hour = 1,
#   minute = 0,
#   second = 0
# )
# expect(
#   all(obtained == expected),
#   "Expected and obtained are different"
# )
#})

test_that("Degree raw. Heating", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(18, 19, 20, 21, 22, 23, 24), timestep = "days")
  baseTemperature <- 21
  expected <- create_serie(7, c(3, 2, 1, 0, 0, 0, 0), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating under", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, c(10, 9, 8, 7, 6, 5, 4), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "heating")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating over", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(21, 22, 23, 24, 25, 26, 27), timestep = "days")
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
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(22, 23, 24, 25, 26, 27, 28), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(0, 0, 0, 0, 1, 2, 3), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling under", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days")
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
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(1, 2, 3, 4, 5, 6, 7), timestep = "days")
  obtained <- (
    degree_raw(testdata, baseTemperature, "cooling")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(18, 19, 20, 21, 22, 23, 24), timestep = "days")
  baseTemperature <- 21
  expected <- create_serie(7, c(3, 2, 1, 0, 0, 0, 0), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "heating", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating under. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days")
  baseTemperature <- 20
  expected <- create_serie(7, c(10, 9, 8, 7, 6, 5, 4), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "heating", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Heating over. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(21, 22, 23, 24, 25, 26, 27), timestep = "days")
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
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(22, 23, 24, 25, 26, 27, 28), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(0, 0, 0, 0, 1, 2, 3), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling under. infreq=D, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(10, 11, 12, 13, 14, 15, 16), timestep = "days")
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
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days")
  baseTemperature <- 25
  expected <- create_serie(7, c(1, 2, 3, 4, 5, 6, 7), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over. infreq=H, outfreq=D", {
  start <- ymd_hms("2020-12-27 00:00:00")
  serie <- c(rep(25, 12), rep(27, 12), rep(26, 12), rep(28, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  baseTemperature <- 25
  expected <- create_serie(2, c(1, 2), timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "D")
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Degree raw. Cooling over. infreq=D, outfreq=M", {
  start <- ymd_hms("2020-12-27 00:00:00")
  testdata <- create_serie(31, c(rep(25, 10), rep(26, 10), rep(28, 11)),
    timestep = "days"
  )
  testdata$time <- with_tz(force_tz(as_datetime(testdata$time), "Europe/Madrid"), "UTC")
  baseTemperature <- 25
  expected <- create_serie(1, 0 + 10 * 1 + 3 * 11, timestep = "days")
  obtained <- (
    degree_days(testdata, "Europe/Madrid", baseTemperature, "cooling", "M")
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
  obtained <- normalize_range(testdata, lower = 0, upper = 1)
  expected <- data.frame(
    value1 = c(0, 0, 0.25, 0.25, 1.0, 1.0),
    value2 = c(0, 0, 0.50, 0.50, 1.0, 1.0)
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("Normalize range int with thresholds", {
  testdata <- data.frame(
    value1 = c(0, 0, 25, 25, 100, 100),
    value2 = c(0, 0, 50, 50, 100, 100)
  )
  obtained <- normalize_range(testdata, lower = 0, upper = 1, 0.30, 0.75)
  expected <- data.frame(
    value1 = c(0.30, 0.30, 0.30, 0.30, 0.75, 0.75),
    value2 = c(0.30, 0.30, 0.50, 0.50, 0.75, 0.75)
  )
  expect(
    all(obtained == expected),
    "Expected and obtained are different"
  )
})

test_that("normalize daily", {
  serie <- c(rep(10, 12), rep(20, 12), rep(5, 12), rep(10, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  obtained <- normalize_daily(testdata, localTimeZone = "Europe/Madrid")
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
  obtained <- normalize_zscore(testdata)
  serie <- c(
    rep(0.0, 12), rep(1.816107, 12),
    rep(-0.9080536, 12), rep(0.0, 12)
  )
  expected <- create_serie(48, serie, timestep = "hours")
  expect(
    all.equal(as.data.frame(obtained), expected, tolerance = 0.0001),
    "Expected and obtained are different"
  )
})

test_that("normalize load relative", {
  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  testdata$temperature <- serie
  obtained <- normalize_load(testdata, "Europe/Madrid")
  serie <- c(rep(0.0, 12), rep(1, 12), rep(1, 12), rep(0, 12))
  values <- data.frame(t(matrix(serie, ncol = 2)))
  names(values) <- as.character(seq(0, 23))
  expected <- list(
    dates = ymd(c("2020-01-01", "2020-01-02")),
    values = as_tibble(values)
  )
  expect(
    all.equal(expected, obtained),
    "Expected and obtained are different"
  )
})

test_that("normalize load absolute", {
  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  testdata$temperature <- serie
  obtained <- normalize_load(testdata, "Europe/Madrid", "absolute")
  serie <- c(rep(0.0, 12), rep(1, 12), rep(0, 12), rep(0, 12))
  values <- data.frame(t(matrix(serie, ncol = 2)))
  names(values) <- as.character(seq(0, 23))
  expected <- list(
    dates = ymd(c("2020-01-01", "2020-01-02")),
    values = as_tibble(values)
  )
  expect(
    all.equal(expected, obtained),
    "Expected and obtained are different"
  )
})
test_that("normalize load relative inputvars", {
  serie <- c(rep(10, 12), rep(20, 12), rep(10, 12), rep(5, 12))
  testdata <- create_serie(48, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  testdata$temperature <- serie
  inputVars <- c(
    "load_curves",
    "days_weekend",
    "days_of_the_week",
    "daily_cons",
    "daily_temp"
  )
  obtained <- normalize_load(testdata, "Europe/Madrid", inputVars = inputVars, holidays = "../../R/holidays.csv")
  serie <- c(rep(0.0, 12), rep(1, 12), rep(1, 12), rep(0, 12))
  values <- data.frame(t(matrix(serie, ncol = 2)))
  names(values) <- as.character(seq(0, 23))
  values$is_weekend <- c(0, 0)
  values$weekday <- c(0, 1)
  values$value <- c(1, 0)
  values$temperature <- c(1, 0)
  expected <- list(
    dates = ymd(c("2020-01-01", "2020-01-02")),
    values = as_tibble(values)
  )
  expect(
    all.equal(expected, obtained),
    "Expected and obtained are different"
  )
})

setup_clustering_test <- function()
{
  serie <- c(
      rep(c(rep(100, 12), rep(200, 12)), 5),
      rep(c(rep(90, 12), rep(190, 12)), 5),
      rep(c(rep(10, 12), rep(20, 12)), 5),
      rep(c(rep(5, 12), rep(10, 12)), 5)
  )
  testdata <- create_serie(24*20, serie, timestep = "hours")
  testdata$time <- with_tz(force_tz(testdata$time, "Europe/Madrid"), "UTC")
  testdata$temperature <- serie
  kMax <- 4 
  inputVars <- c(
    "load_curves"
  )
  return(list(testdata=testdata, kMax=kMax, inputVars=inputVars))
}

#test_that("Easy spectral clustering test", {
#  tmp <- setup_clustering_test()
#  testdata <- tmp$testdata
#  kMax <- tmp$kMax
#  inputVars <- tmp$inputVars
#
#  set.seed(123)
#  consumption_ <- c("time", "value")
#  temperature_ <- c("time", "temperature")
#  obtained <- clustering_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", kMax, inputVars, "absolute", 24)
#  expect(all.equal(
#      obtained$dailyClassification$s,
#      c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
#    "Expected and obtained are different"
#  )
#  expected <- "test_b2back/test_cluster_1.rds"
#  expected <- readRDS(file=expected)
#  expect(all.equal(obtained, expected),
#    "Expected and obtained are different"
#  )
#})
#
test_that("Easy spectral clustering and classification test", {
  tmp <- setup_clustering_test()
  testdata <- tmp$testdata
  kMax <- tmp$kMax
  inputVars <- tmp$inputVars

  set.seed(123)
  consumption_ <- c("time", "value")
  temperature_ <- c("time", "temperature")
  clustering <- clustering_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", kMax, inputVars, "absolute", 24)
  obtained <- classification_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", clustering, "classificationModel")
  expected <-"test_b2back/test_classification_1.rds"
  expected <- readRDS(file=expected)
  expect(all.equal(obtained, expected),
    "Expected and obtained are different"
  )
})

test_that("Easy spectral clustering test via wrapper", {
  tmp <- setup_clustering_test()
  testdata <- tmp$testdata
  kMax <- tmp$kMax
  inputVars <- tmp$inputVars

  set.seed(123)
  consumption_ <- c("time", "value")
  temperature_ <- c("time", "temperature")
  obtained <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=FALSE, kMax, inputVars, "absolute", 24)
  expect(all.equal(
      obtained$dailyClassification$s,
      c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    "Expected and obtained are different"
  )
  expected <- "test_b2back/test_cluster_1.rds"
  expected <- readRDS(file=expected)
  expect(all.equal(obtained, expected),
    "Expected and obtained are different"
  )
})

test_that("Easy spectral clustering and classification test via wrapper", {
  tmp <- setup_clustering_test()
  testdata <- tmp$testdata
  kMax <- tmp$kMax
  inputVars <- tmp$inputVars

  set.seed(123)
  consumption_ <- c("time", "value")
  temperature_ <- c("time", "temperature")
  clustering <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=FALSE, kMax, inputVars, "absolute", 24)
  obtained <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=TRUE, clustering, "classificationModel")
  expected <-"test_b2back/test_classification_1.rds"
  expected <- readRDS(file=expected)
  expect(all.equal(obtained, expected),
    "Expected and obtained are different"
  )
})

test_that("Easy spectral clustering and classification test via wrapper and serialization", {
  tmp <- setup_clustering_test()
  testdata <- tmp$testdata
  kMax <- tmp$kMax
  inputVars <- tmp$inputVars

  set.seed(123)
  consumption_ <- c("time", "value")
  temperature_ <- c("time", "temperature")
  clustering <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=FALSE, kMax, inputVars, "absolute", 24)

  expected <-"test_b2back/test_classification_1.rds"
  expected <- readRDS(file=expected)

  filename <- tempfile("test-no-serialize")
  save(clustering, filename, serialize=FALSE)
  clustering <- load(filename, serialize=FALSE)
  obtained <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=TRUE, clustering, "classificationModel")
  expect(all.equal(obtained, expected),
    "Expected and obtained are different"
  )

  filename <- tempfile("test-serialize")
  save(clustering, filename, serialize=TRUE)
  clustering_ <- load(filename, serialize=TRUE)

  obtained <- similar_dlc(testdata[, consumption_], testdata[, temperature_], "Europe/Madrid", predictionMode=TRUE, clustering, "classificationModel")
  expect(all.equal(obtained, expected),
    "Expected and obtained are different"
  )
})
