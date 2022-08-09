## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(biggr)
library(ggplot2)
library(lubridate)
data(biggr)


## ---- echo=FALSE--------------------------------------------------------------
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


## -----------------------------------------------------------------------------
# Secondly time step
testdata <- readdata("../tests/testthat/test_data/test_ts_secondly.csv")
head(testdata)
detect_time_step(testdata)


## -----------------------------------------------------------------------------
# Minutely time step
testdata <- readdata("../tests/testthat/test_data/test_ts_minutely.csv")
head(testdata)
detect_time_step(testdata)


## -----------------------------------------------------------------------------
# Hourly time step
testdata <- readdata("../tests/testthat/test_data/test_ts_hourly.csv")
head(testdata)
detect_time_step(testdata)


## -----------------------------------------------------------------------------
# Daily time step
testdata <- readdata("../tests/testthat/test_data/test_ts_daily.csv")
head(testdata)
detect_time_step(testdata)

## ---- echo=FALSE--------------------------------------------------------------
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


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(time=ymd_hms(time)) %>%
  rename(c("value"="Qe")) %>%
  select(time, value) %>%
  head(150)
serie$outlier <- detect_ts_min_max_outliers(serie, min = 0, max = 180)

serie %>% ggplot(aes(x=time, y=value, colour=outlier)) + geom_point()


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(time=ymd_hms(time)) %>%
  rename(c("value"="Qe")) %>%
  select(time, value) %>%
  head(150)
serie$outlier <- detect_ts_zscore_outliers(serie, zScoreThreshold=2)

serie %>% ggplot(aes(x=time, y=value, colour=outlier)) + geom_point()


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(time=ymd_hms(time)) %>%
  rename(c("value"="Qe")) %>%
  select(time, value) %>%
  head(150)
serie$value[seq(125, 130)] <- 300
outliers <- detect_ts_calendar_model_outliers(
    serie,
    localTimeColumn = "time",
    calendarFeatures = c("H"),
    autoDetectProfiled = FALSE,
    valueColumn = "value"
  )
serie <- serie %>% left_join(outliers, by="time")
serie %>% ggplot(aes(x=time, y=value, colour=outliers)) + geom_point()


## -----------------------------------------------------------------------------
test_fill_ts_na <- function(methodFillNA)
{
  serie <- electricity5 %>%
    mutate(time=ymd_hms(time),) %>%
    rename(c("value"="Qe")) %>%
    select(time, value) %>%
    head(100)
  missing <- seq(10,15)
  serie$value[missing] <- NA
  outliersMinMax <- rep(0, dim(serie)[1])
  outliersZScore <- rep(0, dim(serie)[1])
  outliersCalendarModel <- rep(0, dim(serie)[1])
  return(fill_ts_na(
      serie, outliersMinMax, outliersZScore,
      outliersCalendarModel, methodFillNA,
  ) %>% mutate(
    imputation=ifelse(row_number() %in% missing, TRUE, FALSE))
  )
}


## ---- fig.width=7,fig.height=4------------------------------------------------
newserie <- test_fill_ts_na("linearInterpolation")

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=value, colour=imputation)) + geom_point() + ggtitle("post")


## ---- fig.width=7,fig.height=4------------------------------------------------
newserie <- test_fill_ts_na("forward")

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=value, colour=imputation)) + geom_point() + ggtitle("post")


## ---- fig.width=7,fig.height=4------------------------------------------------
newserie <- test_fill_ts_na("backward")

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=value, colour=imputation)) + geom_point() + ggtitle("post")


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(time=ymd_hms(time),) %>%
  mutate(isReal=TRUE) %>%
  rename(c("value"="Qe")) %>%
  select(time, value, isReal) %>%
  head(150)

newserie <- align_time_grid(
  serie,
  outputFrequency = "P1D",
  aggregationFunction = "SUM"
)

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=SUM,)) + geom_point() + ggtitle("post")

