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


## -----------------------------------------------------------------------------
# Get time serie calendar properties
start <- ymd_hms("2020-07-01 00:00:00")
testdata <- create_serie(5, rep(1, 5), timestep = "hours", start = start)
calendar_components(testdata, "Europe/Madrid")


## -----------------------------------------------------------------------------
start <- ymd_hms("2020-12-27 00:00:00")
featuresName <- c("temperature")
testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days",
  featuresName = featuresName)
outputFeaturesName <- c("cooling")
baseTemperature <- 25
newtestdata <- degree_raw(testdata, featuresName, baseTemperature,
    outputFeaturesName, mode = "cooling")

print(testdata)
print(newtestdata)


# -----------------------------------------------------------------------------
start <- ymd_hms("2020-12-27 00:00:00")
featuresName <- c("temperature")
testdata <- create_serie(7, c(26, 27, 28, 29, 30, 31, 32), timestep = "days", featuresName = featuresName)
outputFeaturesName <- c("cooling")
baseTemperature <- 25
newtestdata <- degree_days(testdata, featuresName, "Europe/Madrid",
    baseTemperature, outputFeaturesName, mode = "cooling", outputFrequency = "P1D")

print(testdata)
print(newtestdata)


## -----------------------------------------------------------------------------
testdata <- data.frame(
  value1 = c(0, 0, 25, 25, 100, 100),
  value2 = c(0, 0, 50, 50, 100, 100)
)
newtestdata <- normalise_range(testdata, lower = 0, upper = 1)

print(testdata)
print(newtestdata)


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(
    time=ymd_hms(time)
  ) %>%
  rename(c("value"="Qe")) %>%
  select(time, value) %>%
  head(24*6)
newserie <- normalise_daily(serie, localTimeZone = "Europe/Madrid")

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("post")


## ---- fig.width=7,fig.height=4------------------------------------------------
serie <- electricity5 %>%
  mutate(
    time=ymd_hms(time)
  ) %>%
  rename(c("value"="Qe")) %>%
  select(time, value) %>%
  head(24*6)
newvalues <- normalise_zscore(serie$value)
newserie <- as.data.frame(cbind(serie$time, newvalues)) 
colnames(newserie) <- c("time", "value")

serie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("ante")
newserie %>% ggplot(aes(x=time, y=value)) + geom_point() + ggtitle("post")
