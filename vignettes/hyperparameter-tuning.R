library(data.table)
library(ggplot2)
library(gridExtra)
library(plotly)
library(padr)
library(htmlwidgets)
library(carrier)
library(biggr)
library(fs)
data(biggr)

df <- electricity3 %>% calendar_components(localTimeZone = "Europe/Madrid")
df <- cbind(df, do.call(cbind, oce::sunAngle(df$time, latitude = 41.5, longitude = 1))[, c("azimuth", "altitude")])
df <- df %>%
  filter(!duplicated(df$time)) %>%
  select(!(
    contains("outliers") |
      contains("upperPredCalendarModel") |
      contains("lowerPredCalendarModel")
  )) %>%
  left_join(
    detect_ts_calendar_model_outliers(
      data = df,
      localTimeColumn = "localtime",
      valueColumn = "Qe",
      window = "season",
      outputPredictors = T
    ),
    by = "localtime"
  )

features <- list(
  "nhar" = list(
    min = 5,
    max = 6,
    nlevels = 1,
    datatype = "integer"
  ),
  "tbalh" = list(
    min = 16,
    max = 17,
    nlevels = 1,
    datatype = "float"
  ),
  "tbalc" = list(
    min = 24,
    max = 25,
    nlevels = 1,
    datatype = "float"
  ),
  "alpha" = list(
    min = 0.1,
    max = 0.2,
    nlevels = 1,
    datatype = "float"
  )
)

# suggestions <- decodeBinFromValue(
#  values = list(5, 16, 24, 0.1),
#  features = features
# )
suggestions <- list(5, 16, 24, 0.1)

opt_function <- function(X, df, ...) {
  mod <- train(
    Qe ~ hour + th + tc + wh + wc + isolh + isolc,
    data = df[df$outliers == F, ],
    method = PenalisedLM(
      data.frame(
        parameter = c("nhar", "tbalh", "tbalc", "alpha"),
        class = c("integer", "float", "float", "float")
      )
    ),
    trControl = trainControl(
      method = "timeslice",
      initialWindow = ceiling(nrow(df) * 0.5),
      horizon = ceiling(nrow(df) * 0.1),
      skip = ceiling(nrow(df) * 0.1) - 1,
      fixedWindow = T
    ),
    tuneGrid = expand.grid(
      "nhar" = c(X$nhar),
      "tbalh" = c(X$tbalh),
      "tbalc" = c(X$tbalc),
      "alpha" = c(X$alpha)
    ),
    transformationSentences = list(
      "hour" = c(
        "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
        "isWeekend"
      ),
      "tlpf" = "lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
              outputFeaturesNames='temperatureLpf')",
      "th" = c(
        "degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalh,
              mode='heating',outputFeaturesName='heating',inplace=F)",
        "hourBy3",
        "isWeekend"
      ),
      "tc" = c(
        "degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalc,
              mode='cooling',outputFeaturesName='cooling',inplace=F)",
        "hourBy3",
        "isWeekend"
      ),
      "wh" = c(
        "heating",
        "hourBy3",
        "windSpeed",
        "isWeekend"
      ),
      "wc" = c(
        "cooling",
        "hourBy3",
        "windSpeed",
        "isWeekend"
      ),
      "GHIh" = "vectorial_transformation(ifelse(heating>0,-GHI,0),outputFeatureName='GHIh')",
      "GHIc" = "vectorial_transformation(ifelse(cooling>0,GHI,0),outputFeatureName='GHIc')",
      "isolh" = c(
        "GHIh",
        "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
        "isWeekend"
      ),
      "isolc" = c(
        "GHIc",
        "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
        "isWeekend"
      )
    ),
    forcePositiveTerms = c("th", "tc", "wh", "wc", "isolh", "isolc")
  )
  predictor <- crate(function(x, forceGlobalInputFeatures = NULL) {
    biggr::predict.train(
      object = !!mod,
      newdata = x,
      forceGlobalInputFeatures = forceGlobalInputFeatures
    )
  })
  expected <- df$Qe
  obtained <- predictor(df)
  RMSE(expected, obtained, na.rm = T)
}

best_hyperparameters <- hyperparameters_tuning(
  opt_criteria = "minimise",
  opt_function = opt_function,
  features = features,
  suggestions = suggestions,
  maxiter = 1,
  df = df
)
best_hyperparameters
