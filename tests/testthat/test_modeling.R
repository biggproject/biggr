# Sys.setlocale("LC_ALL", "en_US.UTF-8")
#
# library(data.table)
# library(ggplot2)
# library(gridExtra)
# library(plotly)
# library(padr)
# library(htmlwidgets)
# library(carrier)
# library(biggr)
# library(fs)
#
# setup_test <- function() {
#  data(biggr)
#  df <- electricity5 %>% calendar_components(localTimeZone = "Europe/Madrid")
#  df <- cbind(df,
#    do.call(
#      cbind,
#      oce::sunAngle(df$time,latitude=41.5,longitude=1)
#    )[,c("azimuth","altitude")]
#  )
#  df <- df %>% filter(!duplicated(df$time))
#
#  # Clustering the daily load curves
#  clust <- clustering_dlc(
#    data = df,
#    consumptionFeature = "Qe",
#    outdoorTemperatureFeature = "temperature",
#    localTimeZone = "Europe/Madrid",
#    kMax = 4,
#    inputVars = c("loadCurves","dailyConsumption"),
#    loadCurveTransformation = "absolute",
#    nDayParts = 24
#  )
#  if("s" %in% colnames(df))
#    df <- df %>% select(-s)
#  df <- df %>% left_join(clust$dailyClassification)
#  df <- df[!is.na(df$s),] # Once the classification procedure works, no need for this.
#
#  # Filter days which might be outliers in the consumption series
#  df <- df %>%
#    select(!(contains("outliers") |
#      contains("upperPredCalendarModel") |
#      contains("lowerPredCalendarModel"))) %>%
#    left_join(
#      detect_ts_calendar_model_outliers(
#        data = df,
#        localTimeColumn="localtime",
#        valueColumn="Qe",
#        window = "season",
#        outputPredictors=T
#      ),
#    by = "localtime"
#  )
#
#  return(df)
# }
#
# test_that("PenalisedLM b2back test. Using weekday/weekend as daily load curves groups", {
#  expected <- "test_b2back/test_penalisedlm_modeling_weekday.rds"
#
#  df <- setup_test()
#
#  mod <- train(
#    Qe ~ hour + th + tc + wh + wc + isolh + isolc,
#    data=df[df$outliers==F,],
#    method = PenalisedLM(
#     data.frame(
#       parameter = c('nhar','tbalh','tbalc','alpha'),
#       class = c('integer','float','float','float'))
#    ),
#    trControl = trainControl(
#      method = "timeslice",
#      initialWindow = ceiling(nrow(df)*0.5),
#      horizon = ceiling(nrow(df)*0.1),
#      skip = ceiling(nrow(df)*0.1)-1,
#      fixedWindow = T),
#    tuneGrid = expand.grid(
#      "nhar"=c(5),
#      "tbalh"=c(16),
#      "tbalc"=c(24),
#      "alpha"=c(0.1)
#    ),
#    transformationSentences = list(
#     "hour"=c("fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
#              "isWeekend"),
#     "tlpf"="lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
#              outputFeaturesNames='temperatureLpf')",
#     "th"=c("degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalh,
#              mode='heating',outputFeaturesName='heating',inplace=F)",
#            "hourBy3",
#            "isWeekend"
#            ),
#     "tc"=c("degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalc,
#              mode='cooling',outputFeaturesName='cooling',inplace=F)",
#            "hourBy3",
#            "isWeekend"
#           ),
#     "wh"=c("heating",
#            "hourBy3",
#            "windSpeed",
#            "isWeekend"),
#     "wc"=c("cooling",
#            "hourBy3",
#            "windSpeed",
#            "isWeekend"),
#     "GHIh"="vectorial_transformation(ifelse(heating>0,-GHI,0),outputFeatureName='GHIh')",
#     "GHIc"="vectorial_transformation(ifelse(cooling>0,GHI,0),outputFeatureName='GHIc')",
#     "isolh"=c("GHIh",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "isWeekend"),
#     "isolc"=c("GHIc",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "isWeekend")
#    ),
#    forcePositiveTerms = c("th","tc","wh","wc","isolh","isolc")
#  )
#
#  # Generate the predictor object with the trained model
#  predictor <- crate(function(x,forceGlobalInputFeatures=NULL){
#    biggr::predict.train(
#      object = !!mod,
#      newdata = x,
#      forceGlobalInputFeatures = forceGlobalInputFeatures
#    )
#  })
#
#  # Predict with the validation set
#  df_for_pred <- df
#  df_for_pred$Qe_pred <- predictor(df_for_pred)
#
#  obtained <- df_for_pred
#  expected <- readRDS(expected)
#
#  expect(
#    obtained == expected,
#    "Expected and obtained are different"
#  )
# })
#
# test_that("PenalisedLM b2back test. Using the clustering result of daily load curves", {
#  expected <- "test_b2back/test_penalisedlm_modeling_clustering.rds"
#
#  df <- setup_test()
#
#  mod <- train(
#    Qe ~ hour + th + tc + wh + wc + isolh + isolc,
#    data=df[df$outliers==F,],
#    method = PenalisedLM(
#     data.frame(parameter = c('nhar','tbalh','tbalc','alpha'),
#                class = c('integer','float','float','float'))
#    ),
#    trControl = trainControl(
#      method = "timeslice",
#      initialWindow = ceiling(nrow(df)*0.5),
#      horizon = ceiling(nrow(df)*0.1),
#      skip = ceiling(nrow(df)*0.1)-1,
#      fixedWindow = T),
#    tuneGrid = expand.grid(
#      "nhar"=c(5),
#      "tbalh"=c(16),
#      "tbalc"=c(24),
#      "alpha"=c(0.1)
#      ),
#    transformationSentences = list(
#     "hour"=c("fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
#              "s"),
#     "tlpf"="lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
#              outputFeaturesNames='temperatureLpf')",
#     "th"=c("degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalh,
#              mode='heating',outputFeaturesName='heating',inplace=F)",
#            "hourBy3",
#            "s"
#            ),
#     "tc"=c("degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalc,
#              mode='cooling',outputFeaturesName='cooling',inplace=F)",
#            "hourBy3",
#            "s"
#           ),
#     "wh"=c("heating",
#            "hourBy3",
#            "windSpeed",
#            "s"),
#     "wc"=c("cooling",
#            "hourBy3",
#            "windSpeed",
#            "s"),
#     "GHIh"="vectorial_transformation(ifelse(heating>0,-GHI,0),outputFeatureName='GHIh')",
#     "GHIc"="vectorial_transformation(ifelse(cooling>0,GHI,0),outputFeatureName='GHIc')",
#     "isolh"=c("GHIh",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "s"),
#     "isolc"=c("GHIc",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "s")
#    ),
#    forcePositiveTerms = c("th","tc","wh","wc","isolh","isolc")
#  )
#
#  # Generate the predictor object with the trained model
#  predictor <- crate(function(x,forceGlobalInputFeatures=NULL){
#    biggr::predict.train(
#      object = !!mod,
#      newdata = x,
#      forceGlobalInputFeatures = forceGlobalInputFeatures
#    )
#  })
#
#  # Predict with the validation set
#  df_for_pred <- df
#  df_for_pred$Qe_pred <- predictor(df_for_pred)
#
#  obtained <- df_for_pred
#  expected <- readRDS(expected)
#
#  expect(
#    obtained == expected,
#    "Expected and obtained are different"
#  )
# })
#
# test_that("RLS b2back test. Adjust a time-varying regression model", {
#  expected <- "test_b2back/test_rls_modeling_timevarying.rds"
#
#  df <- setup_test()
#
#  mod <- train(
#    Qe ~ hour + th + tc ,
#    data=df[df$outliers==F,],
#    method = RLS(
#     data.frame(
#        parameter = c('nhar','tbalh','tbalc','alpha','lambda'),
#        class = c('integer','float','float','float','float'))
#    ),
#    trControl = trainControl(
#      method = "timeslice", initialWindow = ceiling(nrow(df)*0.90),
#      horizon = ceiling(nrow(df)*0.02), skip = ceiling(nrow(df)*0.02)-1,
#      fixedWindow = T),
#    tuneGrid = expand.grid(
#      "nhar"=c(12),
#      "tbalh"=c(14),
#      "tbalc"=c(24),
#      "alpha"=c(0.2),
#      "lambda"=c(
#        get_lpf_smoothing_time_scale(data.frame("time"=df$time),0.75*24*365)
#      )),
#    transformationSentences = list(
#     "hour"=c("fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
#              "isWeekend"),
#     "temperature"="
#        vectorial_transformation(
#          na.locf(
#            na.locf(
#              na.approx(temperature,na.rm = F),
#              fromLast = T,na.rm = T
#            ),
#            na.rm=T),
#        outputFeatureName='temperature')",
#     "tlpf"="lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
#              outputFeaturesNames='temperatureLpf')",
#     "heatingLpf" = "degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalh,
#              mode='heating',outputFeaturesName='heatingLpf',inplace=F)",
#     "coolingLpf" = "degree_raw(...,featuresName='temperatureLpf',baseTemperature=param$tbalc,
#              mode='cooling',outputFeaturesName='coolingLpf',inplace=F)",
#     "heatingLpf2"="vectorial_transformation(heatingLpf^2,outputFeatureName='heatingLpf2')",
#     "coolingLpf2"="vectorial_transformation(coolingLpf^2,outputFeatureName='coolingLpf2')",
#     "heatingLpfBool"="vectorial_transformation(ifelse(heatingLpf>0,1,0),outputFeatureName='heatingLpfBool')",
#     "coolingLpfBool"="vectorial_transformation(ifelse(coolingLpf>0,1,0),outputFeatureName='coolingLpfBool')",
#     "th"=c("heatingLpf",
#            "fs_components(...,featuresName='hour',nHarmonics=5,inplace=F)",
#            "isWeekend"
#            ),
#     "tc"=c("coolingLpf",
#            "fs_components(...,featuresName='hour',nHarmonics=5,inplace=F)",
#            "isWeekend"
#           ),
#     "thint"=c("heatingLpfBool",
#            "fs_components(...,featuresName='hour',nHarmonics=5,inplace=F)",
#            "isWeekend"
#            ),
#     "tcint"=c("coolingLpfBool",
#            "fs_components(...,featuresName='hour',nHarmonics=5,inplace=F)",
#            "isWeekend"
#           ),
#     "heating" = "degree_raw(...,featuresName='temperature',baseTemperature=param$tbalh,
#              mode='heating',outputFeaturesName='heating',inplace=F)",
#     "cooling" = "degree_raw(...,featuresName='temperature',baseTemperature=param$tbalc,
#              mode='cooling',outputFeaturesName='cooling',inplace=F)",
#     "tempRadiative"="vectorial_transformation((temperature+273.15)^4,outputFeatureName='tempRadiative')",
#     "radc"=c("tempRadiative",
#            "coolingLpfBool",
#            "fs_components(...,featuresName='hour',nHarmonics=3,inplace=F)",
#            "isWeekend"
#            ),
#     "radh"=c("tempRadiative",
#            "heatingLpfBool",
#            "fs_components(...,featuresName='hour',nHarmonics=3,inplace=F)",
#            "isWeekend"
#            ),
#     "wh"=c("heating",
#            "fs_components(...,featuresName='hour',nHarmonics=3,inplace=F)",
#            "windSpeed",
#            "isWeekend"),
#     "wc"=c("cooling",
#            "fs_components(...,featuresName='hour',nHarmonics=3,inplace=F)",
#            "windSpeed",
#            "isWeekend"),
#     "GHIh"="vectorial_transformation(ifelse(heating>0 & !is.na(heating),-GHI,0),outputFeatureName='GHIh')",
#     "GHIc"="vectorial_transformation(ifelse(cooling>0 & !is.na(cooling),GHI,0),outputFeatureName='GHIc')",
#     "isolh"=c("GHIh",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "isWeekend"),
#     "isolc"=c("GHIc",
#              "fs_components(...,featuresName='azimuth',nHarmonics=3,inplace=F)",
#              "isWeekend"),
#     "light"=c("GHI",
#               "isWeekend"
#               )
#    )
#  )
#
#  # Generate the predictor object with the trained model
#  predictor <- crate(
#    function(x, forceGlobalInputFeatures = NULL, model_horizon_in_hours=1,
#      model_window="%Y-%m-%d", model_selection="rmse"){
#    biggr::predict.train(
#      object = !!mod,
#      newdata = x,
#      forceGlobalInputFeatures = forceGlobalInputFeatures,
#      model_horizon_in_hours = model_horizon_in_hours,
#      model_window = model_window,
#      model_selection = model_selection
#    )
#  })
#
#  # Predict with the validation set
#  df_for_pred <- df[df$time>=as.POSIXct("2018-01-01 00:00:00"),]
#  df_for_pred <- df_for_pred[order(df_for_pred$time),]
#  df_for_pred$Qe_pred <- predictor(df_for_pred)
#
#  obtained <- df_for_pred
#  expected <- readRDS(expected)
#
#  expect(
#    obtained == expected,
#    "Expected and obtained are different"
#  )
# })
