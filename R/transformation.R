#' This function shift in time a set of features in order to be used in
#' the training and prediction of the models. It is an important step
#' for the multi-step prediction of Autoregressive models, where the estimated
#' output is directly used in the subsequent predictions.
#'
#' @param data <timeSeries> Containing the multiple series to transform.
#' Optionally, other variables that are not declared in featuresNames can be
#' bypassed to the output
#' @param maxLag <integer> Describing the maximum lags to be considered. One
#' feature will be generated for each lag
#' @param featuresNames <list> selecting the series to transform.
#' @param predictionStep <integer> informing of the timestep considered in the
#' prediction. Only used in prediction mode, when training it doesn't need to
#' be described
#' @param forceGlobalInputFeatures <list> List of features to be replaced by
#' forceInitInputFeatures values. Ignore original values and use
#' forceInitInputFeatures values
#' @param forceInitInputFeatures <series> Series to be used instead of original
#' data. Features to be replaced must be list in forceGlobalInputFeatures param
#' @param forceInitOutputFeatures <dict>
#' @param fillInitNAs <boolean> indicating if the unknown lags should be filled
#' with their last known value.
#' @return data <timeSeries> containing the same initial information of the data
#' input argument, plus the lagged components as new columns.
lag_components <- function(data, maxLag, featuresNames=NULL, predictionStep=NULL, 
                           forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL, 
                           forceInitOutputFeatures=NULL, fillInitNAs=F){
  if(is.null(featuresNames)){
    if(ncol(data)>2){
      stop("Data do not contain only one series. Please, use the featuresNames 
           argument to select the features to transform.")
    }
    if(!("time" %in% colnames(data))){
      stop("No time column has been found.")
    }
    featuresNames <- colnames(data)[colnames(data)!="time"]
  }
  # Change the inputs if are specified in forceGlobalInputFeatures
  if (!is.null(forceGlobalInputFeatures)){
    for (f in names(forceGlobalInputFeatures)){
      if(!(length(forceGlobalInputFeatures[[f]])==1 || 
           length(forceGlobalInputFeatures[[f]])==nrow(data))){
        stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of data argument (%s).",f, nrow(data)))
      }
      data[,f] <- forceGlobalInputFeatures[[f]]
    }
  }
  # Calculate the lag components in training mode
  if (maxLag>0) {
    if (is.null(predictionStep)) {
      for (f in featuresNames){
        if (f %in% colnames(data)) {
          for (l in 1:maxLag) {
            data[, paste0(f,"_l",as.character(l))] <- 
              if(is.numeric(data[,f])){
                tryCatch(zoo::na.fill(shift(data[,f],l),"extend"),
                  error=function(e){rep(unique(data[is.finite(data[,f]),f])[1],
                                        nrow(data))})
              } else {
                shift(data[,f],l)
              }
            if(fillInitNAs){
              data[1:l, paste0(f,"_l",as.character(l))] <- data[l+1, paste0(f,"_l",as.character(l))]
            }
          }
        } else {
          for (l in 1:maxLag){
            data[, paste0(f,"_l",as.character(l))] <- NA
          }
        }
        if(!is.null(forceInitOutputFeatures)){
          if(f %in% names(forceInitOutputFeatures)){
            initItem <- forceInitOutputFeatures[[f]]
            initItem <- c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
            for(i in 1:maxLag){
              data[i,mapply(function(l){paste0(f,"_l",as.character(l))},i:maxLag)] <- na.omit(lag(rev(initItem),i-1))
            }
          }
        }
        if(!is.null(forceInitInputFeatures)){
          if(f %in% names(forceInitInputFeatures)){
            initItem <- forceInitInputFeatures[[f]]
            initItem <- c(rep(initItem[1],maxLag-length(initItem)),initItem)
            for(i in 1:maxLag){
              data[i,mapply(function(l){paste0(f,"_l",as.character(l))},i:maxLag)] <- na.omit(lag(rev(initItem),i-1))
            }
          }
        }
      }
      # Calculate the lag components in prediction mode
    } else {
      for (f in featuresNames){
        # Initialise the variables Input and Output when predictionStep==0
        if(!is.null(forceInitOutputFeatures) && predictionStep==0){
          if(f %in% names(forceInitOutputFeatures)){
            initItem <- forceInitOutputFeatures[[f]]
            initItem <- c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
            data[1,mapply(function(l){paste0(f,"_l",as.character(l))},1:maxLag)] <- rev(initItem)
          }
        }
        if(!is.null(forceInitInputFeatures) && predictionStep==0){
          if(f %in% names(forceInitInputFeatures)){
            initItem <- forceInitInputFeatures[[f]]
            initItem <- c(rep(initItem[1],maxLag-length(initItem)),initItem)
            data[1,mapply(function(l){paste0(f,"_l",as.character(l))},1:maxLag)] <- rev(initItem)
          }
        }
        # Generate the lag component for the next prediction 
        if (predictionStep>0 && nrow(data) >= (predictionStep+1)){
          if (maxLag>1){
            data[predictionStep+1,mapply(function(l){paste0(f,"_l",as.character(l))},1:maxLag)] <- 
              data[predictionStep,c(f,mapply(function(l){paste0(f,"_l",as.character(l))},1:(maxLag-1)))]
          } else {
            data[predictionStep+1,paste0(f,"_l",as.character(1))] <- 
              data[predictionStep,f]
          }
        }
      }
    }
  }
  return(data)
}

#' This function computes the first-order low pass filter for smoothing
#' a time series
#'
#' @param data <timeSeries> containing the series to transform. Optionally,
#' other variables that are not declared in featuresNames can be bypassed to
#' the output
#' @param featuresNames <string> selecting the series to transform
#' @param smoothingTimeScaleParameter <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.
#' @param outputFeaturesName <string> giving the column name used as output of
#' the transformation.By default, suffix "_lpf" is added to featuresNames
#' @param inplace: <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @param autoUnbox: <boolean> indicating if the output timeSeries should be
#' unboxed of the resultant data.frame, obtaining a numeric vector. Only is
#' usable when inplace is True.
#' @return data <timeSeries> containing the same initial information of the
#' data input argument, but updating the featuresNames variables to the
#' low-pass filtered version
lpf_ts <- function (data, featuresNames, smoothingTimeScaleParameter, outputFeaturesNames=NULL, inplace=T, autoUnbox=F){
  lpf <- function(x, alpha) {
    y <- numeric(length(x))
    for (i in 1:length(x)) {
      if (is.na(y[i - 1]) || i == 1) {
        y[i] <- (1 - alpha) * x[i]
      }
      else {
        y[i] <- (alpha) * y[i - 1] + (1 - alpha) * x[i]
      }
    }
    return(y)
  }
  lpfSeries <- do.call(cbind,
                       setNames(
                         lapply(
                           setNames(data.frame(data[,featuresNames]),featuresNames),
                           function(x){lpf(x,smoothingTimeScaleParameter)}
                         ),
                         if(is.null(outputFeaturesNames)) {
                           paste0(featuresNames,"_lpf")
                         } else {
                           outputFeaturesNames
                         }
                       ))
  if(inplace==T){
    return(cbind(data,lpfSeries))
  } else {
    if(ncol(lpfSeries)==1 && autoUnbox==T){
      return(as.numeric(lpfSeries))
    } else {
      return(as.data.frame(lpfSeries))
    }
  }
}

#' Physical transformation of the smoothing time scale parameter to consider
#' no affectance to the output variable after a known number of hours
#'
#' @param data <timeSeries> to filter
#' @param timeConstantInHours <float> It means the number of hours that are
#' affected by a certain change in the input time series (data argument).
#' For instance, when outdoor temperature is considered, this time constant
#' in hours correspond to the thermal inertia of the building.
#' @return smoothingTimeScaleParameter <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.
get_lpf_smoothing_time_scale <- function (data, timeConstantInHours) {
  timestep <- detect_time_step(data)
  return( (exp(1)^(-(3600/as.numeric(lubridate::as.period(timestep)))/
  ((2 * pi) * timeConstantInHours/24))) )
}

#' Decompose the time in date, day of the year, day of the week, day of the
#' weekend, working day, non-working day, season, month, hour, minute, ...
#'
#' @param data <timeSeries> containing the series to transform. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones). This argument is
#' optional, by default no transformation to local time zone is done.
#' @param holidays <list> of dates classified as holidays
#' @param inplace <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @return data <timeSeries> containing the same initial information of data
#' input argument, plus the calendar components as new columns.
calendar_components <- function (data, localTimeZone = NULL, holidays = c(), inplace=T){
  getSeason <- function(dates){
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d")
    SE <- as.Date("2012-3-15", format = "%Y-%m-%d")
    SS <- as.Date("2012-6-15", format = "%Y-%m-%d")
    FE <- as.Date("2012-9-15", format = "%Y-%m-%d")
    d <- as.Date(strftime(dates, format = "2012-%m-%d"))
    ifelse(d >= WS | d < SE, "Winter", 
           ifelse(d >= SE & d < SS, "Spring",
                  ifelse(d >= SS & d < FE, "Summer", "Fall")))
  }
  result <- data %>% summarise(
    localtime = with_tz(time, localTimeZone), 
    date = lubridate::date(localtime), 
    weekday = as.factor(lubridate::wday(localtime, week_start = getOption("lubridate.week.start", 1))),
    weekdayNum = as.numeric(as.character(weekday)),
    dayYear = as.numeric(format(date,"%j")),
    timestamp = as.numeric(localtime),
    isWeekend = weekday %in% c(6, 7),
    isHolidays = as.factor(date %in% holidays),
    holidaysDate = (date %in% holidays),
    year = year(localtime), 
    quarter = as.factor(quarter(localtime)), 
    semester = as.factor(semester(localtime)), 
    season = as.factor(getSeason(localtime)),
    monthInt = month(localtime),
    month = as.factor(monthInt),
    day = day(localtime), 
    hour = hour(localtime),
    hourBy3 = as.factor(ceiling((hour+1)/3)),
    hourBy4 = as.factor(ceiling((hour+1)/4)),
    hourBy6 = as.factor(ceiling((hour+1)/6)),
    hourBy8 = as.factor(ceiling((hour+1)/8)),
    weekhour = as.numeric(hour + 24 * (as.numeric(weekday)-1)),
    minute = minute(localtime), 
    second = second(localtime))# %>% 
  # select(-localtime)
  if(inplace==T){
    return(cbind(data[,!(colnames(data) %in% colnames(result))],result))
  } else {
    return(result)
  }
}

#' Obtain the components of the Fourier Series, in sine-cosine form.
#' It is useful for linearising the relationship of a seasonal input
#' time series (e.g. solar azimuth, solar elevation, calendar
#' features, ...) to some output (energy consumption, indoor
#' temperatures, ...). It basically decomposes a cyclic time series
#' into a set of sine-cosine components that are used as inputs for
#' the modelling of some output, each of the components linearly depends
#' to the output.
#'
#' @param data <timeSeries> containing the series to transform.
#' This series must have a cyclic behaviour (e.g. hour of the day, day of
#' the week, solar azimuth, day of the year, ...) in order to be correctly
#' transformed. Optionally, other variables that are not declared in
#' featuresNames can be bypassed to the output.
#' @param featuresNames <list string> selecting the series to transform.
#' @param nHarmonics <integer> defines the number of harmonics considered
#' in the Fourier Series. A high number allows to model more precisely
#' the relation, but it considerably increase the cost of computation.
#' The number of harmonics is related with the number of features in
#' the output matrix
#' @param mask <boolean serie> containing the timestamps that should be
#' accounted for the transformation. The timestamps set to false will
#' consider 0's for all their related sine-cosine components. By default,
#' all elements of the time series are considered.
#' @param inplace: <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @param normMode <string> normalization method to be used in features
#' preprocessing. Supported scaling methods
#'     - divided_by_max_plus_one: Max+1 normalization method 
#'     - min_max_range: Min-max normalization method
#'     - NULL: No normalization applied
#' @return data <timeSeries> containing the same initial information of data
#' input argument, plus the sine-cosine components of the Fourier Series as new
#' columns.
fs_components <- function (data, featuresNames, nHarmonics, mask=NULL, inplace=T, normMode="divided_by_max_plus_one") {
  data_ <- data
  fs_multiple <- NULL
  for (featureName in featuresNames) {
    if (normMode=="min_max_range") {
      data_[[featureName]] <- (data_[[featureName]]-min(data_[[featureName]],na.rm=T))/(max(data_[[featureName]],na.rm=T)-min(data_[[featureName]],na.rm=T))
    } else if (normMode=="divided_by_max_plus_one") {
      data_[[featureName]] <- data_[[featureName]]/(max(data_[[featureName]],na.rm=T)+1)
    }
    if (!is.null(mask)) {
      data_[mask, featureName] <- 0
    }
    fs_tmp <- fs(data_[[featureName]], featureName, nHarmonics)
    if(!is.null(fs_multiple)) { fs_multiple <- cbind(fs_multiple, fs_tmp)} else { fs_multiple <- fs_tmp }
  }
  if(inplace==T){
    return(cbind(data,fs_multiple))
  } else {
    return(fs_multiple)    
  }
}

#' Calculate the difference between outdoor temperature and a base temperature,
#' without considering the frequency of the original data.
#'
#' @param data <timeSeries> containing the series with data. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param featuresName <string> giving the column name of the outdoor
#' temperature feature.
#' @param baseTemperature <float> describing the Balance Point Temperature (BPT)
#' used in the calculation. Below BPT in heating mode, heat would be required by
#' the building. The contrary in the case of cooling, over BPT in cooling mode.
#' @param outputFeaturesName <string> giving the column name used as output of
#' the transformation.By default, "heating" or "cooling" depending the mode
#' used in the transformation.
#' @param mode <string> describing the calculation mode, which could be
#' "cooling" or "heating". By default, "heating" is configured.
#' @param maxValue <float> differente threshold. Low pass filter threshold
#' @param baseTemperatureName <string> giving the column name of the
#' baseTemperature if it is available in data. In this case baseTemperature
#' parameter is ignored
#' @param inplace <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @return <timeSeries< of the difference between the temperature argument
#' and the selected base temperature, mantaining the original frequency of
#' temperature.
degree_raw <- function (data, featuresName, baseTemperature = 18, outputFeaturesName = NULL, 
                        mode = "heating", maxValue = NULL, baseTemperatureName = NULL, 
                        hysteresisBaseTemperature = 2, inplace=T){
  if(!is.null(baseTemperatureName)){
    baseTemperature <- data[,baseTemperatureName]
  }
  result <- setNames(data.frame((if (mode == "heating") {
    ifelse(unlist(baseTemperature - data[,featuresName])>0,
           unlist((baseTemperature + abs(hysteresisBaseTemperature)) - data[,featuresName]),
           0)
  } else {
    ifelse(unlist(data[,featuresName] - baseTemperature)>0,
           unlist((data[,featuresName]) - (baseTemperature - abs(hysteresisBaseTemperature))),
           0)
  })),if(is.null(outputFeaturesName)){mode} else {outputFeaturesName})
  if(!is.null(maxValue)){
    result[,1] <- ifelse(result[,1] > maxValue, maxValue, result[,1])
  }
  if(inplace==T){
    return(cbind(data,result))
  } else {
    return(result)
  }
}

#' Create data frame from multiple arrays
#'
#' @param series <list> List of arrays to join to single data frame
#' @param outputFeatureName <list> List of column names. One name per each
#' of the arrays provided as input
#' @return <data.frame> Data frame with input series as named columns

vectorial_transformation <- function(series, outputFeatureName="transformated"){
  return(setNames(data.frame(series),outputFeatureName))
}

#' Add new serie to current data frame
#'
#' @param data <data.frame> Original data frame to be updated
#' @param newColumn <serie> Serie appended to original data frame
#' @return <data.frame> Data frame with additional column

add_to_dataframe <- function(data, newColumn){
  return(cbind(data, newColumn))
}

#' Calculate the degree-days with a desired output frequency and considering
#' cooling or heating mode.
#'
#' @param data <timeSeries> containing the series with data. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param temperatureFeature <string> giving the column name of the outdoor
#' temperature feature.
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). This
#' argument is optional, by default no transformation to local time zone is
#' done.
#' @param baseTemperature <float> describing the Balance Point
#' Temperature (BPT) used in the calculation. Below BPT in heating mode,
#' heat would be required by the building. The contrary in the case of
#' cooling, over BPT in cooling mode
#' @param mode <string> describing the calculation mode, which could be
#' "cooling" or "heating". By default, "heating" is configured.
#' @param outputFrequency <string> The frequency used to resample the daily
#' degree days. It must be a string in ISO 8601 format representing the
#' time step. Only yearly ("Y"), monthly ("M"), daily ("D") output time
#' steps are allowed.
#' @param outputFeaturesName <string> giving the column name used as output of
#' the degreedays result. By default, "HDD" is configured
#' @param fixedOutputFeaturesName <boolean> enable fixed column name used as
#' output of the degreedays results. Otherwise baseTemperature is added 
#' as suffix in the output column name
#' @return degreeDays <timeSeries> in the outputTimeStep of the heating or
#' cooling degree days.

degree_days <- function(data, temperatureFeature, localTimeZone, baseTemperature,
                        mode = "heating", outputFrequency = "P1D", 
                        hysteresisBaseTemperature = 2, outputFeaturesName = "HDD",
                        fixedOutputFeaturesName=F) {
  temperature <- setNames(data[,c("time",temperatureFeature)],c("time","value"))
  tmp <- temperature %>%
    mutate(
      localtime = lubridate::with_tz(time, localTimeZone),
      time = lubridate::as_datetime(lubridate::date(localtime))
    ) %>%
    group_by(time) %>%
    summarize(
      value = mean(value, na.rm = TRUE)
    )
  dd_ <- do.call(cbind,lapply(FUN = function(b) {
      degree_raw(data = tmp, featuresName = "value", baseTemperature = b, mode = mode, 
                 hysteresisBaseTemperature = hysteresisBaseTemperature,
                 outputFeaturesName = if(fixedOutputFeaturesName){outputFeaturesName}else{paste0(outputFeaturesName, b)}, 
                 inplace = F)
    }, baseTemperature))
  dd_$time <- tmp$time
  
  return(
    dd_ %>%
       mutate(group = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                 week_start = getOption("lubridate.week.start", 1)
       )) %>%
       group_by(group) %>%
       summarise_at(
         .vars = vars(-time),
         .funs = function(x){sum(x, na.rm = TRUE)}
       ) %>%
       rename_at("group", function(x) "time") %>%
       mutate(time = lubridate::force_tz(time, localTimeZone))
    )
}

#' Calculate change point temperature by modeling temperature vs consumption
#' relationship using a multi-step weather dependency signature model.
#' Multi-step weather dependency signature model is based in best
#' fitting penalized regression model analysis. Regression quality analysis
#' is used to identify wether proposed model properly describes the
#' weather vs consumption relationship. Model coefficients analysis is used
#' is used to identify type and amount of weather dependency.
#'
#' @param consumptionData <timeSerie> containing consumption data serie.
#' The time zone of the datetimes must be UTC
#' @param weatherData <timeSerie> containing temperature data serie.
#' The time zone of the datetimes must be UTC
#' @param consumptionFeature <string> Column name of consumption
#' @param temperatureFeature <string> Column name of temperature
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). This
#' argument is optional, by default no transformation to local time zone is
#' done.
#' @param plot <boolean> Plot change point model describing load vs
#' temperature relationship in order to identify breakpoints
#' @return Changepoint analysis results attributes <struct>
#      - tbal. Change point temperature 
#      - heating. Heating dependency detected
#      - cooling. Cooling dependency detected

get_change_point_temperature <- function(consumptionData, weatherData, 
                                         consumptionFeature, 
                                         temperatureFeature,
                                         localTimeZone, plot = F){
  # consumptionFeature <- "Qe"
  # temperatureFeature <- "temperature"
  # weatherData <- data[,c("time",temperatureFeature)]
  # consumptionData <- data[,c("time",consumptionFeature)]
  timestepOriginalData <- detect_time_step(consumptionData$time)
  weatherData <- weatherData[,c("time",temperatureFeature)]
  colnames(weatherData) <- c("time","temperature")
  weatherData$time <- as.Date(weatherData$time, localTimeZone)
  # weatherDataD <- smartAgg(weatherData, by="time",
  #                              args=list(
  #                                function(x){mean(x,na.rm=T)},"temperature"
  #                              ))
  weatherDataD <- weatherData %>% 
    group_by(time) %>% 
    summarise(across(c(temperature), function(x){mean(x,na.rm=T)}),
              .groups = 'drop')
  
  consumptionData <- consumptionData[,c("time",consumptionFeature)]
  colnames(consumptionData) <- c("time","consumption")
  consumptionData$time <- as.Date(consumptionData$time, localTimeZone)
  # consumptionDataD <- smartAgg(consumptionData, by="time",
  #                              args=list(
  #                                function(x)sum(x,na.rm=T),"consumption"
  #                              ))
  n_timesteps <- hourly_timesteps(24,timestepOriginalData)
  consumptionDataD <- consumptionData %>% 
    group_by(time) %>% 
    summarise(across(c(consumption), function(x){mean(x,na.rm=T)*
        n_timesteps}),
              .groups = 'drop')
  
  estimate_signature <- function(par, weatherData, consumptionData, localTimeZone){
    newdata <- consumptionData %>% mutate(time=force_tz(time,localTimeZone))
    newdata <- newdata %>% 
      left_join(weatherData, by="time") %>% 
      left_join(
        degree_days(
          data = weatherData, 
          temperatureFeature = "temperature", 
          outputFrequency = "D", 
          localTimeZone = localTimeZone,
          baseTemperature = par["tbalh"], 
          mode = "heating",
          hysteresisBaseTemperature = 0,
          outputFeaturesName = "hdd",
          fixedOutputFeaturesName=T),
        by="time") %>%
      left_join(
        degree_days(
          data = weatherData, 
          temperatureFeature = "temperature", 
          outputFrequency = "D", 
          localTimeZone = localTimeZone,
          baseTemperature = par["tbalc"], 
          mode = "cooling",
          hysteresisBaseTemperature = 0,
          outputFeaturesName = "cdd",
          fixedOutputFeaturesName=T),
        by="time")
    
    newdata$heating_intercept <- ifelse(newdata$hdd>0, 1, 0)
    newdata$cooling_intercept <- ifelse(newdata$cdd>0, 1, 0)
    newdata$weekday <- as.factor(strftime(newdata$time,"%w"))
    
    # if(length(unique(newdata$weekday))>1){
    #   mod <- lm(consumption ~ 0 + weekday + hdd + cdd , newdata)
    #   x <- suppressMessages(model.matrix( ~ 0 + weekday + hdd + cdd,newdata))
    #   y <- unlist(newdata[,"consumption"])
    #   cv <- glmnet::cv.glmnet(x,y)
    #   hdd_slope <- coef(cv)["hdd",1]>0
    #   cdd_slope <- coef(cv)["cdd",1]>0
    #   newdata$fix <- 1
    #   mod <- list(
    #     "mod"= penalized(y,penalized =
    #                         if(hdd_slope && cdd_slope){
    #                           as.formula("~ 0 + hdd + cdd")
    #                         } else if (hdd_slope){as.formula("~ 0 + hdd")
    #                         } else if (cdd_slope){as.formula("~ 0 + cdd")
    #                         } else {~ fix},
    #                     unpenalized = ~ 0 + weekday, data=newdata,
    #                     positive = T,
    #                     trace = F)
    #   )
    #   mod[["yhat"]] <- mod$mod@fitted
    #   mod[["y"]] <- y
    #   mod[["temperature"]] <- newdata$temperature
    # } else {
      x <- suppressMessages(
        model.matrix(~ 1 + hdd + cdd, newdata))
      y <- unlist(newdata[,"consumption"])
      if(!(all(newdata$hdd==0) | all(newdata$cdd==0)) & nrow(newdata)>=30){
        #cv <- glmnet::cv.glmnet(x,y,relax=T)
        hdd_slope <- T#coef(cv)["hdd",1]>0
        cdd_slope <- T#coef(cv)["cdd",1]>0
      } else {
        hdd_slope <- F
        cdd_slope <- F
      }
      newdata$fix <- 1
      mod <- list(
        "mod"= penalized(y,penalized =
                           if(hdd_slope && cdd_slope){
                             as.formula("~ 0 + hdd + cdd")
                           } else if (hdd_slope){as.formula("~ 0 + hdd")
                           } else if (cdd_slope){as.formula("~ 0 + cdd")
                           } else {~ fix}, 
                         unpenalized = ~1,
                         data=newdata,
                         positive = T,# lambda1 = tail(cv$lambda,1), lambda2 = tail(cv$lambda,1),
                         trace=F)
      )
      mod[["yhat"]] <- mod$mod@fitted
      mod[["y"]] <- y
      mod[["temperature"]] <- newdata$temperature
    # }
    mod
    #a <- summary(mod)
    
    # plot(newdata$hdd,col="red", ylim=c(0,40))
    # points(newdata$cdd,col="blue")
  }
  tbals <- seq(ceiling(quantile(weatherData$temperature,0.1,na.rm=T)),
               floor(quantile(weatherData$temperature,0.9,na.rm=T)),
               by=1)
  hysteresis <- seq(1,3,by=1)
  pars <- expand.grid(tbals,hysteresis)
  pars <- data.frame(
    "tbalh"=pars$Var1-pars$Var2,
    "tbalc"=pars$Var1+pars$Var2)
  mod_results <- lapply(FUN=function(i){
    estimate_signature(par = pars[i,], weatherData = weatherDataD, 
                       consumptionData = consumptionDataD, localTimeZone)
  },1:nrow(pars))
  results <- data.frame(
    "tbalh" = pars$tbalh,
    "tbalc" = pars$tbalc,
    "cost" = mapply(function(mod){
      RMSE(mod$y, mod$yhat)
    },mod_results)
  )
  if(all(results$cost == results$cost[1])){
    if(plot){
      print(ggplot() + 
              geom_point(aes(x=weatherDataD$temperature, 
                             y=consumptionDataD$consumption)) + 
              geom_point(aes(x=mod_results[[1]]$temperature,
                             y=mod_results[[1]]$yhat),
                         col="red",size=2) +
              theme_bw() + xlab("temperature (ºC)") + ylab("consumption (kWh)"))
    }
    return(
      list(
        "tbalh" = NA,
        "tbalc" = NA,
        "heating" = F,
        "cooling" = F
      )
    )
  }
  best <- which.min(results$cost)
  if(plot){
    g <- ggplot() + 
      geom_point(aes(x=weatherDataD$temperature, 
                    y=consumptionDataD$consumption)) + 
      geom_point(aes(x=mod_results[[best]]$temperature,
                     y=mod_results[[best]]$yhat),
                 col="red",size=2) +
      theme_bw() + xlab("temperature (ºC)") + ylab("consumption (kWh)")
    if("hdd" %in% names(mod_results[[best]]$mod@penalized))
      g <- g + geom_vline(aes(xintercept=results$tbalh[best]),col="red",alpha=0.5)
    if("cdd" %in% names(mod_results[[best]]$mod@penalized))
      g <- g + geom_vline(aes(xintercept=results$tbalc[best]),col="blue",alpha=0.5)
    print(g)
  }
  # mod_results <<- mod_results
  # consumptionDataD <<- consumptionDataD
  # weatherDataD <<- weatherDataD
  # best2 <<- best
  # # best <- best2
  # results <<- results
  r2 <- tryCatch(cor(mod_results[[best]]$y,mod_results[[best]]$yhat)^2,
                 warning=function(e){0})
  
  avghdd <- mean(mod_results[[best]]$mod@penalized[grepl("hdd",names(mod_results[[best]]$mod@penalized))])
  avgcdd <- mean(mod_results[[best]]$mod@penalized[grepl("cdd",names(mod_results[[best]]$mod@penalized))])
  # Check if the hdd and cdd are higher or equal than 
  # 0.75% of average daily consumption per degree day
  heating_dep <- (if(is.finite(avghdd) && r2>=0.1){avghdd} else {0}) >= 
    mean(consumptionDataD$consumption,na.rm=T)*0.005
  cooling_dep <- (if(is.finite(avgcdd) && r2>=0.1){avgcdd} else {0}) >= 
    mean(consumptionDataD$consumption,na.rm=T)*0.005
  return(
    list(
      "tbalh" = if(heating_dep){results$tbalh[best]} else {NA},
      "tbalc" = if(cooling_dep){results$tbalc[best]} else {NA},
      "heating" = heating_dep,
      "cooling" = cooling_dep
      )
  )
}


get_change_point_temperature_v2 <- function(consumptionData, weatherData, 
                                         consumptionFeature, 
                                         temperatureFeature,
                                         consumptionGroupFeature=NULL,
                                         localTimeZone, plot = F){
  
  weatherData <- weatherData[,c("time",temperatureFeature)]
  colnames(weatherData) <- c("time","temperature")
  weatherData$time <- as.Date(weatherData$time, localTimeZone)
  weatherDataD <- weatherData %>% 
    group_by(time) %>% 
    summarise(across(c(temperature), function(x){mean(x,na.rm=T)}),
              .groups = 'drop')
  
  if(is.null(consumptionGroupFeature)){
    consumptionData <- data.frame(
      consumptionData[,c("time",consumptionFeature)],"01")
  } else {
    consumptionData <- consumptionData[,c("time",consumptionFeature,
                                          consumptionGroupFeature)]
  }
  colnames(consumptionData) <- c("time","consumption","group")
  n_timesteps <- hourly_timesteps(24,detect_time_step(consumptionData$time))
  consumptionData$time <- as.Date(consumptionData$time, localTimeZone)
  consumptionDataD <- consumptionData %>% 
    group_by(time) %>% 
    summarise(
      across(c(consumption), function(x){mean(x,na.rm=T)*n_timesteps}),
      group = first(group),
      .groups = 'drop')
  
  estimate_signature <- function(par, weatherDataD, consumptionDataD, localTimeZone){
    newdata <- consumptionDataD %>% mutate(time=force_tz(time,localTimeZone))
    newdata <- newdata %>% 
      left_join(weatherDataD, by="time") %>% 
      left_join(
        degree_days(
          data = weatherData, 
          temperatureFeature = "temperature", 
          outputFrequency = "D", 
          localTimeZone = localTimeZone,
          baseTemperature = par["tbalh"], 
          mode = "heating",
          hysteresisBaseTemperature = 0,
          outputFeaturesName = "hdd",
          fixedOutputFeaturesName=T),
        by="time") %>%
      left_join(
        degree_days(
          data = weatherData, 
          temperatureFeature = "temperature", 
          outputFrequency = "D", 
          localTimeZone = localTimeZone,
          baseTemperature = par["tbalc"], 
          mode = "cooling",
          hysteresisBaseTemperature = 0,
          outputFeaturesName = "cdd",
          fixedOutputFeaturesName=T),
        by="time")
    
    newdata$heating_intercept <- ifelse(newdata$hdd>0, 1, 0)
    newdata$cooling_intercept <- ifelse(newdata$cdd>0, 1, 0)
    newdata$weekday <- strftime(newdata$time,"%w")
    
    # if(length(unique(newdata$weekday))>1){
    #   mod <- lm(consumption ~ 0 + weekday + hdd + cdd , newdata)
    #   x <- suppressMessages(model.matrix( ~ 0 + weekday + hdd + cdd,newdata))
    #   y <- unlist(newdata[,"consumption"])
    #   cv <- glmnet::cv.glmnet(x,y)
    #   hdd_slope <- coef(cv)["hdd",1]>0
    #   cdd_slope <- coef(cv)["cdd",1]>0
    #   newdata$fix <- 1
    #   mod <- list(
    #     "mod"= penalized(y,penalized =
    #                         if(hdd_slope && cdd_slope){
    #                           as.formula("~ 0 + hdd + cdd")
    #                         } else if (hdd_slope){as.formula("~ 0 + hdd")
    #                         } else if (cdd_slope){as.formula("~ 0 + cdd")
    #                         } else {~ fix},
    #                     unpenalized = ~ 0 + weekday, data=newdata,
    #                     positive = T,
    #                     trace = F)
    #   )
    #   mod[["yhat"]] <- mod$mod@fitted
    #   mod[["y"]] <- y
    #   mod[["temperature"]] <- newdata$temperature
    # } else {
    # x <- suppressMessages(
    #   model.matrix(~ 1 + hdd + cdd, newdata))
    # y <- unlist(newdata[,"consumption"])
    hdd_slope <- (sum(newdata$hdd>0)/nrow(newdata))>0.2
    cdd_slope <- (sum(newdata$cdd>0)/nrow(newdata))>0.2
    
    group_var <- length(unique(newdata$group))>1
    if(group_var){
      #newdata$group <- as.factor(newdata$group)
      form <- if(hdd_slope && cdd_slope){
        as.formula("consumption ~ 0 + hdd:group + cdd:group + heating_intercept:group + cooling_intercept:group")
      } else if (hdd_slope) { as.formula("consumption ~ 0 + hdd:group + heating_intercept:group")
      } else if (cdd_slope) { as.formula("consumption ~ 0 + cdd:group + cooling_intercept:group")
      } else {consumption ~ group}
    } else {
      form <- if(hdd_slope && cdd_slope){
        as.formula("consumption ~ 0 + hdd + cdd")
      } else if (hdd_slope) { as.formula("consumption ~ 0 + hdd")
      } else if (cdd_slope) { as.formula("consumption ~ 0 + cdd")
      } else {consumption ~ 1}
    }
    mod <- lm(formula = form, data = newdata)
           #quantreg::rq(form, data = newdata, tau = 0.5)    # mod <- list(
    #   "mod"= penalized(
    #     response = y,
    #     penalized =
    #      form, 
    #     unpenalized = ~1,
    #     data=newdata,
    #     positive = T,# lambda1 = tail(cv$lambda,1), lambda2 = tail(cv$lambda,1),
    #     trace=F)
    # )
    if(sum(
        is.na(mod$coefficients) | (
          grepl("hdd|cdd",names(mod$coefficients)) & ( 
          mod$coefficients<0 | is.na(mod$coefficients)) ) , 
      na.rm=T) > 0 ) {
      mod$coefficients[ 
        is.na(mod$coefficients) | (
          grepl("hdd|cdd",names(mod$coefficients)) & ( 
            mod$coefficients<0 | is.na(mod$coefficients)) ) ] <- 0
    }
    mod[["yhat"]] <- predict(mod,newdata)
      #mod$mod@fitted
    mod[["y"]] <- newdata$consumption
    mod[["temperature"]] <- newdata$temperature
    # }
    mod
    #a <- summary(mod)
    
    # plot(newdata$hdd,col="red", ylim=c(0,40))
    # points(newdata$cdd,col="blue")
  }
  tbals <- seq(ceiling(quantile(weatherData$temperature,0.1,na.rm=T)),
               floor(quantile(weatherData$temperature,0.9,na.rm=T)),
               by=1)
  hysteresis <- seq(1,6,by=1)
  pars <- expand.grid(tbals,hysteresis)
  pars <- data.frame(
    "tbalh"=pars$Var1-pars$Var2,
    "tbalc"=pars$Var1+pars$Var2)
  mod_results <- lapply(FUN=function(i){
    estimate_signature(par = pars[i,], weatherDataD = weatherDataD, 
                       consumptionDataD = consumptionDataD, localTimeZone)
  },1:nrow(pars))
  results <- data.frame(
    "tbalh" = pars$tbalh,
    "tbalc" = pars$tbalc,
    "cost" = mapply(function(mod){
      RMSE(mod$y, mod$yhat)
    },mod_results)
  )
  if(all(results$cost == results$cost[1])){
    if(plot){
      print(ggplot() + 
              geom_point(aes(x=weatherDataD$temperature, 
                             y=consumptionDataD$consumption)) + 
              geom_point(aes(x=mod_results[[1]]$temperature,
                             y=mod_results[[1]]$yhat),
                         col="red",size=2) +
              theme_bw() + xlab("temperature (ºC)") + ylab("consumption (kWh)"))
    }
    return(
      setNames(data.frame(
        unique(consumptionData$group), NA, NA, F, F), 
        c(consumptionGroupFeature,"tbalh","tbalc","heating","cooling"))
    )
  }
  best <- which.min(results$cost)
  if(plot){
    g <- ggplot() + 
      geom_point(aes(x=weatherDataD$temperature, 
                     y=consumptionDataD$consumption)) + 
      geom_point(aes(x=mod_results[[best]]$temperature,
                     y=mod_results[[best]]$yhat),
                 col="red",size=2) +
      theme_bw() + xlab("temperature (ºC)") + ylab("consumption (kWh)")
    if("hdd" %in% names(mod_results[[best]]$mod))
      g <- g + geom_vline(aes(xintercept=results$tbalh[best]),col="red",alpha=0.5)
    if("cdd" %in% names(mod_results[[best]]$mod))
      g <- g + geom_vline(aes(xintercept=results$tbalc[best]),col="blue",alpha=0.5)
    print(g)
  }
  # mod_results <<- mod_results
  # consumptionDataD <<- consumptionDataD
  # weatherDataD <<- weatherDataD
  # best2 <<- best
  # # best <- best2
  # results <<- results
  r2 <- tryCatch(cor(mod_results[[best]]$y,mod_results[[best]]$yhat)^2,
                 warning=function(e){0})
  baseload <- predict(mod_results[[best]],data.frame(
    "group"=sort(unique(consumptionDataD$group)),
    "hdd"=0,"cdd"=0,"heating_intercept"=0,"cooling_intercept"=0
  ))
  avg_hdd_cdd <- data.frame(
    "group" = sort(unique(consumptionDataD$group)),
    "baseload"=baseload,
    "heating" = predict(mod_results[[best]],data.frame(
      "group"=unique(consumptionDataD$group),
      "hdd"=1, "cdd"=0, "heating_intercept"=0, "cooling_intercept"=0)) - baseload,
    "cooling" = predict(mod_results[[best]],data.frame(
      "group"=unique(consumptionDataD$group),
      "hdd"=0, "cdd"=1, "heating_intercept"=0, "cooling_intercept"=0)) - baseload)
  
  # Check if the hdd and cdd are higher or equal than 
  # 0.75% of average daily consumption per degree day
  heating_dep <- (if(is.finite(avg_hdd_cdd$heating) && r2>=0.2){avg_hdd_cdd$heating} else {0}) >= 
    baseload*0.005
  cooling_dep <- (if(is.finite(avg_hdd_cdd$cooling) && r2>=0.2){avg_hdd_cdd$cooling} else {0}) >= 
    baseload*0.005
  return(
    setNames(data.frame(
      avg_hdd_cdd$group,
      "tbalh" = ifelse(heating_dep,results$tbalh[best],NA),
      "tbalc" = ifelse(cooling_dep,results$tbalc[best],NA),
      "heating" = heating_dep,
      "cooling" = cooling_dep
    ), 
    c(if(is.null(consumptionGroupFeature)){"group"}else{consumptionGroupFeature},
      "tbalh","tbalc","heating","cooling"))
  )
}


get_local_min_from_density <- function(values,onlyPosition=NULL,minNumberValleys=1,kDensityAdjuster=3){
  d <- density(values,adjust=kDensityAdjuster,na.rm=T)
  tp <- pastecs::turnpoints(ts(d$y))
  y <- d$y[tp$tppos]
  x <- d$x[tp$tppos]
  sy <- c(0,shift(y,1)[is.finite(shift(y,1))])
  mins <- x[y-sy<=0]
  if(length(mins) < minNumberValleys) return(NULL)
  if(!is.null(onlyPosition)){
    return(mins[onlyPosition])
  } else { return(mins) }
}

### ---
### Clustering and classification of daily load curves----
### ---

#' Normalze time serie using min-max range normalization method
#'
#' @param data <data.frame> containing a set of time series to normalise
#' @param lower <float> lower value
#' @param upper <float> upper value
#' @param lowerThreshold <float> lower threshold value
#' @param upperThreshold <float> upper threshold value
#' @param scalingAttr <struct> setting lower and upper threshold in case of
#' null lowerThreshold or upperThreshold
#' - min: lowerThreshold
#' - max: upperThreshold
#' @return Normalized input data using range normalization method. Output
#' data properties is different depending on input data properties
#' If input data class is not in "matrix", "data.frame" or "tbl_df" then
#' output data provides normalization 
#' If input data class is in "matrix", "data.frame" or "tbl_df" then
#' output data provides struct with normalization in "values" attribute and
#' min-max values used in "scalingAttr" attribute
normalise_range <- function(data, lower = 0, upper = 1, lowerThreshold = NULL, 
                            upperThreshold = NULL, scalingAttr = NULL) {
  if (!(is.null(lower) & is.null(upper))) {
    if (lower > upper) stop("Lower > upper in normalisation by range is not supported")
  }
  normalise <- function(x, lower=0, upper=1, lowerThreshold=NULL, upperThreshold=NULL) {
    minValue <- if (!is.null(lowerThreshold)) {
        lowerThreshold
      } else {
        min(x, na.rm = TRUE)
      }
    maxValue <- if (!is.null(upperThreshold)) {
      upperThreshold
    } else {
      max(x, na.rm = TRUE)
    }
    if ((minValue == maxValue)) {
      result <- rep(lower, length(x))
    } else {
      result <- ((x - minValue) / (maxValue - minValue)) * (upper - lower) + lower
    }
    return( list("values"=result,"scalingAttr"=setNames(
      data.frame(c("min"=minValue,"max"=maxValue)),NULL) ) )
  }
  if(is.null(lowerThreshold) && !is.null(scalingAttr)){
    lowerThreshold <- setNames(
      as.numeric(scalingAttr["min",]),colnames(scalingAttr))
  }
  if(is.null(upperThreshold) && !is.null(scalingAttr)){
    upperThreshold <- setNames(
      as.numeric(scalingAttr["max",]),colnames(scalingAttr))
  }
  if(class(data)[1] %in% c("matrix","data.frame","tbl_df")){
    norm_list <- lapply(1:ncol(data),function(x){
      normalise(data[,x], lower, upper, lowerThreshold[colnames(data)[x]], 
                upperThreshold[colnames(data)[x]])
    })
    return( list(
      "values"=do.call(cbind,lapply(norm_list,function(x)x[["values"]])),
      "scalingAttr"=setNames(do.call(cbind,lapply(norm_list,function(x)x[["scalingAttr"]])),
                             colnames(data))
    ) )
  } else {
    return( normalise(data, lower, upper, lowerThreshold, upperThreshold) )
  }
}

#' Normalize time serie using hourly over daily relative consumption (%) normalization method
#'
#' @param data <timeSeries> containing serie to normalise
#' @param method <string> Normalization method. Supported methods
#'   relative
#' @return daily normalised timeserie
normalise_daily <- function(data, method = "relative", localTimeZone) {
  if (!(method == "relative")) stop("Method not supported")
  time <- data$time
  return(data %>%
           mutate(
             date = lubridate::date(with_tz(time, localTimeZone))
           ) %>%
           group_by(date) %>%
           summarize(value = value / sum(value)) %>%
           ungroup() %>%
           mutate(time = time) %>%
           select(time, value))
}

#' Normalize time serie using Zscore normalization method
#'
#' @param data <timeSeries> containing serie to normalise
#' @return Normalized input data using Z-score normalization method.
#' Output data properties is different depending on input data properties
#' If input data class is not in "matrix", "data.frame" or "tbl_df" then
#' output data provides struct with normalized vector in "values" attribute
#' and scaling "mean" and "sd" in "scalingAttr" attribute
#' If input data class is in "matrix", "data.frame" or "tbl_df" then
#' output data provides struct with normalization in "values" attribute and
#' scaling "mean" and "sd" in "scalingAttr" attribute
normalise_zscore <- function(data, scalingAttr=NULL) {
  x <- if(is.null(scalingAttr)){
    scale(data)
  } else {
    scale(data,scalingAttr["mean",],scalingAttr["sd",])
  }
  if(class(data)[1] %in% c("matrix","data.frame","tbl_df")){
    return( list(
      "values"=as.data.frame(x),
      "scalingAttr"=rbind("mean"=attr(x,"scaled:center"),"sd"=attr(x,"scaled:scale"))
    ) )
  } else {
    return( list(
      "values"=as.numeric(x),
      "scalingAttr"=data.frame(c("mean"=attr(x,"scaled:center"),"sd"=attr(x,"scaled:scale")))
    ) )
  }
}

#' Normalise time serie applying pre processing transformations
#' and considering multiple input variables. Ignoring specific dates if required
#' and supporting multiple normalization methods
#'
#' @param data <timeserie>
#' @param localTimeZone <string> timezone
#' @param transformation <string> absolute or relative
#' @param inputVars <list of strings> Possible values: loadCurves, daysWeekend,
#' daysHolidays, daysWeek, dailyTemperature, dailyConsumption, dailyHdd, dailyCdd,
#' ratioDailyConsumptionHdd, ratioDailyConsumptionCdd
#' @param nDayParts <int> number of part days. Clustering considering
#' parts of the day as "aggregation" of multiple hours. Default value 24 so
#' each hour is considered a single part of the day
#' @param holidays <list date> holidays dates ignored in clustering
#' @param method <string> Normalization methods supported:
#'  - range01. Min-max normalization method
#'  - znorm. Z-score normalization method
#' @param scalingAttr <data.frame> it includes the scaling attributes for each
#' variable
#' @return normalised load <timeseries>
normalise_dlc <- function(data, localTimeZone, transformation = "relative", 
                          inputVars = c("loadCurves"), nDayParts = 24, holidays = c(),
                          method = "range01", scalingAttr = NULL, balanceOutdoorTemperatures = NULL){
  
  # data = tmp
  # transformation = loadCurveTransformation
  # holidays = holidaysDates
  # scalingAttr = NULL
  # method = normalisationMethod
  
  # if (class(holidays) == "character") {
  #   holidays <- read_csv(holidays, col_names = FALSE)
  #   holidays <- holidays$X1
  # }
  
  tmp <- data %>% 
    mutate(localtime = with_tz(time, localTimeZone), 
           date = lubridate::date(localtime), hour = hour(localtime), 
           daypart = floor(hour(localtime)/(24/nDayParts)) ) %>% 
    distinct(date, hour, .keep_all = TRUE)
  n_timesteps <- hourly_timesteps(24,detect_time_step(data))
  tmp_daily <- tmp %>% 
    mutate(
      weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start",1)),
      isWeekend = weekday %in% c(6, 7), 
      isHolidays = date %in% holidays) %>% 
    group_by(date, weekday, isWeekend, isHolidays) %>% 
    summarize(
      consumption = mean(consumption, na.rm=T) * n_timesteps, 
      temperature = mean(temperature, na.rm = TRUE),
      minConsumption = min(consumption, na.rm=T),
      maxConsumption = max(consumption, na.rm=T)
      ) %>% 
    ungroup()
  tmp_daily <- as.data.frame(tmp_daily)
  tmp_daily$weekday_f <- as.factor(as.character(tmp_daily$weekday))
  if(is.null(scalingAttr$levels_consumption)){
    levels_consumption <- c(min(tmp_daily$minConsumption,na.rm=T),
      get_local_min_from_density(tmp_daily$minConsumption,minNumberValleys=1),
      max(tmp_daily$minConsumption,na.rm=T))
    # levels_consumption <- setNames(lapply(levels(tmp_daily$weekday_f), function(d){
    #   aux <- tmp_daily$consumption[tmp_daily$weekday_f==d]
    #   density_mins <- lapply(seq(2,3.5,by=0.1), function(k){
    #     density_mins <- get_local_min_from_density(
    #       aux,minNumberValleys=1,kDensityAdjuster = k)
    #   })
    #   density_mins <- density_mins[mapply(function(d){is.null(d) || length(d)<=1},density_mins)][1][[1]]
    #   c(min(aux,na.rm=T),
    #     density_mins,
    #     max(aux,na.rm=T))}),levels(tmp_daily$weekday_f))
  } else {
    levels_consumption <- scalingAttr$levels_consumption
    levels_consumption[1] <- min(levels_consumption[1],min(tmp_daily$minConsumption,na.rm=T))
    levels_consumption[length(levels_consumption)] <- max(
      levels_consumption[length(levels_consumption)],
      max(tmp_daily$minConsumption,na.rm=T))
    # levels_consumption <- setNames(
    #   lapply(levels(tmp_daily$weekday_f), function(d){
    #     aux <- tmp_daily$consumption[tmp_daily$weekday_f==d]
    #     lc <- levels_consumption[[d]]
    #     lc[1] <- min(aux,na.rm=T)
    #     lc[length(lc)] <- max(aux,na.rm=T)
    #     lc
    #   }),
    #   levels(tmp_daily$weekday_f))
  }
  tmp_daily$lev <- arules::discretize(
              tmp_daily$minConsumption, method = "fixed", breaks = levels_consumption, labels=F)
  # tmp_daily <- tmp_daily %>% left_join(
  #   do.call(rbind, lapply(levels(tmp_daily$weekday_f), function(d){
  #     data.frame(
  #       date = tmp_daily$date[tmp_daily$weekday_f==d],
  #       lev = as.factor(paste(d,arules::discretize(
  #           tmp_daily$consumption[tmp_daily$weekday_f==d], 
  #           method = "fixed", breaks = levels_consumption[[d]], labels=F),
  #         sep="_"))
  #     )})), by="date")
  qu = c(0.05,0.25,0.5,0.75,0.95)
  if(is.null(scalingAttr$gam)){
    gam_temperature <-
      qgam::mqgam(
        consumption ~ 1 + s(temperature,bs="cs"),
        data=tmp_daily,
        qu=qu
      )
  } else {
    gam_temperature <- scalingAttr$gam
  }
  for (i in qu){
    tmp_daily[,sprintf("GAM%s",i*100)] <- as.numeric(qgam::qdo(gam_temperature, i, 
                                                     predict, newdata = tmp_daily))
    tmp_daily[,sprintf("residualsGAM%s",i*100)] <- 
      (tmp_daily$consumption - tmp_daily[,sprintf("GAM%s",i*100)]) / tmp_daily[,sprintf("GAM%s",i*100)]
  }
  # ggplot(tmp_daily)+geom_point(aes(temperature,consumption))+
  #   geom_line(aes(temperature,GAM5))+
  #   geom_line(aes(temperature,GAM25))+
  #   geom_line(aes(temperature,GAM50))+
  #   geom_line(aes(temperature,GAM75))+
  #   geom_line(aes(temperature,GAM95))
  
  gam_temperature_50 <- qgam::qdo(gam_temperature,0.5)
  aux <- gam_temperature_50$model$temperature
  tmp_daily$tbal <- mean(aux[which(gam_temperature_50$fitted.values <=
                     quantile(gam_temperature_50$fitted.values,0.2,na.rm=T))])
  # tmp_daily$tbal <- as.numeric(as.character(factor(
  #   tmp_daily$lev,
  #   levels=levels(tmp_daily$lev),
  #   labels=
  #     mapply(function(g){
  #       aux <- gam_temperature_50$model$temperature[gam_temperature_50$model$lev==g]
  #       tbalaux <- mean(aux[which(gam_temperature_50$fitted.values[gam_temperature_50$model$lev==g] <=
  #                   quantile(gam_temperature_50$fitted.values[gam_temperature_50$model$lev==g],0.2,na.rm=T))])
  #       # ggplot() + geom_point(aes(
  #       #   gam_temperature_50$model$temperature[gam_temperature_50$model$weekday_f==g],
  #       #   gam_temperature_50$fitted.values[gam_temperature_50$model$weekday_f==g]
  #       # )) + geom_vline(aes(xintercept = tbalaux))
  #       },
  #       levels(tmp_daily$lev))
  # )))
  tmp_daily$tbal <- ifelse(
    tmp_daily$tbal > quantile(tmp_daily$temperature,0.8,na.rm=T),
    quantile(tmp_daily$temperature,0.8,na.rm=T),
    ifelse(tmp_daily$tbal < quantile(tmp_daily$temperature,0.2,na.rm=T),
           quantile(tmp_daily$temperature,0.2,na.rm=T),
           tmp_daily$tbal))
  # tmp_daily$tbal <- 
  #   if(length(unique(as.character(tmp_daily$lev)))==1){
  #     as.numeric(gam_temperature_50$model$temperature[which.min(gam_temperature_50$fitted.values)])
  #   } else {
  #     as.numeric(as.character(factor(
  #     as.factor(as.character(tmp_daily$lev)),
  #     levels=unique(as.character(tmp_daily$lev)),
  #     labels=
  #       mapply(function(g){
  #         aux <- gam_temperature_50$model$temperature[gam_temperature_50$model$`as.factor(lev)`==g]
  #         aux[which.min(gam_temperature_50$fitted.values[gam_temperature_50$model$`as.factor(lev)`==g])]},
  #         unique(as.character(tmp_daily$lev)))
  #     )))
  # }
  tmp_daily[,"baseload"] <- 
    as.numeric(predict(gam_temperature_50,
                      tmp_daily %>% 
                        mutate(temperature=tbal) %>% 
                        select(temperature,lev,weekday_f)))
  tmp_daily["residualsEnergySignature"] <- 
    ((as.numeric(predict(gam_temperature_50,tmp_daily)))-tmp_daily$consumption)/
    tmp_daily$consumption
  tmp_daily[,"positiveResidualsEnergySignature"] <- 
    ifelse(tmp_daily[,"residualsEnergySignature"]>0,
           tmp_daily[,"residualsEnergySignature"],0)
  tmp_daily["negativeResidualsEnergySignature"] <- 
    ifelse(tmp_daily[,"residualsEnergySignature"]<0,
           -tmp_daily[,"residualsEnergySignature"],0)
  # ggplot(tmp_daily) +
  #   geom_point(aes(temperature,consumption,col=residualsEnergySignature))
  if(is.null(balanceOutdoorTemperatures)){ balanceOutdoorTemperatures <- "X" }
  for (b in balanceOutdoorTemperatures){
    if(b=="X"){
      tmp_daily[,paste0("hdd",b)] <- ifelse(tmp_daily$temperature>(tmp_daily$tbal-2),0,
                                            (tmp_daily$tbal-2)-tmp_daily$temperature)
      tmp_daily[,paste0("cdd",b)] <- ifelse(tmp_daily$temperature>(tmp_daily$tbal+2),
                                            tmp_daily$temperature-(tmp_daily$tbal+2),0)
    } else {
      tmp_daily[,paste0("hdd",b)] <- ifelse(tmp_daily$temperature>(b-2),0,(b-2)-tmp_daily$temperature)
      tmp_daily[,paste0("cdd",b)] <- ifelse(tmp_daily$temperature>(b+2),tmp_daily$temperature-(b+2),0)
    }
    tmp_daily[,paste0("hddint",b)] <- ifelse(tmp_daily[,paste0("hdd",b)]>0,1,0)
    tmp_daily[,paste0("cddint",b)] <- ifelse(tmp_daily[,paste0("cdd",b)]>0,1,0)
  }
  if(is.null(scalingAttr$lm_hdd_cdd)){
    #cl <- specc(as.matrix(tmp_daily[,c("consumption","temperature","isHolidays")]),centers=3)
    # ggplot(tmp_daily) + geom_point(aes(temperature,consumption,col=as.character(cl)))
    lm_hdd_cdd <- lapply(balanceOutdoorTemperatures,function(b){
      form <- as.formula(
          sprintf("consumption ~ 0 + lev +
                   hdd%s + cdd%s",b,b) )
      tryCatch({
        quantreg::rq(form, data = tmp_daily, tau = 0.75)
      }, error = function(m){ lm(form, tmp_daily) })
    })
    names(lm_hdd_cdd) <- balanceOutdoorTemperatures
  } else {
    lm_hdd_cdd <- scalingAttr$lm_hdd_cdd
  }
  # plotly::ggplotly(ggplot(tmp_daily) +
  #   geom_point(aes(temperature,consumption,col=lev),size=0.2,alpha=0.3) +
  #   geom_point(aes(temperature,predict(lm_hdd_cdd[["X"]],tmp_daily),col=lev),alpha=1))
  rls_data_builder <- function(form, data, na.rm=T){
    if(na.rm){
      data <- data[complete.cases(
        model.frame(form,data,na.action = 'na.pass')),]}
    data_matrix <- model.matrix(form,data)
    colnames(data_matrix) <- gsub(":","_",colnames(data_matrix))
    return(list(data=data, data_matrix=data_matrix))
  }
  
  # if(is.null(scalingAttr$rls_hdd_cdd)){
  #   rls_hdd_cdd <- lapply(balanceOutdoorTemperatures,function(b){
  #       form <- as.formula(
  #         sprintf("consumption ~ 0 + lev +
  #                      hdd%s:lev + cdd%s:lev",b,b) )
  #       
  #       rls_data <- rls_data_builder(form, tmp_daily)
  #       data <- rls_data$data
  #       data_matrix <- rls_data$data_matrix
  #       outputName = "consumption"
  #       
  #       # Create the model object
  #       model <- forecastmodel$new()
  #       model$output <- outputName
  #       do.call(model$add_inputs,as.list(setNames(colnames(data_matrix),colnames(data_matrix))))
  #       model$add_regprm("rls_prm(lambda=0.9)")
  #       model$kseq <- 0
  #       
  #       # Data transformation for RLS framework
  #       data_for_rls <- setNames(
  #         lapply(colnames(data_matrix),
  #                function(x) data.frame("k0"=data_matrix[,x])),
  #         colnames(data_matrix)
  #       )
  #       logOutput=F
  #       data_for_rls[[outputName]] <- if (logOutput){
  #         log(ifelse(data[,outputName]>0,data[,outputName],0.01))
  #       } else { data[,outputName] }
  #       data_for_rls[["t"]] <- data$date
  #       data_for_rls$scoreperiod <- sample(c(F,T),length(data_for_rls$t),replace = T,prob = c(0.9,0.1))
  #       
  #       # Fit the RLS model and obtain the time-varying coefficients
  #       mod_rls <- rls_fit(c("lambda"=0.91),model, data_for_rls, scorefun = rmse, printout = F)
  #       mod_rls$form <- form
  #       
  #       return(mod_rls)
  #     })
  #     names(rls_hdd_cdd) <- balanceOutdoorTemperatures
  # } else {
  #   rls_hdd_cdd <- scalingAttr$rls_hdd_cdd
  # }
  for (b in balanceOutdoorTemperatures){
    tmp_daily[,paste0("baseload",b)] <-
      predict(lm_hdd_cdd[[as.character(b)]],
              tmp_daily %>% mutate(
                across(c(paste0("cdd",b),paste0("hdd",b),
                         paste0("cddint",b),paste0("hddint",b)),
                       function(x)0)
              ))
    tmp_daily[,paste0("lmHddCdd",b)] <- predict(lm_hdd_cdd[[as.character(b)]],tmp_daily)
  }
  for (b in balanceOutdoorTemperatures){
    tmp_daily[paste0("ratioConsumptionHdd",b)] <- 
      ifelse(tmp_daily[,paste0("hdd",b)] >= 1 & tmp_daily[,paste0("cdd",b)] == 0,
             ifelse(tmp_daily$consumption > tmp_daily[,paste0("baseload",b)] ,
                    tmp_daily[,paste0("lmHddCdd",b)] - tmp_daily[,paste0("baseload",b)] ,0) / 
             (tmp_daily[,paste0("hdd",b)])
             , 0)
    tmp_daily[paste0("ratioConsumptionCdd",b)] <- ifelse(
      tmp_daily[,paste0("cdd",b)] >= 1 & tmp_daily[,paste0("hdd",b)] == 0,
      ifelse(tmp_daily$consumption > tmp_daily[,paste0("baseload",b)] ,
             tmp_daily[,paste0("lmHddCdd",b)] - tmp_daily[,paste0("baseload",b)] ,0) / 
        (tmp_daily[,paste0("cdd",b)])
      , 0)
    tmp_daily[paste0("residualsEnergySignature",b)] <- 
      (tmp_daily[,paste0("lmHddCdd",b)]-tmp_daily$consumption)/
      ifelse(tmp_daily$consumption>0,tmp_daily$consumption,1)
    tmp_daily[paste0("positiveResidualsEnergySignature",b)] <- 
      ifelse(tmp_daily[,paste0("residualsEnergySignature",b)]>0,
             tmp_daily[,paste0("residualsEnergySignature",b)],0)
    tmp_daily[paste0("negativeResidualsEnergySignature",b)] <- 
      ifelse(tmp_daily[,paste0("residualsEnergySignature",b)]<0,
             -tmp_daily[,paste0("residualsEnergySignature",b)],0)
    one_cdd_data <- tmp_daily %>% 
      mutate(
        across( c(paste0("cdd",b)),
                function(x)1) ) %>%
      mutate(
        across( c(paste0("hdd",b),paste0("cddint",b),paste0("hddint",b)),
                function(x)0)
      )
    one_hdd_data <- tmp_daily %>% 
      mutate(
        across( c(paste0("hdd",b)),
                function(x)1) ) %>%
      mutate(
        across( c(paste0("cdd",b),paste0("cddint",b),paste0("hddint",b)),
                function(x)0)
      )
    zero_dd_data <- tmp_daily %>%
      mutate(
        across( c(paste0("cdd",b),paste0("hdd",b),paste0("cddint",b),paste0("hddint",b)),
                function(x)0)
      )
    tmp_daily[paste0("slopeHdd",b)] <- 
      predict(lm_hdd_cdd[[as.character(b)]], one_hdd_data) - tmp_daily[paste0("baseload",b)]
    tmp_daily[paste0("slopeHdd",b)] <- ifelse(tmp_daily[,paste0("slopeHdd",b)]>0,
                                              tmp_daily[,paste0("slopeHdd",b)],0)
    tmp_daily[paste0("slopeCdd",b)] <- 
      predict(lm_hdd_cdd[[as.character(b)]],one_cdd_data) - tmp_daily[paste0("baseload",b)]
    tmp_daily[paste0("slopeCdd",b)] <- ifelse(tmp_daily[,paste0("slopeCdd",b)]>0,
                                              tmp_daily[,paste0("slopeCdd",b)],0)
    # tmp_daily <- tmp_daily %>% left_join(
    #   setNames(data.frame(
    #     rls_hdd_cdd[[as.character(b)]]$data$t,
    #     rowSums(rls_data_builder(rls_hdd_cdd[[as.character(b)]]$form, one_cdd_data,na.rm=T)$data_matrix[
    #     ,colnames(rls_hdd_cdd[[as.character(b)]]$Lfitval$k0)] * rls_hdd_cdd[[as.character(b)]]$Lfitval$k0) - 
    #       rowSums(rls_data_builder(rls_hdd_cdd[[as.character(b)]]$form, zero_dd_data,na.rm=T)$data_matrix[
    #       ,colnames(rls_hdd_cdd[[as.character(b)]]$Lfitval$k0)] * rls_hdd_cdd[[as.character(b)]]$Lfitval$k0),
    #     rowSums(rls_data_builder(rls_hdd_cdd[[as.character(b)]]$form, one_hdd_data,na.rm=T)$data_matrix[
    #       ,colnames(rls_hdd_cdd[[as.character(b)]]$Lfitval$k0)] * rls_hdd_cdd[[as.character(b)]]$Lfitval$k0) - 
    #       rowSums(rls_data_builder(rls_hdd_cdd[[as.character(b)]]$form, zero_dd_data,na.rm=T)$data_matrix[
    #         ,colnames(rls_hdd_cdd[[as.character(b)]]$Lfitval$k0)] * rls_hdd_cdd[[as.character(b)]]$Lfitval$k0)
    #   ),c("date",paste0("slopeCddDyn",b),paste0("slopeHddDyn",b))) )

  }
  tmp_daily$weekday_f <- NULL
  tmp_daily$lev <- NULL
  tmp_spread <- tmp %>% group_by(date, daypart) %>% 
    summarize(consumption = mean(consumption)) %>% 
    ungroup() %>% spread(daypart, consumption)
  loadCurves_columns <- colnames(tmp_spread)[colnames(tmp_spread)!="date"]
  if (transformation == "relative") {
    columns <- names(tmp_spread)
    columns <- columns[!(columns %in% "date")]
    tmp_spread[, columns] <- 
      tmp_spread[columns]/rowSums(tmp_spread[columns], 
      na.rm = TRUE)
  }
  tmp_spread <- tmp_spread %>% inner_join(tmp_daily, by = "date")
  dates <- tmp_spread$date
  tmp_spread$date <- NULL
  if(method=="range01"){
    norm <- normalise_range(data = tmp_spread %>% mutate_all(na.aggregate), lower = 0, upper = 1, 
                            scalingAttr = scalingAttr$normalise)
  } else if(method=="znorm") {
    norm <- normalise_zscore(data = tmp_spread, scalingAttr = scalingAttr$normalise)
  }
  vars <- list(loadCurves = loadCurves_columns, 
               daysWeekend = c("isWeekend"), 
               daysHolidays = c("isHolidays"), 
               daysWeek = c("weekday"),
               residualsGAM = sprintf("residualsGAM%s",qu*100),
               dailyConsumption = c("consumption"), 
               dailyMinConsumption = c("minConsumption"), 
               dailyMaxConsumption = c("maxConsumption"), 
               dailyTemperature = c("temperature"),
               ratioDailyConsumptionHdd = paste0("ratioConsumptionHdd",balanceOutdoorTemperatures),
               ratioDailyConsumptionCdd = paste0("ratioConsumptionCdd",balanceOutdoorTemperatures),
               dailyBaseloadConsumptionGAM = "baseload",
               residualsEnergySignature = "residualsEnergySignature",
               positiveResidualsEnergySignatureGAM = "positiveResidualsEnergySignature",
               negativeResidualsEnergySignatureGAM = "negativeResidualsEnergySignature",
               residualsEnergySignature = paste0("residualsEnergySignature",balanceOutdoorTemperatures),
               positiveResidualsEnergySignature = paste0("positiveResidualsEnergySignature",balanceOutdoorTemperatures),
               negativeResidualsEnergySignature = paste0("negativeResidualsEnergySignature",balanceOutdoorTemperatures),
               dailyBaseloadConsumption = paste0("baseload",balanceOutdoorTemperatures),
               slopeHdd = paste0("slopeHdd",balanceOutdoorTemperatures),
               slopeCdd = paste0("slopeCdd",balanceOutdoorTemperatures),
               # slopeHddDyn = paste0("slopeHddDyn",balanceOutdoorTemperatures),
               # slopeCddDyn = paste0("slopeCddDyn",balanceOutdoorTemperatures),
               dailyHdd = paste0("hdd",balanceOutdoorTemperatures),
               dailyCdd = paste0("cdd",balanceOutdoorTemperatures)
    )
  vars2 <- as.vector(unlist(vars[inputVars]))
  return(
    list(
      dates = dates,
      values = norm$values[,vars2[vars2 %in% colnames(norm$values)]], 
      scalingAttr = list("normalise"=norm$scalingAttr,
                         "lm_hdd_cdd"=lm_hdd_cdd,
                         "gam"=gam_temperature,
                         "levels_consumption"=levels_consumption), 
      inputVars = inputVars#[mapply(function(i)all(vars[[i]] %in% colnames(norm$values)),inputVars)]
    ))
}

#' Calculate affinity
#'  Code from https://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
#' Done applying a k-nearest neighboor filter to build a representation of a
#' graph. connecting just the closest dataset points. However, to be symmetric,
#' if Aij is selected as a nearest neighboor, so will Aji.
#'
#' @param S <matrix> Matrix to evaluate
#' @param n.neighboors <int> Number of neighboors
#' @return Affinity matrix
make.affinity <- function(S, n.neighboors = 2) {
  N <- length(S[, 1])
  
  if (n.neighboors >= N) { # fully connected
    A <- S
  } else {
    A <- matrix(rep(0, N^2), ncol = N)
    for (i in 1:N) { # for each line
      # only connect to those points with larger similarity
      best.similarities <- sort(S[i, ], decreasing = TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i, ] == s)
        A[i, j] <- S[i, j]
        A[j, i] <- S[i, j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A
}

#' Calculate similarity
#' Exponential Euclidean distance (Frobenius norm) with weighting factor
#'
#' @param x1 <vector> First vector
#' @param x2 <vector> Second vector
#' @param alpha <float> Weighting factor
#' @return Distance between vectors
similarity <- function(x1, x2, alpha = 1) {
  exp(-alpha * norm(as.matrix(x1 - x2), type = "F"))
}

#' Calculate similarity matrix
#' Code from https://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
#' Similarity Matrix, S, as the matrix that at Sij=s(xi,xj) gives the
#' similarity between observations xi and xj.
#' Common similarity measures are euclidean (default) an gaussian
#'
#' @param my.data <matrix> Matrix to evaluate
#' @param similariry <function> Similarity function
#' @return Similarity matrix
make.similarity <- function(my.data, similarity) {
  N <- nrow(my.data)
  S <- matrix(rep(NA, N^2), ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j) {
        S[i, j] <- similarity(my.data[i, ], my.data[j, ])
      } else {
        S[i, j] <- 0
      }
    }
  }
  S
}

#' Cluster similar daily load curves based on the load curves itself, calendar
#' variables and outdoor temperature
#'
#' @param data <timeSeries> containing the time series for total energy
#' consumption of a building, the outdoor temperature, or whatever input is
#' needed for clustering.
#' @param consumptionFeature <string> containing the column name the consumption
#' feature  in the data argument.
#' @param outdoorTemperatureFeature <string> containing the column name of the
#' outdoor temperature feature in the data argument.
#' @param localTimeZone <string> specifying the local time zone related
#' to the building in analysis
#' @param kMin <integer> defining the minimum number of allowed groups in
#' the clustering proceed.
#' @param kMax <integer> defining the maximum number of allowed groups in
#' the clustering proceed.
#' @param inputVars <list of strings> Select a number of features as an input
#' of the clustering.
#'   loadCurve: use the transformed version of the consumption daily load curve
#'     based on loadCurveTransformation and nDayParts arguments
#'   dailyTemperature: use the average daily outdoor temperature
#'   dailyConsumption: use the total daily consumption
#'   daysOfTheWeek: use a discrete value to represent the days of the week
#'   daysWeekend: use a boolean representing whether is weekend or not
#'   dailyHolidays: use a boolean representing whether is holiday or not.
#' @param loadCurveTransformation <string> that defines the transformation
#' procedure over the consumption load curves. Possible values are:
#'   relative: All daily load curves are relative to their total daily
#'     consumption. It is the default mode.
#'   absolute: The absolute consumption is used to define each daily
#'     load curves.
#' @param nDayParts <integer> defining the parts of day used to. Possible
#'   values: 24, 12, 8, 6, 4, 3, 2.
#' @param ignoreDates <list of dates> list of dates to ignore (holidays,
#' weather, ..)
#' @return <dict>
#'     dailyClassification <timeSeries> in daily frequency, containing
#' the classification of each daily load curve.
#'     absoluteLoadCurvesCentroids <matrix> with row names as the
#' identifier of the cluster, and column names as the day hours. This matrix
#' is filled with the hourly average consumption of each cluster detected.
#'     clusteringCentroids <matrix> with row names as the identifier of
#' the cluster, and column names as the input variables used by the clustering
#' algorithm. This matrix is filled with the average values of each input
#' variable and cluster.
#'     classificationModel <object> containing a simple classification
#' model to predict a load curve based on calendar features.
#'     opts: <dictionary>
#'   normSpecs: matrix of aggregations used in the Z-score normalisation of
#'     the clustering inputs. The different types of aggregations (mean,
#'     median, std, ...) are specified as row names, and the different
#'     input features are related with column names.
#'   loadCurveTransformation: object
#'   inputVars: object
#'   nDayParts: object

clustering_dlc <- function (data, consumptionFeature, outdoorTemperatureFeature, 
                            localTimeZone, kMax, kMin, inputVars, loadCurveTransformation, 
                            nDayParts, balanceOutdoorTemperatures, ignoreDates = c(), holidaysDates = c(),
                            normalisationMethod = "range01") {
  # data = df
  # consumptionFeature = "Qe"
  # outdoorTemperatureFeature = "temperature"
  # localTimeZone = tz
  # kMax = settings$DailyLoadCurveClustering$kMax
  # kMin = settings$DailyLoadCurveClustering$kMin
  # inputVars = settings$DailyLoadCurveClustering$inputVars
  # loadCurveTransformation = settings$DailyLoadCurveClustering$loadCurveTransformation
  # balanceOutdoorTemperatures = settings$DailyLoadCurveClustering$balanceOutdoorTemperatures
  # ignoreDates =
  #   df %>% group_by(date) %>% summarise(outliers=sum(outliers)>0) %>% filter(
  #     if(is.null(maxDateForClustering)){
  #       outliers==T
  #     } else {
  #       outliers==T | (date >= maxDateForClustering)
  #     }) %>% select(date) %>%
  #   unlist %>% as.Date
  # holidaysDates = holidaysDates
  # nDayParts = settings$DailyLoadCurveClustering$nDayParts
  # normalisationMethod = "range01"
  
  if(kMax>99){
    warning("kMax was coarced to 99. More than 100 groups are not allowed.")
    kMax<-99
    }
  data <- data[complete.cases(data[,c(consumptionFeature)]),]
  data[,outdoorTemperatureFeature] <- 
    na.locf(na.locf(
        na.approx(data[,outdoorTemperatureFeature],na.rm = F),
        fromLast = T,na.rm = T
      ),na.rm=T)
  tmp <- data %>% 
    select(time, all_of(consumptionFeature), all_of(outdoorTemperatureFeature)) %>% 
    filter(!(lubridate::date(time) %in% ignoreDates)) %>% 
    rename(consumption = consumptionFeature, temperature = outdoorTemperatureFeature)
  tmp_norm <- normalise_dlc(data = tmp, localTimeZone, transformation = loadCurveTransformation, 
                            inputVars, nDayParts, balanceOutdoorTemperatures = balanceOutdoorTemperatures, 
                            holidays = holidaysDates, scalingAttr = NULL, 
                            method = normalisationMethod)
  inputVars <- tmp_norm$inputVars
  S <- make.similarity(apply(tmp_norm$values,1:2,as.numeric), similarity)
  A <- make.affinity(S, 3)
  D <- diag(apply(A, 1, sum))
  U <- D - A
  evL <- eigen(U, symmetric = TRUE)
  evSpectrum <- log(rev(evL$values)[1:kMax] + 1e-12)
  evSpectrum <- evSpectrum - lag(evSpectrum, 1)
  evSpectrum[1] <- 0
  # if ((which.max(evSpectrum) - 1) <= 3) {
  #   evSpectrum[1:which.max(evSpectrum)] <- NA
  # }
  k <- max(kMin,which.max(evSpectrum))
  spectral_clust <- lapply(1:20,function(i)tryCatch({
    # kernlab::specc(apply(tmp_norm$values,1:2,as.numeric), centers = k,
    #                iterations = 400, mod.sample = 0.75, na.action = na.omit)
    kernlab::specc(apply(tmp_norm$values,1:2,as.numeric), centers = k, 
                   kpar="local",kernel="polydot",
                   #nystrom.red = T, nystrom.sample = nrow(tmp_norm$values)[1]/6,
          iterations = 80000, na.action = na.omit)
  }, error = function(e) {
    kernlab::specc(apply(tmp_norm$values,1:2,as.numeric), centers = k,
                   iterations = 8000, na.action = na.omit)
  }))
  spectral_clust <- spectral_clust[[
    which.min(mapply(spectral_clust,FUN=function(x)if(class(spectral_clust[[1]])=="specc"){sum(x@withinss,na.rm=T)}else{Inf}))
  ]]
  spectral_clust_valid <- (!(class(spectral_clust)[1] == "numeric"))
  s <- sprintf("%02i",as.numeric(as.character(spectral_clust)))
  if (spectral_clust_valid == TRUE) 
    s <- sprintf("%02i",as.numeric(as.character(spectral_clust@.Data)))
  cluster_results <- data.frame(date = tmp_norm$dates[complete.cases(tmp_norm$values)], s = s)
  tmp <- tmp %>% 
    mutate(localtime = with_tz(time, localTimeZone), 
           hour = hour(localtime), 
           date = lubridate::date(localtime), 
           weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start", 1)), 
           isWeekend = weekday %in% c(6, 7)) %>% 
    distinct(date, hour, .keep_all = TRUE) %>%
    arrange(time)
  cluster_struct <- merge(tmp, cluster_results, by = "date")
  if (spectral_clust_valid == TRUE) {
    cluster_centroids <- tmp_norm$values %>% mutate(s=s) %>% group_by(s) %>%
      summarise(across(.fns=mean)) %>% as.data.frame(.)
  } else {
    cluster_centroids <- tmp_norm$values %>% summarize_all(mean, na.rm = T)
    cluster_centroids <- data.frame(s = "01", t(cluster_centroids))
  }
  cluster_centroids <- cluster_centroids %>% 
    tibble::remove_rownames() %>% 
    tibble::column_to_rownames(var = "s")
  tmp_centroids <- cluster_struct %>% 
    group_by(hour, s) %>% 
    summarize(consumption = mean(consumption, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(s) %>% 
    spread(hour, consumption) %>% 
    tibble::remove_rownames() %>% 
    tibble::column_to_rownames(var = "s")
  
  ## Classification model
  cluster_results_df <- calendar_components(
    data.frame("time"=as.Date(cluster_results$date,tz=localTimeZone),
               "s"=cluster_results$s), 
    localTimeZone, holidaysDates, inplace = T)
  cluster_results_df$s <- as.factor(cluster_results_df$s)
  mod <- ranger::ranger(s~.,data = cluster_results_df[,c("s","dayYear","weekday")])
  # plot(as.numeric(cluster_results$s))
  # points(as.numeric(as.character(mod$predictions)),col="red")  
  
  return(list(
    dailyClassification = cluster_results, 
    absoluteLoadCurvesCentroids = tmp_centroids, 
    clusteringCentroids = cluster_centroids, 
    classificationModel = mod, 
    opts = list(scalingAttr = tmp_norm$scalingAttr, 
                loadCurveTransformation = loadCurveTransformation, 
                inputVars = inputVars, 
                nDayParts = nDayParts,
                normalisationMethod = normalisationMethod,
                balanceOutdoorTemperatures = balanceOutdoorTemperatures))
    )
}


#' Classify daily load curves based on the outputs of a clustering and a
#' new set of data
#'
#' @param data <timeSeries> containing the time series for total energy
#' consumption of a building, the outdoor temperature, or whatever input is
#' needed for clustering.
#' @param consumptionFeature <string> containing the column name the consumption
#' feature in the data argument.
#' @param outdorTemperatureFeature <string> containing the column name of the outdoor
#' temperature feature in the data argument.
#' @param localTimeZone <string> local time zone
#' @param clustering <object> clustering_dlc() output
#' @param method: <string>. Choose one, by default clusteringCentroids:
#'   absoluteLoadCurvesCentroids: Based on the absolute consumption load
#'     curve
#'   clusteringCentroids: Based on the inputs considered in the clustering
#'     procedure. Applying the same transformations done during that process.
#'   classificationModel: Based on a classification model of the calendar
#'     features.
#' @return dailyClassification: <timeSeries>
classification_dlc <- function(data, consumptionFeature, outdoorTemperatureFeature, 
                               localTimeZone, clustering, methodNormalDays="clusteringCentroids",
                               holidaysDatesFeature = NULL, abnormalDaysFeature = NULL,
                               holidaysDates = c(), abnormalDays = c(), methodAbnormalDays="clusteringCentroids") {
  
  # data = df[is.na(df$s),]
  # consumptionFeature = "Qe"
  # outdoorTemperatureFeature = "temperature"
  # localTimeZone = tz
  # holidaysDatesFeature = "holidaysDate"
  # abnormalDaysFeature = "abnormalDay"
  # clustering = clust
  # methodNormalDays = "clusteringCentroids"
  # methodAbnormalDays = "classificationModel"
  
  if(!is.null(holidaysDatesFeature)){
    holidaysDates <- unique(data$date[data[,holidaysDatesFeature]])
  }
  if(!is.null(abnormalDaysFeature)){
    abnormalDays <- unique(data$date[data[,abnormalDaysFeature]])
  }
  
  # Get consumption and temperatures from the dataset
  rows_with_consumption_and_temperature <- 
    complete.cases(data[,c(consumptionFeature, outdoorTemperatureFeature)])
  date_faulty <- unique(data$date[!rows_with_consumption_and_temperature])
  if(length(date_faulty)>0){
    abnormalDays <- unique(c(abnormalDays,date_faulty))
  }
  
  tmp <- data %>% 
    filter(!(date %in% date_faulty)) %>%
    select(time, all_of(consumptionFeature), all_of(outdoorTemperatureFeature)) %>% 
    rename(consumption = consumptionFeature, temperature = outdoorTemperatureFeature)
  
  # Normalise the dataset according to the settings of the clustering
  loadCurveTransformation <- clustering$opts$loadCurveTransformation
  inputVars <- clustering$opts$inputVars
  nDayParts <- clustering$opts$nDayParts
  normalisationMethod <- clustering$opts$normalisationMethod
  normalisationAttributes <- clustering$opts$scalingAttr
  balanceOutdoorTemperatures <- clustering$opts$balanceOutdoorTemperatures
  clusteringCentroids <- clustering$clusteringCentroids
  absoluteLoadCurvesCentroids <- clustering$absoluteLoadCurvesCentroids 
  if(nrow(tmp)>0){
    tmp_norm <- normalise_dlc(data = tmp, localTimeZone, transformation = loadCurveTransformation, 
                              inputVars, nDayParts, balanceOutdoorTemperatures = balanceOutdoorTemperatures,
                              holidays = holidaysDates, method=normalisationMethod, 
                              scalingAttr = normalisationAttributes)
    tmp_norm$values <- mapply(function(x)na.aggregate(unlist(tmp_norm$values[,x])),
                              colnames(tmp_norm$values))
    
    # Assure all hours are in place
    all_hours <- as.character(0:23)
    all_hours_default <- rep(NA, length(all_hours))
    names(all_hours_default) <- all_hours
    tmp <- tmp %>%
      mutate(
        localtime = with_tz(time, localTimeZone),
        date = lubridate::date(localtime),
        hour = hour(localtime),
        weekday = lubridate::wday(date,
                       week_start = getOption("lubridate.week.start", 1)
        ),
        is_weekend = weekday %in% c(6, 7)
      ) %>%
      distinct(date, hour, .keep_all = TRUE)
    tmp_spread <- tmp %>%
      select(date, hour, consumption) %>%
      spread(hour, consumption)
    tmp_spread <- tibble::add_column(tmp_spread, !!!all_hours_default[setdiff(all_hours, names(tmp_spread))]) %>%
      select(c(date, !!!all_hours))
    
    # Distance matrix between each individual and the clustering centroids.
    columns <- !(colnames(tmp_spread) %in% "date")
    tmp_centroids_dist <- t(proxy::dist(
      as.matrix(absoluteLoadCurvesCentroids),
      as.matrix(tmp_spread[, columns])))
    
    tmp_clust_dist <- if(class(tmp_norm$values)[1]=="numeric"){
      t(proxy::dist(
        as.matrix(clusteringCentroids[,names(tmp_norm$values)]),
        matrix(tmp_norm$values,nrow=1,dimnames = list("1",names(tmp_norm$values)))
      ))
    } else {
      t(proxy::dist(
        as.matrix(clusteringCentroids[colnames(tmp_norm$values)]),
        as.matrix(tmp_norm$values)
      ))
    }
  } else {
    tmp_clust_dist <- matrix()
    tmp_centroids_dist <- matrix()
    tmp_spread <- data.frame(
      "date"=sort(unique(data$date))
    )
  }
  
  # Classify across all centroids or the model
  classification_results <- data.frame(
    "date" = tmp_spread$date,
    # Centroids from raw data
    "absoluteLoadCurvesCentroids" = sprintf("%02i",
        tmp_centroids_dist %>% apply(1, function(x) {ifelse(sum(is.na(x))>1, NA, order(x)[1])})),
    "clusteringCentroids" = sprintf("%02i",
        tmp_clust_dist %>% apply(1, function(x) {ifelse(sum(is.na(x))>1, NA, order(x)[1])}))
  ) %>% right_join(
    data.frame(
      "date" = unique(data$date),
      "classificationModel" = if (class(clustering$classificationModel) %in% c("numeric","character","factor")) {
        sprintf("%02i",as.numeric(as.character(clustering$classificationModel)))
      } else {
        as.character(
          ranger::predictions(predict(clustering$classificationModel,
                  calendar_components(
                    data.frame("time"=as.Date(unique(data$date),tz=localTimeZone)), 
                    localTimeZone, holidaysDates, inplace = T)))
        )
      }
  ), by="date")
  classification_results <- classification_results[order(classification_results$date),]
  classification_results$s <- ifelse(
    classification_results$date %in% abnormalDays, 
    classification_results[,methodAbnormalDays],
    classification_results[,methodNormalDays]
  )
  
  return(list(
      dailyClassification = classification_results[,c("date","s")],
      sRaw = (data[,!(colnames(data) %in% c("s"))] %>% 
              left_join(classification_results[,c("date","s")],by="date"))$s
  ))
}

###
### Wrapper for data transformation in the modelling phase ----
###

data_transformation_wrapper <- function(data, features, transformationSentences, param, 
                                        transformationResults=list(), 
                                        weatherDependenceByCluster=NULL,
                                        clusteringResults=NULL){
  
  transformationItems <- list()
  if(is.null(transformationSentences)){
    transformationSentences <- list()
  }
  for (feature in unique(c(names(transformationSentences), features))){
      #feature <- unique(c(names(transformationSentences), features))[3]
      trFields <- list()
      trData <- NULL
      attach(data,warn.conflicts = F)
      if(feature %in% names(transformationSentences)){
        for (trFunc in transformationSentences[[feature]]){
          #trFunc <- transformationSentences[[feature]][1]
          trFuncName <- if(length(transformationSentences[[feature]])>1){
            trFunc } else { feature }
          if(grepl("[.]{3}",trFunc)){trFunc <- gsub("[.]{3}","data=data",trFunc)}
          if(grepl("clustering_wrapper\\(",trFunc,perl = T) && feature %in% names(transformationResults)){
            trFunc <- gsub("clustering_wrapper\\(","clustering_wrapper\\(results=transformationResults[[feature]],",trFunc,perl = T)
          }
          trDataElem <- eval(parse(text=trFunc))
          trOriginal <- NULL
          if(grepl("clustering_wrapper\\(",trFunc)){
            transformationResults[[feature]] <- trDataElem
            trDataElem <- setNames(trDataElem$labels,feature)
          } else if(any(class(trDataElem) %in% c("factor","character","logical"))){
            trDataElem <- fastDummies::dummy_cols(as.factor(trDataElem),remove_selected_columns = F)
            colnames(trDataElem) <- gsub(".data",trFuncName,colnames(trDataElem))
            trOriginal <- data.frame(trDataElem[,1])
            colnames(trOriginal)[1] <- colnames(trDataElem)[1]
            trDataElem[,1] <- NULL
          } else if(any(class(trDataElem) %in% c("numeric","integer"))){
            trDataElem <- data.frame(trDataElem)
            colnames(trDataElem) <- trFuncName
          } else if(any(class(trDataElem) %in% c("data.frame")) && length(trDataElem)==1){
            colnames(trDataElem) <- trFuncName
          }
          
          trData <- 
            if(!is.null(trData)){
              cbind(trData,trDataElem)
            } else {trDataElem}
          if(!is.null(trOriginal)){ 
            trData <- cbind(trOriginal,trData)
          }
          if(feature %in% features){
            trFields[[length(trFields)+1]] <- colnames(trDataElem)
          }
          # trData <- trData[,(ncol(data)+1):(ncol(trData))]
        }
      } else {
        trData <- tryCatch({
          aux <- eval(parse(text=feature))
          orig <- NULL
          aux <- if(is.list(aux)){
            as.data.frame(aux)
          } else {
            data.frame(aux)
          }
          if(ncol(aux)==1 && any(class(aux[,1]) %in% c("factor","character"))){
            aux <- fastDummies::dummy_cols(as.factor(aux[,1]),remove_selected_columns = F)
            colnames(aux) <- gsub(".data",feature,colnames(aux))
            orig <- data.frame(aux[,1])
            colnames(orig)[1] <- colnames(aux)[1]
            aux[,1] <- NULL
          } else if(ncol(aux)==1){
            aux <- setNames(aux,feature)
          }
          trData <- if (is.null(trData)) {
            aux
          } else {
            cbind(trData,aux)
          }},
          error=function(e){
            if(is.null(trData)){
              setNames(
                data.frame(rep(0,nrow(data))),
                feature
              )
            } else {
              cbind(trData, setNames(
                  data.frame(rep(0,nrow(data))),
                  feature
                )
              )
            }
          })
        if(!is.null(orig)){ 
          trData <- cbind(orig,trData)
        }
        trFields[[length(trFields)+1]] <- if(ncol(aux)==1){feature}else{
          colnames(aux)}
      }
      detach(data,unload = T)
      data <- cbind(data,trData)
      data <- data[,!duplicated(colnames(data),fromLast=T)]
      transformationItems[[feature]] <- list(
        "formula" = do.call(paste,c(do.call(expand.grid,trFields),sep=":")),
        "vars" = do.call(c,trFields)
      )
    }
    
    featuresAll <- features[!(features %in% names(transformationItems))]
    for(trFeat in names(transformationItems)[names(transformationItems) %in% features]){
      featuresAll <- c(featuresAll, transformationItems[[trFeat]]$vars)
    }
  # } else {
  #   featuresAll <- features
  # }
  return(list("featuresAll" = unique(featuresAll), "features" = unique(features), "items" = transformationItems, 
              "results" = transformationResults, "data" = data))
}
