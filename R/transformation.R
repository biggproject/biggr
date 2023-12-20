#
# General utils for transformation ----
#

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

get_local_min_from_density <- function(values,onlyPosition=NULL,minNumberValleys=1,kDensityAdjuster=3){
  d <- density(values,adjust=kDensityAdjuster,na.rm=T)
  tp <- pastecs::turnpoints(ts(d$y))
  y <- d$y[tp$tppos]
  x <- d$x[tp$tppos]
  sy <- c(0,dplyr::lag(y,1)[is.finite(dplyr::lag(y,1))])
  mins <- x[y-sy<=0]
  if(length(mins) < minNumberValleys) return(NULL)
  if(!is.null(onlyPosition)){
    return(mins[onlyPosition])
  } else { return(mins) }
}

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

opt_boxcox_lambda <- function(x){
  b <- MASS::boxcox(lm(x ~ 1))
  return(b$x[which.max(b$y)])
}

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

boxcox_transformation <- function(x, lambda){
  return(
    if(lambda == 0){ 
      log(x)
    } else {
      ((x ^ lambda) - 1) / lambda
    }
  )
}

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

inverse_boxcox_transformation <- function(x, lambda){
  return(
    if(lambda == 0){
      exp(x)
    } else {
      exp(log(lambda*x+1)/lambda)
    }
  )
}

#' 
#' 
#' Description
#'
#' @param arg <> 
#' @return 

data_transformation_wrapper <- function(data, features, transformationSentences, param, 
                                        transformationResults=list(), 
                                        weatherDependenceByCluster=NULL,
                                        clusteringResults=NULL){
  
  transformationItems <- list()
  if(is.null(transformationSentences)){
    transformationSentences <- list()
  }
  for (feature in unique(c(names(transformationSentences), features))){
    trFields <- list()
    trData <- NULL
    attach(data,warn.conflicts = F)
    if(feature %in% names(transformationSentences)){
      for (trFunc in transformationSentences[[feature]]){
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

#
# Time series transformations ----
#

#' Lag components
#' 
#' This function shift in time a set of features in order to be used in
#' the training and prediction of the models. It is an important step
#' for the multi-step prediction of autoregressive models, where the estimated
#' output is directly used in the subsequent predictions.
#'
#' @param data <data.frame> containing the multiple series to transform.
#' All the series that are not declared in featuresNames are
#' bypassed to the output
#' @param maxLag <integer> describing the maximum lags to be considered. One
#' feature will be generated for each lag.
#' @param featuresNames <array> selecting the series names to transform 
#' (names of the columns in data).
#' @param predictionStep optional, <integer> informing of the timestep considered in the
#' prediction. Only used in prediction mode, when training it doesn't need to
#' be described
#' @param forceGlobalInputFeatures <list> of values to be considered instead 
#' of the original values defined in data.
#' @param forceInitInputFeatures <list> of the initial values to consider 
#' before the first value defined in data.
#' @param forceInitOutputFeatures <list> of the initial values to consider 
#' before the first value defined in data.
#' @param fillInitNAs <boolean> indicating if the unknown lags should be filled
#' with their last known value.
#' 
#' @return <data.frame> containing the same initial information of the data
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
                tryCatch(zoo::na.fill(dplyr::lag(data[,f],l),"extend"),
                  error=function(e){rep(unique(data[is.finite(data[,f]),f])[1],
                                        nrow(data))})
              } else {
                dplyr::lag(data[,f],l)
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

#' Low pass filter
#' 
#' This function computes the first-order low pass filter for smoothing
#' a time series
#'
#' @param data <data.frame> containing the series to transform. Optionally,
#' other variables that are not declared in featuresNames can be bypassed to
#' the output
#' @param featuresNames <array> of strings selecting the series to transform
#' @param smoothingTimeScaleParameter <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.
#' @param outputFeaturesName optional, <array> giving the series names used as the output of
#' the transformation. It must have the same length as featuresNames.
#' By default, suffix "_lpf" is added to featuresNames.
#' @param inplace <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @param autoUnbox <boolean> indicating if the output data.frame should be
#' unboxed of the resultant data.frame, obtaining a numeric vector. Only is
#' usable when inplace is True.
#' 
#' @return <data.frame> containing the same initial information of the
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

#' Obtain alpha value for low pass filter
#' 
#' Physical transformation of the smoothing time scale parameter to consider
#' no affectance to the output variable after a known number of hours
#'
#' @param data <data.frame> to filter
#' @param timeConstantInHours <float> defining the number of hours that the a system
#' is affected by a certain change in the input time series (data argument).
#' For instance, when there's a need to model the affectance of the outdoor temperature 
#' to a building, this time constant in hours correspond to the 
#' thermal inertia of the building.
#' 
#' @return <float> of the smoothing time scale
#' parameter. It corresponds to the so-called alpha parameter of low pass
#' filter. It ranges from 0 to 1.

get_lpf_smoothing_time_scale <- function (data, timeConstantInHours) {
  timestep <- detect_time_step(data)
  return( (exp(1)^(-(3600/as.numeric(lubridate::as.period(timestep)))/
  ((2 * pi) * timeConstantInHours/24))) )
}

#' Time decomposition
#' 
#' Decompose the time in date, day of the year, day of the week, day of the
#' weekend, working day, non-working day, season, month, hour, minute, ...
#'
#' @param data <data.frame> containing the series to transform. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones). This argument is
#' optional, by default no transformation to local time zone is done.
#' @param holidays <array> of dates classified as holidays
#' @param inplace <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' 
#' @return <data.frame> containing the same initial information of data
#' input argument, plus the calendar components as new columns.
calendar_components <- function (data, localTimeZone = NULL, holidays = c(), inplace=T){
  data_ini <- data
  data <- data.frame("time"=lubridate::with_tz(
    data[,which(mapply(function(x)class(data[,x])[1],colnames(data)) %in% c("POSIXct","Date"))[1]],
    "UTC"))
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
  
  if(length(holidays) > 0){
    ## Holidays count
    localdates <- as.Date(data$time, tz=localTimeZone)
    start_date <- min(localdates)
    end_date <- max(localdates)
    date_sequence <- seq(start_date, end_date, by="1 month")
    formatted_dates <- format(date_sequence, "%Y-%m")
    unique_dates <- unique(formatted_dates) # List of months in holidays, %Y-%m
    holiday_counts <- table(format(holidays, "%Y-%m"))
    month_counts <- table(unique_dates) -1
    suppressWarnings(month_counts[names(month_counts) %in% names(holiday_counts)] <- holiday_counts)
    ## Add the weekends count
    for(d in unique_dates){
      first_day <- as.Date(paste(d, "-01", sep=""))
      weekend_days_bool <- wday(seq(first_day, ceiling_date(ymd(first_day), 'month') - days(1), "days")) %in% c(7, 1)
      weekend_days <- seq(first_day, ceiling_date(ymd(first_day), 'month') - days(1), "days")[weekend_days_bool]
      month_counts[paste(d)] <- month_counts[paste(d)] + sum(weekend_days_bool) - sum(weekend_days %in% holidays) 
    }
  } else {
    month_counts <- NULL
  }
  get_month_count <- function(month_counts,date){
    if(is.null(month_counts)){ 0 } else {
      as.numeric(month_counts[format(date,"%Y-%m")])}
  }
  
  result <- data %>% summarise(
    localtime = lubridate::with_tz(time, localTimeZone), 
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
    holidaysPerMonth = get_month_count(month_counts,as.Date(date,tz=localTimeZone)),
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
    if(nrow(data_ini)==1){
      return(cbind(data_ini,result))
    } else {
      return(cbind(data_ini[,!(colnames(data_ini) %in% colnames(result))],result))
    }
  } else {
    return(result)
  }
}

#' Obtain the components of the Fourier Series, in sine-cosine form
#'
#' It is useful for linearising the relationship of a seasonal input
#' time series (e.g. solar azimuth, solar elevation, calendar features, ...)
#' to some output (energy consumption, indoor temperatures, ...).
#' It basically decomposes a cyclic time series into a set of sine-cosine
#' components that are used as inputs for the modelling of some output,
#' each of the components linearly depends to the output.
#' @param X <timeSeries> timeSeries containing the series to transform.
#' This series must have a cyclic behaviour (e.g. hour of the day, day
#' of the week, solar azimuth, day of the year, ...) in order to be
#' correctly transformed. Optionally, other variables that are not
#' declared in featuresNames can be bypassed to the output..
#' @param featureNames <list> list of strings selecting the series
#' to transform
#' @param nHarmonics <integer> number of harmonics considered in
#' the Fourier Series. A high number allows to model more precisely
#' the relation, but it considerably increase the cost of computation.
#' The number of harmonics is related with the number of features in
#' the output matrix (2 * nHarmonics) + 1
#' @return <timeSeries> same initial information of data input argument,
#' plus the sine-cosine components of the Fourier Series as new columns

fs <- function(X, featureName, nHarmonics) {
  cbind(do.call(cbind,lapply(1:nHarmonics, function(i) {
    value <- list(sin(i * X * 2 * pi), cos(i * X * 2 * 
                                             pi))
    names(value) <- paste0(featureName, c("_sin_", "_cos_"), 
                           i)
    return(as.data.frame(value))
  })), setNames(data.frame(rep(1,length(X))),paste0(featureName,"_fs_int")))
}

#' Fourier series components decomposition
#'
#' Obtain the components of the Fourier Series, in sine-cosine form.
#' It is useful for linearising the relationship of a seasonal input
#' time series (e.g. solar azimuth, solar elevation, calendar
#' features, ...) to some output (energy consumption, indoor
#' temperatures, ...). It basically decomposes a cyclic time series
#' into a set of sine-cosine components that are used as inputs for
#' the modelling of some output, each of the components linearly depends
#' to the output.
#'
#' @param data <data.frame> containing the series to transform.
#' This series must have a cyclic behaviour (e.g. hour of the day, day of
#' the week, solar azimuth, day of the year, ...) in order to be correctly
#' transformed. Optionally, other variables that are not declared in
#' featuresNames can be bypassed to the output.
#' @param featuresNames <array> of strings selecting the series to transform.
#' @param nHarmonics <integer> defines the number of harmonics considered
#' in the Fourier Series. A high number allows to model more precisely
#' the relation, but it considerably increase the cost of computation.
#' The number of harmonics is related with the number of features in
#' the output matrix
#' @param mask <array> of booleans containing the timestamps that should be
#' accounted for the transformation. The timestamps set to false will
#' consider 0's for all their related sine-cosine components. By default,
#' all elements of the time series are considered.
#' @param inplace: <array> of booleans indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @param normMode <string> normalisation method to be used in features
#' preprocessing. Supported scaling methods
#'     - divided_by_max_plus_one: Max+1 normalisation method 
#'     - min_max_range: Min-max normalisation method
#'     - NULL: No normalisation applied
#'     
#' @return <data.frame> containing the same initial information of data
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

#
# Weather data transformations ----
#

#' Degree days without time aggregation
#' 
#' Calculate the difference between outdoor temperature and a base temperature,
#' without considering the frequency of the original data.
#'
#' @param data <data.frame> containing the series with data. The time zone
#' of the datetimes must be UTC. The other variables describing the series are
#' directly bypassed to the output.
#' @param featuresName <string> giving the column name of the outdoor
#' temperature feature.
#' @param baseTemperature <float> describing the Balance Point Temperature (BPT)
#' used in the calculation. Below BPT in heating mode, heat would be required by
#' the building. The contrary in the case of cooling, over BPT in cooling mode.
#' @param baseTemperatureName <string> giving the column name of the
#' baseTemperature if it's available in data. In this case baseTemperature
#' parameter is ignored.
#' @param outputFeaturesName optional, <string> giving the column name used as output of
#' the transformation. By default, "heating" or "cooling" depending the mode
#' used in the transformation.
#' @param mode <string> describing the calculation mode, which could be
#' "cooling" or "heating". By default, "heating" is configured.
#' @param maxValue <float> defining the maximum degree days allowed as output.
#' @param inplace <boolean> indicating if the output should be the original
#' data argument, plus the transformed objects -True- , or only the transformed
#' series -False.
#' @return <data.frame> of the difference between the temperature argument
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


#' Degree days with time aggregation
#' 
#' Calculate the degree-days with a desired output frequency and considering
#' cooling or heating mode.
#'
#' @param data <data.frame> containing the series with data. The time zone
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
#' time step. Examples: 'P1D' (One day), 'P1Y' (One year), 'P1M' (One month)...
#' @param outputFeaturesName <string> giving the column name used as output of
#' the degreedays result. By default, "HDD" is configured
#' @param fixedOutputFeaturesName <boolean> enable fixed column name used as
#' output of the degreedays results. Otherwise baseTemperature is added 
#' as suffix in the output column name
#' 
#' @return <data.frame> in the outputTimeStep of the heating or
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
      value = mean(value, na.rm = T)
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

#' Estimate the optimal change point temperature and weather dependance
#' 
#' Calculate change point temperature by modeling temperature vs consumption
#' relationship using a multi-step weather dependency signature model.
#' Multi-step weather dependency signature model is based in best
#' fitting penalised regression model analysis. Regression quality analysis
#' is used to identify wether proposed model properly describes the
#' weather vs consumption relationship. Model coefficients analysis is used
#' is used to identify type and amount of weather dependency.
#'
#' @param consumptionData <data.frame> containing consumption time series.
#' The time zone of the datetimes must be UTC.
#' @param weatherData <data.frame> containing temperature time series.
#' The time zone of the datetimes must be UTC.
#' @param consumptionFeature <string> with the name of consumption column.
#' @param temperatureFeature <string> with the name of temperature column.
#' @param consumptionGroupFeature <string> with the name of column in consumptionData
#' that describes the grouping feature used. By default, no group is
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones). This
#' argument is optional, by default no transformation to local time zone is
#' done.
#' @param plot <boolean> Plot change point model describing load vs
#' temperature relationship in order to identify breakpoints
#' @return Changepoint analysis results attributes <struct>
#'     - group. Group unique identifier
#'     - tbalh. Change point temperature in heating season
#'     - tbalh. Change point temperature in cooling season
#'     - heating. Heating dependency detected
#'     - cooling. Cooling dependency detected

get_change_point_temperature <- function(consumptionData, weatherData, 
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
    consumptionData <- data.frame(consumptionData[,c("time",consumptionFeature)],"01")
    consumptionGroupFeature <- "group"
  } else {
    consumptionData <- consumptionData[,c("time",consumptionFeature, consumptionGroupFeature)]
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
  
  newdata <- consumptionDataD %>% mutate(time=lubridate::force_tz(time,localTimeZone))
  newdata <- newdata %>% 
    left_join(weatherDataD, by="time")
  newdata$weekday <- strftime(newdata$time,"%w")
  newdata$group <- as.factor(newdata$group)

  group_var <- length(unique(newdata$group))>1
  if(group_var){
    #newdata$group <- as.factor(newdata$group)
    temp_term <- "s(temperature)"
    form <- as.formula(paste0("consumption ~ 0 + group + s(temperature, by=group)"))
  } else {
    temp_term <- "s(temperature)"
    form <- as.formula(paste0("consumption ~ 1 + s(temperature,k=1)"))
  }
  
  mod <- gam(form, data = newdata)
  mod[["yhat"]] <- predict(mod,newdata)
  mod[["y"]] <- newdata$consumption
  mod[["temperature"]] <- newdata$temperature
  
  pred_mat <- predict(mod, 
                      newdata = newdata, 
                      type = "link", 
                      se.fit = TRUE,
                      unconditional = TRUE)
  
  fd_tw_fit <- gratia::derivatives(object = mod,
                           term = "s(temperature)",
                           data = newdata,
                           order = 1L,
                           type = "central",
                           n = 40,
                           eps = 0.01,
                           level=0.95,
                           interval = "confidence",
                           n_sim = 100,
                           unconditional = F, 
                           partial_match = TRUE)
  
  signifD <- function(x, d, upper, lower, eval = 0) {
    miss <- upper > eval & lower < eval
    incr <- decr <- x
    want <- d > eval
    incr[!want | miss] <- NA
    want <- d < eval
    decr[!want | miss] <- NA
    list(incr = incr, decr = decr)
  }
  
  m2.dsig <- signifD(x = pred_mat$fit, 
                     d = fd_tw_fit$derivative,
                     upper = fd_tw_fit$upper, 
                     lower = fd_tw_fit$lower,
                     eval = 0)
  group_vector_on_condition_and_nearby_elements <- function(condition){
    gr <- 0
    x <- rep(1,length(condition))
    for(i in 1:length(condition)){
      if(i==1){
        x[i] <- NA
      } else if (condition[i-1]==T && condition[i]==T){
        x[i] <- gr
      } else if (condition[i]==T){
        gr <- gr + 1
        x[i] <- gr
      } else {
        x[i] <- NA
      }
    }
    return(x)
  }
  
  fd_tw_fit <- fd_tw_fit %>% 
    mutate(
      incr = m2.dsig$incr,
      incrl = !is.na(incr),
      decr = m2.dsig$decr,
      decrl = !is.na(decr)
    ) %>%
    arrange(group,data) %>%
    group_by(group) %>% 
    mutate(
      incrg = group_vector_on_condition_and_nearby_elements(incrl),
      decrg = group_vector_on_condition_and_nearby_elements(decrl)
    ) %>% ungroup()
    
  
  
  # Do not allow heating dependencies with higher temperatures than cooling dependencies.
  fd_tw_fit <- fd_tw_fit %>%
    left_join(
      fd_tw_fit %>% filter(!is.na(incrg)) %>% group_by(group,incrg) %>% summarise(
        considerIncr = max(data) >= 20 & length(data)>=3 )
    ) %>%
    left_join(
      fd_tw_fit %>% filter(!is.na(decrg)) %>% group_by(group,decrg) %>% summarise(
        considerDecr = min(data) <= 16 & length(data)>=3 )
    ) %>%
    mutate(
      incr = ifelse(considerIncr,incr,NA),
      decr = ifelse(considerDecr,decr,NA)
    )
  
  incr <- fd_tw_fit %>% filter(!is.na(incr))
  decr <- fd_tw_fit %>% filter(!is.na(decr))
  
  # Calculate upper and lower confidence Intervals
  upper_2se <- (pred_mat$fit) + (pred_mat$se.fit*2)  
  lwr_2se <- (pred_mat$fit) - (pred_mat$se.fit*2)
  pred_fit <- pred_mat$fit
  
  newdata %<>% dplyr::mutate(pred = pred_fit,
                              lwr_2se = lwr_2se,
                              upr_2se = upper_2se,
                              weatherDep = ifelse(newdata$temperature %in% incr$data,
                                                    "cooling",
                                                    ifelse(newdata$temperature %in% decr$data,
                                                           "heating",
                                                           "not detected") ) %>%
                                factor())
  
  # Summarise the balance temperatures and weather dependence per group
  summarised_results <- do.call(rbind,
    lapply( unique(newdata$group), 
            FUN= function(gr){
              tbalh <- newdata$temperature[newdata$group==gr & newdata$weatherDep=="heating"]
              tbalc <- newdata$temperature[newdata$group==gr & newdata$weatherDep=="cooling"]
              setNames(
                data.frame(
                  gr,
                  "tbalh" = ifelse(any(is.finite(tbalh)),round(max(tbalh,na.rm=T),2),NA),
                  "tbalc" = ifelse(any(is.finite(tbalc)),round(min(tbalc,na.rm=T),2),NA),
                  "heating" = any(is.finite(tbalh)),
                  "cooling" = any(is.finite(tbalc))
                ), c(consumptionGroupFeature,"tbalh","tbalc","heating","cooling")
              )}
    ))
  summarised_results <- summarised_results[order(summarised_results$s),]
  
  if(plot){
    g <- ggplot(data = newdata, aes(x = temperature, y = pred, group = group, shape=group, color = weatherDep))+
      geom_point(size = 1.25) +
      scale_color_manual(
        name = "Weather dependence",           
        values = c("not detected" = "black",
                   "cooling" = "blue",
                   "heating" = "red")) +
      scale_shape_discrete(name = "group") +
      geom_ribbon(aes(ymin = lwr_2se, ymax = upr_2se),
                  colour = "grey",
                  alpha = 0.2,
                  linetype = "blank") +
      geom_point(data = newdata %>%
                   select(temperature, consumption, weekday, group) %>%
                   dplyr::mutate(weatherDep = "not detected" %>% factor()),
                 aes(x = temperature, y = consumption, group=group, color = weatherDep),
                 size = 1.5,
                 alpha = 0.2,
                 colour = "black") +
      theme_bw() + xlab("temperature (ºC)") + ylab("consumption (kWh)") 
    print(
      g / tableGrob(
        summarised_results %>% rename("Balance temperature\nheating (ºC)"="tbalh", 
                                      "Balance temperature\ncooling (ºC)"="tbalc",
                                      "Heating dependence\ndetected" = "heating",
                                      "Cooling dependence\ndetected" = "cooling"
                                      ), 
        rows=NULL,theme = ttheme_default(base_size = 8))
    )
  }
  
  return(summarised_results)
}

#
# Daily load curves clustering and classification ----
#

#' Normalze time series using min-max range normalisation method
#'
#' @param data <data.frame> containing a set of time series to normalise
#' or an <array> of numerical values to normalise.
#' @param lower <float> lower value of the resultant range.
#' @param upper <float> upper value of the resultant range.
#' @param lowerThreshold <float> lower threshold value.
#' @param upperThreshold <float> upper threshold value.
#' @param scalingAttr <data.frame> setting min-max threshold for each variable.
#' Column names: column names specified in data argument; Row names: min, max.
#' In case of NULL (default value), lowerThreshold or upperThreshold will be used.
#' @return <list> containing two keys: "values" and "scalingAttr". The
#' former consists on a <data.frame> or an <array> with the normalised values, 
#' depending the class of data argument. The latter consists on the scaling 
#' attributes used for normalisation.

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

#' Normalise time series by sum of the day
#' 
#' Normalise time series using daily relative normalisation method
#'
#' @param data <data.frame> containing serie to normalise
#' @param method <string> Normalisation method. 
#' Supported methods: relative
#' 
#' @return <data.frame> daily normalised time series

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

#' Z-Normalisation
#' 
#' Normalise time series using Zscore normalisation method
#'
#' @param data <data.frame> containing serie to normalise
#' @param scalingAttr <data.frame> setting average and standard deviation 
#' thresholds for each variable. Column names: column names specified in 
#' data argument; Row names: mean, sd.
#' @return <list> containing two keys: "values" and "scalingAttr". The
#' former consists on a <data.frame> or an <array> with the normalised values, 
#' depending the class of data argument. The latter consists on the scaling 
#' attributes used for normalisation.

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

#' Daily load curve normalisation
#' 
#' Normalise time serie applying pre processing transformations
#' and considering multiple input variables. Ignoring specific dates if required
#' and supporting multiple normalisation methods
#'
#' @param data <timeserie>
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones).
#' @param transformation <string> absolute or relative
#' @param inputVars <array> of strings. Possible values: loadCurves, daysWeekend,
#' daysHolidays, daysWeek, dailyTemperature, dailyConsumption, dailyHdd, dailyCdd,
#' ratioDailyConsumptionHdd, ratioDailyConsumptionCdd
#' @param nDayParts <int> number of part days. Clustering considering
#' parts of the day as "aggregation" of multiple hours. Default value 24 so
#' each hour is considered a single part of the day
#' @param holidays <array> of dates. Holidays dates that are ignored in clustering phase.
#' @param method <string> Normalisation methods supported:
#'  - range01. Min-max normalisation method
#'  - znorm. Z-score normalisation method
#' @param scalingAttr <data.frame> it includes the scaling attributes for each
#' variable
#' @param balanceOutdoorTemperatures <array> of floats with the balance temperatures 
#' used in the degree days calculation. Optional. 
#' @return normalised load <data.frame>

normalise_dlc <- function(data, localTimeZone, transformation = "relative", 
                          inputVars = c("loadCurves"), nDayParts = 24, holidays = c(),
                          method = "range01", scalingAttr = NULL, balanceOutdoorTemperatures = NULL){
  
  tmp <- data %>% 
    mutate(localtime = lubridate::with_tz(time, localTimeZone), 
           date = lubridate::date(localtime), hour = hour(localtime), 
           daypart = floor(hour(localtime)/(24/nDayParts)) ) %>% 
    distinct(date, hour, .keep_all = TRUE)
  n_timesteps <- hourly_timesteps(24,detect_time_step(data))
  tmp_daily <- tmp %>% 
    mutate(
      weekday = lubridate::wday(date, week_start = getOption("lubridate.week.start",1)),
      isWeekend = weekday %in% c(6, 7), 
      isHolidays = date %in% holidays,
      month = month(date)) %>% 
    group_by(date, weekday, isWeekend, isHolidays, month) %>% 
    summarise(
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
  qu = c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9,0.95)
  if(is.null(scalingAttr$gam)){
    gam_temperature <-
      qgam::mqgam(
        consumption ~ 1 + s(temperature, bs="cs"),#weekday_f + s(temperature,bs="cs",by=weekday_f),
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
               month = c("month"), 
               dailyMinConsumption = c("minConsumption"), 
               dailyMaxConsumption = c("maxConsumption"), 
               dailyTemperature = c("temperature"),
               ratioDailyConsumptionHdd = paste0("ratioConsumptionHdd",balanceOutdoorTemperatures),
               ratioDailyConsumptionCdd = paste0("ratioConsumptionCdd",balanceOutdoorTemperatures),
               dailyBaseloadConsumptionGAM = "baseload",
               residualsEnergySignatureGAM = "residualsEnergySignature",
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
#' 
#' Code from https://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html
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
  #norm(as.matrix(x1 - x2), type = "F")
}

#' Calculate similarity matrix
#' 
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

#' Daily load curves clustering
#' 
#' Cluster similar daily load curves based on the load curves itself, calendar
#' variables and outdoor temperature
#'
#' @param data <data.frame> containing the time series for total energy
#' consumption of a building, the outdoor temperature, or whatever input is
#' needed for clustering.
#' @param consumptionFeature <string> containing the column name the consumption
#' feature  in the data argument.
#' @param outdoorTemperatureFeature <string> containing the column name of the
#' outdoor temperature feature in the data argument.
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones).
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
#'   daysWeek: use a factorial value for each day of the week.
#'   loadCurves: loadCurves_columns, 
#'   month: use the month of the year,
#'   dailyMinConsumption: use the minimum consumption of the day
#'   dailyMaxConsumption: use the maximum consumption of the day
#'   
# residualsGAM = sprintf("residualsGAM%s",qu*100),
# dailyMinConsumption = c("minConsumption"), 
# dailyMaxConsumption = c("maxConsumption"), 
# dailyTemperature = c("temperature"),
# ratioDailyConsumptionHdd = paste0("ratioConsumptionHdd",balanceOutdoorTemperatures),
# ratioDailyConsumptionCdd = paste0("ratioConsumptionCdd",balanceOutdoorTemperatures),
# dailyBaseloadConsumptionGAM = "baseload",
# residualsEnergySignatureGAM = "residualsEnergySignature",
# positiveResidualsEnergySignatureGAM = "positiveResidualsEnergySignature",
# negativeResidualsEnergySignatureGAM = "negativeResidualsEnergySignature",
# residualsEnergySignature = paste0("residualsEnergySignature",balanceOutdoorTemperatures),
# positiveResidualsEnergySignature = paste0("positiveResidualsEnergySignature",balanceOutdoorTemperatures),
# negativeResidualsEnergySignature = paste0("negativeResidualsEnergySignature",balanceOutdoorTemperatures),
# dailyBaseloadConsumption = paste0("baseload",balanceOutdoorTemperatures),
# slopeHdd = paste0("slopeHdd",balanceOutdoorTemperatures),
# slopeCdd = paste0("slopeCdd",balanceOutdoorTemperatures),
# # slopeHddDyn = paste0("slopeHddDyn",balanceOutdoorTemperatures),
# # slopeCddDyn = paste0("slopeCddDyn",balanceOutdoorTemperatures),
# dailyHdd = paste0("hdd",balanceOutdoorTemperatures),
# dailyCdd = paste0("cdd",balanceOutdoorTemperatures)
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
#' @return <list>
#'     dailyClassification <data.frame> in daily frequency, containing
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
                            nDayParts, balanceOutdoorTemperatures, nNeighboursAffinity = 7,
                            ignoreDates = c(), holidaysDates = c(), normalisationMethod = "range01") {
  # data = df
  # consumptionFeature = "Qe"
  # outdoorTemperatureFeature = "temperature"
  # localTimeZone = tz
  # kMax = settings$DailyLoadCurveClustering$kMax
  # kMin = settings$DailyLoadCurveClustering$kMin
  # nNeighboursAffinity = settings$DailyLoadCurveClustering$nNeighboursAffinity
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
        fromLast = T,na.rm = F
      ),na.rm=F)
  tmp <- data %>%
    select(time, all_of(consumptionFeature), all_of(outdoorTemperatureFeature)) %>%
    filter(!(lubridate::date(time) %in% ignoreDates)) %>%
    rename(consumption = consumptionFeature, temperature = outdoorTemperatureFeature)
  # inputVars <- c("dailyMinConsumption","loadCurves","dailyHdd",
  #                "dailyCdd","daysWeekend")
  tmp_norm <- normalise_dlc(data = tmp, localTimeZone, transformation = loadCurveTransformation, 
                            inputVars, nDayParts, balanceOutdoorTemperatures = balanceOutdoorTemperatures, 
                            holidays = holidaysDates, scalingAttr = NULL, 
                            method = normalisationMethod)
  inputVars <- tmp_norm$inputVars
  S <- make.similarity(apply(tmp_norm$values,1:2,as.numeric), similarity)
  A <- make.affinity(S, nNeighboursAffinity)
  D <- diag(apply(A, 1, sum))
  U <- D - A
  "%^%" <- function(M, power){
    with(eigen(M), vectors %*% (values^power * solve(vectors)))}
  L <- (D %^% (-1/2)) %*% A %*% (D %^% (-1/2))
  evL <- eigen(U, symmetric = TRUE)
  evSpectrum <- log(rev(evL$values)[1:(kMax+1)] - min(rev(evL$values)[1:(kMax+1)]) + 1e-12)
  evSpectrum <- (evSpectrum - lag(evSpectrum, 1))[-1]
   
  # if ((which.max(evSpectrum) - 1) <= 3) {
  #   evSpectrum[1:which.max(evSpectrum)] <- NA
  # }
  k <- max(kMin,which.max(evSpectrum))
  if(k>1){
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
  } else {
    spectral_clust <- 1
  }
  spectral_clust_valid <- (!(class(spectral_clust)[1] == "numeric"))
  if (spectral_clust_valid == TRUE){
    s <- sprintf("%02i",as.numeric(as.character(spectral_clust@.Data)))
  } else {
    s <- sprintf("%02i",as.numeric(as.character(spectral_clust)))
  }
  cluster_results <- data.frame(date = tmp_norm$dates[complete.cases(tmp_norm$values)], s = s)
  tmp <- tmp %>% 
    mutate(localtime = lubridate::with_tz(time, localTimeZone), 
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
    cluster_centroids$s <- "01"
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
  mod <- ranger::ranger(s~.,data = cluster_results_df[,c("s","month","dayYear","weekday")])
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


#' Daily load curve classification
#' 
#' Classify daily load curves based on the outputs of a clustering and a
#' new set of data
#'
#' @param data <data.frame> containing the time series for total energy
#' consumption of a building, the outdoor temperature, or whatever input is
#' needed for clustering.
#' @param consumptionFeature <string> containing the column name the consumption
#' feature in the data argument.
#' @param outdorTemperatureFeature <string> containing the column name of the outdoor
#' temperature feature in the data argument.
#' @param localTimeZone <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones).
#' @param clustering <object> clustering_dlc() output
#' @param method: <string>. Choose one, by default clusteringCentroids:
#'   absoluteLoadCurvesCentroids: Based on the absolute consumption load
#'     curve
#'   clusteringCentroids: Based on the inputs considered in the clustering
#'     procedure. Applying the same transformations done during that process.
#'   classificationModel: Based on a classification model of the calendar
#'     features.
#' @return dailyClassification: <data.frame>

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
        localtime = lubridate::with_tz(time, localTimeZone),
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


