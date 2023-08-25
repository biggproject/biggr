#
# General utils for harmonised data reading ----
#

#' Get timezone of a building
#' 
#' This function get from a BIGG-harmonised dataset the timezone of a list of buildings.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingSubjects <array> of URIs of the building subjects.
#' @return named <array> with the timezone for each building. 
#' The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones).

get_tz_building <- function(buildingsRdf, buildingSubjects){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?b ?tz
    WHERE {
      ?b a bigg:Building .
      FILTER ( ?b IN (<',paste(buildingSubjects,collapse='>,<'),'>) ) .
      ?b bigg:hasLocationInfo ?l .
      ?l bigg:addressTimeZone ?tz .
    }')))
  return( if(length(metadata_df)>0) {
    setNames(as.character(metadata_df$tz),nm=as.character(metadata_df$b))
  } else {NULL} )
}

#' Get gross floor area of a building
#' 
#' This function get from a BIGG-harmonised dataset the gross floor area of a list of buildings.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingSubjects <array> of URIs of the building subjects.
#' @return named <array> with the total gross floor area for each building.

get_area_building <- function(buildingsRdf, buildingSubjects){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?b ?area
    WHERE {
      ?b a bigg:Building .
      FILTER ( ?b IN (<',paste(buildingSubjects,collapse='>,<'),'>) ) .
      ?b bigg:hasSpace ?s .
      ?s bigg:hasArea ?a .
      ?a bigg:hasAreaType ?types .
      FILTER regex(str(?types),"GrossFloorArea$") .
      ?a bigg:areaValue ?area .
    }')))
  if(length(metadata_df)>0) {
    metadata_df$area <- as.numeric(metadata_df$area)
    metadata_df <- metadata_df %>% group_by(b) %>% summarise(area=sum(area,na.rm=T))
    r <- setNames(ifelse(metadata_df$area>0,metadata_df$area,NA),nm=as.character(metadata_df$b))
  } else { r <- NULL }
  return(r)
}

#' Get namespaces of a building
#' 
#' This function get from a BIGG-harmonised dataset the namespaces of a list of buildings.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingSubjects <array> of URIs of the building subjects.
#' @return named <array> with the namespace for each building.

get_building_namespaces <- function(buildingsRdf, buildingSubjects){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?b
    WHERE {
      ?b a bigg:Building .
      FILTER ( ?b IN (<',paste(buildingSubjects,collapse='>,<'),'>) ) .
    }')))
  
  return( 
    if(length(metadata_df)>0) {
      setNames(mapply(function(x){
        paste0(strsplit(metadata_df$b,"#")[[x]][1],"#")},1:nrow(metadata_df)),
        nm=metadata_df$b)
    } else { NULL } 
  )
}

#' Get building subjects from a set of building identifier from organisation
#' 
#' This function get the building subjects based on a list of building identifier(s) from organisation.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingIdsFromOrganization <array> of strings containing the building identifiers from the organisation.
#' @return <array> with the subject URI for each building.

get_buildings_subjects <- function(buildingsRdf, buildingIdsFromOrganization){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?o ?b
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?o .
      FILTER ( ?o IN ("',paste(buildingIdsFromOrganization,collapse='","'),'") ) .
    }')))
  
  return( 
    if(length(metadata_df)>0) {
      setNames(as.character(metadata_df$b),nm=as.character(metadata_df$o))
    } else { NULL } 
  )
}

#' Calculate the building identifiers
#' 
#' This function calculates the building identifiers from a list of building subjects.
#'
#' @param buildingSubjects <array> of URIs of the building subjects.
#' @return 

get_building_identifiers <- function(buildingSubjects){
  return(
    setNames(gsub("http://|https://||#||\\.","",buildingSubjects),nm = buildingSubjects)
  )
}

uri_to_identifier <- function(uris){
  return(
    setNames(gsub("http://|https://||#||\\.","",uris),nm = uris)
  )
}

#' Get all building subjects
#' 
#' This function get all building subjects available from a BIGG-harmonised dataset.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingSubjects <array> of URIs of the building subjects.
#' @return <array> with the subject URI for each building.

get_all_buildings_list <- function(buildingsRdf){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?b
    WHERE {
      ?b a bigg:Building .
    }')))
  return( if(length(metadata_df)>0) {as.character(metadata_df$b)} else {NULL} )
}

#' Check if exists an analytical model.
#' 
#' This function checks if certain analytical model subject exists in a BIGG-harmonised dataset.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param modelSubject <uri> of an analytical model.
#' @param namespaces named <array> that relates simple namespaces and complete.
#' ones.
#' @return <boolean> if the model exists.

exists_analytical_model <- function(buildingsRdf, modelSubject, namespaces){
  
  modelSubject <- namespace_integrator(modelSubject, namespaces)
  return(nrow(suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?m
    WHERE {
      ?m a bigg:AnalyticalModel .
      FILTER (?m = <',modelSubject,'>) .
    }'))))>0)
}

#' ISO 8601 period to natural text
#' 
#' This function converts an ISO 8601 period (e.g. PT1H30M) to natural text (in the example, "1 hour, 30 mins").
#'
#' @param x <string> defining the frequency to be converted. 
#' It must follow ISO 8601 format representing the time step. 
#' @param only_first <boolean> specifying if only the first 
#' timestep item should be converted.
#' @return <string> with the time step in natural text.

iso8601_period_to_text <- function(x,only_first=F){
  x <- lubridate::period(x)
  items <- c("year"=x@year,"month"=x@month,"day"=x@day,
             "hour"=x@hour,"min"=x@minute,
             "sec"=lubridate::second(x))
  text_items <- lapply(FUN = function(i){
    if(items[i]>0){
      paste(items[i], 
            if(items[i]>1){paste0(names(items)[i],"s")
            } else { names(items)[i] })
    }},1:length(items))
  text_items <- text_items[mapply(function(i)!is.null(i),text_items)]
  if(only_first)
    text_items <- text_items[1]
  return(do.call(function(...) paste(..., sep=", "), text_items))
}

#' ISO 8601 period to period
#' 
#' This function converts an ISO 8601 period (e.g. PT1H30M) to a period object.
#'
#' @param x <string> defining the frequency to be converted. 
#' It must follow ISO 8601 format representing the time step.
#' @return <period> according the format defined in lubridate's 'Period-class'.

iso8601_period_to_timedelta <- function(x){
  x <- lubridate::period(x)
  return(lubridate::years(x@year) + months(x@month) + lubridate::days(x@day) +
           lubridate::hours(x@hour) + lubridate::minutes(x@minute) + lubridate::seconds(lubridate::second(x)))
}

#
# Read time series from devices ----
#

#' Get sensor metadata
#' 
#' This function gets the available metadata of a certain sensor time series.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param sensorId <uri> of an analytical model.
#' @param tz <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones).
#' @return <data.frame> with the metadata of the sensor identifier.

get_sensor_metadata <- function(buildingsRdf, sensorId, tz){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?hasMeasurement ?timeSeriesFrequency ?timeSeriesIsCumulative
    ?timeSeriesTimeAggregationFunction ?timeSeriesIsOnChange ?timeSeriesIsRegular
    ?measuredProperty ?considerEstimatedValues
    WHERE {
      {
        SELECT ?m ?hasMeasurement 
        WHERE {
          ?m a bigg:Sensor.
          ?m bigg:hasMeasurement ?hasMeasurement .
          FILTER regex(str(?hasMeasurement), "',sensorId,'")
        }
      }
      optional {?m bigg:timeSeriesTimeAggregationFunction ?timeSeriesTimeAggregationFunction .}
      optional {?m bigg:timeSeriesIsCumulative ?timeSeriesIsCumulative .}
      optional {?m bigg:timeSeriesIsOnChange ?timeSeriesIsOnChange .}
      optional {?m bigg:timeSeriesIsRegular ?timeSeriesIsRegular .}
      optional {?m bigg:hasMeasuredProperty ?measuredProperty .}
      optional {?m bigg:timeSeriesFrequency ?timeSeriesFrequency .}
      optional {?m bigg:hasEstimationMethod ?hasEstimationMethod .}
      optional {?hasEstimationMethod bigg:considerEstimatedValues ?considerEstimatedValues .}
    }')))
  metadata_df$tz <- tz
  metadata_df$measuredProperty <- gsub(paste0(bigg_namespaces,collapse="|"),"",
                                       metadata_df$measuredProperty)
  metadata_df$timeSeriesFrequency <- ifelse(mapply(function(x)!is.na(x),
                                                   as.period(metadata_df$timeSeriesFrequency)),
                                            metadata_df$timeSeriesFrequency,NA)
  metadata_df$sensorId <- sensorId
  return(metadata_df)
}

#' Read time series files and generate a list of them split by sensor identifiers
#' 
#' This function gets time series from JSON files that has the name of a 
#' selected sensor identifier. If a list of time series is provided, this function
#' only filters the input list considering the sensor identifiers. 
#'
#' @param timeseriesObject <string> describing the local path with the JSON 
#' files or a <list> containing the time series.
#' @param sensorId <string> identifying a time series. 
#' @return <list> of data.frames with all time series found.

get_sensor_file <- function(timeseriesObject,sensorId){
  if(is.character(timeseriesObject)){
    jsonFiles <- list.files(timeseriesObject,"json",full.names=T)
    timeseriesObject_ <- unlist(lapply(
      jsonFiles[grepl(sensorId,jsonFiles)],
      function(x){jsonlite::fromJSON(x)}),recursive=F)
  } else {
    timeseriesObject_ <- timeseriesObject[names(timeseriesObject) %in% sensorId]
  }
  return(timeseriesObject_)
}

#' Read and prepare a time series
#' 
#' This function get a raw time series related with a sensor and transform it to the one with 
#' the required characteristics (e.g. time aggregation, time alignment, cumulative to instantaneous, 
#' irregular to regular time steps...). It also integrates the calculation of the energy cost and
#' energy emissions.
#' 
#' @param timeseriesObject <string> path of JSON files, or <list> of time series.
#' @param buildingSubject <uri> containing the building subject.
#' @param sensorId <string> containing the sensor identifier.
#' @param tz <string> specifying the local time zone related to the
#' building in analysis. The format of this time zones are defined by the IANA
#' Time Zone Database (https://www.iana.org/time-zones).
#' @param outputFrequency <string> defining the frequency selected as output. 
#' It must follow ISO 8601 format representing the time step.
#' @param aggFunctions <string> describing the possible aggregation functions of the
#' resultant time series. Possible values: 'SUM', 'AVG', 'HDD', 'CDD'.
#' @param useEstimatedValues <boolean> describing if the estimated values of time series 
#' should be taken into account.
#' @param ratioCorrection <boolean> describing whether a ratio correction should be done, 
#' or not. Important to set to TRUE when time series contain gaps.
#' @return <data.frame> containing the resultant time series.

get_sensor <- function(timeseriesObject, buildingsRdf, sensorId, tz,
                                      outputFrequency, aggFunctions,
                                      useEstimatedValues, integrateCost = T,
                                      integrateEmissions = T){
  
  # Get period and aggregation function specific for the timeseries
  metadata <- get_sensor_metadata(buildingsRdf, sensorId, tz)
  if(integrateCost){
    metadata_tariff <- get_tariff_metadata(buildingsRdf, sensorId)
  } else {
    metadata_tariff <- data.frame()
  }
  if(integrateEmissions){
    metadata_emissions <- get_emissions_metadata(buildingsRdf, sensorId)
  } else {
    metadata_emissions <- data.frame()
  }
  
  # If timeseriesObject is NULL, read certain sensor
  timeseriesObject_ <- get_sensor_file(timeseriesObject,metadata$sensorId)
  
  if(is.null(timeseriesObject_)) return(NULL)
  timeseriesSensor <- timeseriesObject_[sensorId][[1]]
  timeseriesSensor$start <- parse_iso_8601(timeseriesSensor$start)
  timeseriesSensor$end <- parse_iso_8601(timeseriesSensor$end)
  timeseriesSensor <- timeseriesSensor[order(timeseriesSensor$start),]
  if(!("isReal" %in% colnames(timeseriesSensor))){
    timeseriesSensor$isReal <- T
  }
  
  # Non-regular timeseries
  if(metadata$timeSeriesIsRegular==F){
    # If value data is cumulative, integrate the series
    if(metadata$timeSeriesIsCumulative){
      timeseriesSensor$start <- lag(timeseriesSensor$start,1)
      timeseriesSensor$end <- timeseriesSensor$end - lubridate::seconds(1)
      timeseriesSensor$value <- timeseriesSensor$value - lag(timeseriesSensor$value,1)
      timeseriesSensor <- timeseriesSensor[is.finite(timeseriesSensor$start),]
    }
    
    timesteps <- data.frame(
      secs = c(1,60,3600,86400),
      freq = c("PT1S","PT1M","PT1H","P1D"))
    
    # If values are on change, reconsider the start and end timestamps
    if(metadata$timeSeriesIsOnChange){
      aux <- timeseriesSensor$end + lubridate::period(metadata$timeSeriesFrequency)
      aux2 <- aux < lead(timeseriesSensor$start,1)
      aux2[is.na(aux2)] <- T
      timeseriesSensor$end <- lubridate::as_datetime(
        ifelse(aux2, aux, lead(timeseriesSensor$start,1)))
      interpolateFrequency <- lubridate::format_ISO8601(lubridate::as.period(
        quantile(difftime(lead(timeseriesSensor$start,1),timeseriesSensor$start,
                          tz = "UTC",units = "secs"),
                 0.1,na.rm=T)))
    } else {
      # Calculate the minimum frequency for series interpolation
      i_timestep <- which(timesteps$secs>=
              as.numeric(lubridate::as.period(
                quantile(difftime(timeseriesSensor$end, timeseriesSensor$start,
                                  tz = "UTC",units = "secs"), 0.1, na.rm=T)
              )))
      i_timestep <- if(length(i_timestep)==0){nrow(timesteps)}else{i_timestep[1]-1}
      interpolateFrequency <- timesteps[i_timestep,"freq"]
    }
    metadata$timeSeriesFrequency <- interpolateFrequency
    # Detect the subsets with no internal gaps
    timeseriesSensor$gapAfter <- ifelse(
      difftime(timeseriesSensor$start,lag(timeseriesSensor$end,1),units = "secs") > as.numeric(as.period(interpolateFrequency)),
      1,0)
    timeseriesSensor$gapAfter <- cumsum(ifelse(is.na(timeseriesSensor$gapAfter),0,timeseriesSensor$gapAfter))
    # Resample the original irregular series to a regular series, among the detected subsets
    dfs <- lapply(split(timeseriesSensor,timeseriesSensor$gapAfter), function(tsChunk){
      # tsChunk <- split(timeseriesSensor,timeseriesSensor$gapAfter)[[1]]
      if("AVG" %in% aggFunctions){
        tsChunk$iniValue <- tsChunk$value
        tsChunk$iniIsReal <- tsChunk$isReal
      } else {
        tsChunk$value <- cumsum(tsChunk$value)
        tsChunk$iniValue <- lag(tsChunk$value,1)
        tsChunk$iniIsReal <- lag(tsChunk$isReal,1)
        tsChunk$iniValue[1] <- 0
        tsChunk <- tsChunk[cumsum(tsChunk$isReal)!=0 &
          rev(cumsum(rev(tsChunk$isReal))!=0),]
      }
      if(nrow(tsChunk)==1 && is.na(tsChunk$iniValue)){
        return(NULL)
      }
      tsChunk <- rbind(
        data.frame("time"=tsChunk$start,"value"=tsChunk$iniValue,"isReal"=tsChunk$isReal),
        data.frame("time"=tsChunk$end,"value"=tsChunk$value,"isReal"=tsChunk$isReal))
      tsChunk$time <- lubridate::force_tz(lubridate::round_date(
        tsChunk$time,
        unit = iso8601_period_to_text(interpolateFrequency,only_first = T),
        week_start = getOption("lubridate.week.start", 7)),tz)
      tsChunk <- tsChunk[!duplicated(tsChunk$time),]
      tsChunk <- tsChunk[order(tsChunk$time),]
      if(metadata$considerEstimatedValues==F || useEstimatedValues==F) tsChunk <- tsChunk[tsChunk$isReal==T,]
      tsChunk <- tsChunk %>% 
        padr::pad(interval = iso8601_period_to_text(interpolateFrequency,only_first = T),
                  by = "time") %>%
        mutate(#time=as.POSIXct(lubridate::with_tz(time,"UTC"),tz="UTC"),
          value=zoo::na.approx(value,na.rm = F),
          isReal=zoo::na.locf(isReal))
      if(!("AVG" %in% aggFunctions)){
        tsChunk$value <- c(diff(tsChunk$value),NA)
      }
      return(tsChunk)
    })
    # Aggregation function used for the
    func <- function(x){ 
      if("AVG" %in% aggFunctions) {
        if(all(is.na(x))) NA else mean(x[!is.na(x)])
      } else { if(all(is.na(x))) NA else sum(x[!is.na(x)]) }
    }
    
    timeseriesSensor <- padr::pad(
      do.call(rbind,dfs) %>% 
        group_by(time) %>%
        summarise(value = func(value),
                  isReal = any(isReal,na.rm = T))
    )
    timeseriesSensor$isReal <- ifelse(is.finite(timeseriesSensor$isReal),timeseriesSensor$isReal,F)
    
    # Regular timeseries
  } else {
    timeseriesSensor$time <- timeseriesSensor$start
    timeseriesSensor$start <- NULL
    timeseriesSensor$end <- NULL
    timeseriesSensor <- padr::pad(timeseriesSensor,
                                  interval = iso8601_period_to_text(
                                    metadata$timeSeriesFrequency,only_first = T))
  }
  # Align time grid
  timeseriesSensor$time <- lubridate::round_date(
    timeseriesSensor$time,
    unit = iso8601_period_to_text(metadata$timeSeriesFrequency,only_first = T),
    week_start = getOption("lubridate.week.start", 7)
  )
  if(lubridate::period(outputFrequency)<lubridate::period("P1D")){
    aggFunctions <- aggFunctions[!(aggFunctions %in% c("HDD","CDD"))]
  }
  timeseriesSensor <- align_time_grid(
    data = timeseriesSensor,
    timeColumn = "time",
    valueColumn = "value",
    isRealColumn = "isReal",
    outputFrequency = outputFrequency,
    # if(any(c("HDD", "CDD") %in% aggFunctions)){
    #   "P1D" } else { outputFrequency },
    aggregationFunctions = aggFunctions,
    aggregationFunctionsSuffix = metadata$measuredProperty,
    # if(any(c("HDD", "CDD") %in% aggFunctions)){
    #   unique(c("AVG",aggFunctions[aggFunctions %in% c("SUM","MIN","MAX")]))
    # } else { 
    #   aggFunctions[aggFunctions %in% c("SUM","AVG","MIN","MAX")] 
    # },
    useEstimatedValues = metadata$considerEstimatedValues==T || useEstimatedValues==T,
    tz = metadata$tz
  )
  
  # Add energy cost component
  if(nrow(metadata_tariff)>0 & integrateCost){
    timeseriesSensor <- append_cost_to_sensor(
      buildingsRdf, timeseriesObject, 
      tariffSubject = metadata_tariff$tariff,
      measuredProperty = metadata$measuredProperty,
      frequency = metadata$timeSeriesFrequency,
      energyTimeseriesSensor = timeseriesSensor)
  }
  # Add emissions component
  if(nrow(metadata_emissions)>0 & integrateEmissions){
    timeseriesSensor <- append_emissions_to_sensor(
      buildingsRdf, timeseriesObject, 
      emissionsSubject = metadata_emissions$emissions,
      measuredProperty = metadata$measuredProperty,
      frequency = metadata$timeSeriesFrequency,
      energyTimeseriesSensor = timeseriesSensor)
  }
  
  return(timeseriesSensor)
}

#
# Read device aggregators ----
#

#' Get the metadata of all device aggregators
#'  
#' This function gets all the device aggregators available in a BIGG-harmonised dataset.
#' It also provides metadata with the characteristics of this device aggregators.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @return 

get_device_aggregators_metadata <- function(buildingsRdf){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?buildingSubject ?deviceAggregatorName ?deviceAggregatorFormula 
      ?deviceAggregatorFrequency ?deviceAggregatorTimeAggregationFunction
      ?measuredProperty
    WHERE {
      {
        SELECT ?buildingSubject ?s
        WHERE {
          ?buildingSubject a bigg:Building .
          ?buildingSubject bigg:hasSpace ?bs .
          ?bs bigg:hasDeviceAggregator ?s .
        }
      }
      optional {?s bigg:deviceAggregatorName ?deviceAggregatorName .}
      optional {?s bigg:deviceAggregatorFrequency ?deviceAggregatorFrequency .}
      optional {?s bigg:deviceAggregatorTimeAggregationFunction ?deviceAggregatorTimeAggregationFunction .}
      optional {?s bigg:deviceAggregatorFormula ?deviceAggregatorFormula .}
      optional {?s bigg:hasDeviceAggregatorProperty ?measuredProperty .}
    }')))
  result$measuredProperty <- gsub(paste0(bigg_namespaces,collapse="|"),"",
                                  result$measuredProperty)
  result$deviceAggregatorFrequency <- ifelse(mapply(function(x)!is.na(x),
                                                    as.period(result$deviceAggregatorFrequency)),
                                             result$deviceAggregatorFrequency,NA)
  return(result)
}

#' Compute the specified formula of a device aggregator
#' 
#' This function obtains all the time series related with a device aggregator, aggregates them 
#' according to the device aggregator metadata and obtains a single resultant time series.
#' 
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param timeseriesObject <string> path of JSON files, or <list> of time series.
#' @param buildingSubject <uri> containing the building subject.
#' @param formula <string> describing the formula for the device aggregator. 
#' It consist on a sequence of arithmetical operations over one (or multiple) sensor 
#' identifier(s). The sensor identifiers must be written between prefix '<mi>' and suffix </mi>. 
#' On contrary, the operators are described between prefix <mo> and suffix </mo>.
#' Example of the sum of two sensors (let's identify them as 'ID1' and 'ID2'): 
#' '<mi>ID1</mi><mo>+</mo><mi>ID2</mi>'
#' @param outputFrequency <string> defining the frequency selected as output. 
#' It must follow ISO 8601 format representing the time step.
#' @param aggFunctions <string> describing the possible aggregation functions of the
#' resultant time series. Possible values: 'SUM', 'AVG', 'HDD', 'CDD'.
#' @param useEstimatedValues <boolean> describing if the estimated values of time series 
#' should be taken into account.
#' @param ratioCorrection <boolean> describing whether a ratio correction should be done, 
#' or not. Important to set to TRUE when time series contain gaps.
#' @return <data.frame> containing the resultant time series.

compute_device_aggregator_formula <- function(buildingsRdf, timeseriesObject, 
                                            buildingSubject, formula, 
                                            outputFrequency, aggFunctions,
                                            useEstimatedValues, ratioCorrection=F, minRatioCorrection=0.7){
  
  result <- data.frame()
  op <- NULL
  tz <- get_tz_building(buildingsRdf, buildingSubject)
  while (formula!=""){
    if (substr(formula,1,4)=="<mi>"){
      res <- stringr::str_match(formula, "<mi>\\s*(.*?)\\s*</mi>")
      formula <- gsub(res[1,1],"",formula,fixed = T)
      aux_result <- eval(parse(text=
                                 paste0('get_sensor(
            timeseriesObject = timeseriesObject,
            buildingsRdf = buildingsRdf,
            sensorId = "',res[1,2],'",
            tz = "',tz,'",
            outputFrequency = "',outputFrequency,'",
            aggFunctions = ',paste0('c("',paste(aggFunctions,collapse='","'),'")'),',
            useEstimatedValues = ',useEstimatedValues,'
          )')
      ))
      aux_result$utctime <- lubridate::with_tz(aux_result$time,"UTC")
      if(ratioCorrection){
        if(any(grepl("^SUM",colnames(aux_result)))){
          for (sum_col in colnames(aux_result)[grepl("^SUM",colnames(aux_result))]){
            aux_result[,sum_col] <- ifelse(aux_result$RATIO >= minRatioCorrection, 
                                           unlist(aux_result[,sum_col] / aux_result$RATIO), NA)
            aux_result <- aux_result[is.finite(unlist(aux_result[,sum_col])),]
          }
        }
      }
      if(length(result)==0){
        result <- aux_result
      } else {
        elems <- colnames(result)
        result$utctime <- lubridate::with_tz(result$time,"UTC")
        result <- merge(result %>% select(-time), aux_result %>% select(-time), 
                        suffixes=c("_1","_2"), all=T, 
                        by.x="utctime",by.y="utctime")
        result$time <- lubridate::with_tz(result$utctime,tz)
        result$utctime <- NULL
        elems <- elems[elems!="utctime"]
        if(is.null(op)) {
          stop("Device aggregator operator is not defined")
        } else if(op=="+") {
          for (elem in elems[!(elems %in% c("time"))]){
            if(grepl("^AVG|RATIO|GAPS",elem)){
              result[,elem] <- rowMeans(result[,c(paste0(elem,"_1"),paste0(elem,"_2"))],na.rm=T)
            } else {
              result[,elem] <- rowSums(result[,c(paste0(elem,"_1"),paste0(elem,"_2"))],na.rm=T)
            }
          }
        }
      }
      if(!is.null(result)){
        result[,endsWith(colnames(result),"_1") | endsWith(colnames(result),"_2")] <- NULL
      }
    } else if (substr(formula,1,4)=="<mo>"){
      res <- stringr::str_match(formula, "<mo>\\s*(.*?)\\s*</mo>")
      formula <- gsub(res[1,1],"",formula,fixed = T)
      op <- res[1,2]
    } else if (substr(formula,1,4)=="<mn>"){
      res <- stringr::str_match(formula, "<mn>\\s*(.*?)\\s*</mn>")
      formula <- gsub(res[1,1],"",formula,fixed = T)
      num <- res[1,2]
      for (elem in elems[!(elems %in% c("time"))]){
        result[,elem] <- eval(parse(text=paste0('result[,"',elem,'"] ',op,' num')))
      }
    }
  }
  return(result)
}

#' Get the metadata and time series from a filtered set of device aggregators
#' 
#' This function get the metadata and time series from a filtered set of device 
#' aggregators available in a BIGG-harmonised dataset. 
#' 
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param timeseriesObject <string> path of JSON files, or <list> of time series.
#' @param allowedBuildingSubjects <array> of URI(s) containing the allowed building 
#' subject(s).
#' @param allowedMeasuredProperties <array> of string(s) containing the allowed 
#' measured property(ies).
#' @param useEstimatedValues <boolean> describing if the estimated values of time 
#' series should be taken into account.
#' @param ratioCorrection <boolean> describing whether a ratio correction should 
#' be done, or not. Important to set to TRUE when time series contain gaps.
#' @param containsEEMs <boolean> to filter only those buildings that contain one
#' or more EEM.
#' @param alignGreaterThanHourlyFrequencyToYearly <boolean> to force time
#' alignment to P1Y frequency if original alignment frequency is greater to PT1H.
#' @return <list> of time series and metadata related with all device aggregators.

get_device_aggregators <- function(
    buildingsRdf, timeseriesObject=NULL, allowedBuildingSubjects=NULL, 
    allowedMeasuredProperties=NULL, useEstimatedValues=F, ratioCorrection=T,
    containsEEMs=F, alignGreaterThanHourlyFrequencyToYearly=F){
  
  # Get formulas and associated metadata for each building and device aggregator
  devagg_buildings <- get_device_aggregators_metadata(buildingsRdf)
  
  # Filter by the allowed buildings
  if(!is.null(allowedBuildingSubjects)){
    devagg_buildings <- devagg_buildings[
      devagg_buildings$buildingSubject %in% allowedBuildingSubjects,
    ]
  }
  
  # Filter by the allowed device aggregator names
  if(!is.null(allowedMeasuredProperties)){
    devagg_buildings <- devagg_buildings[
      devagg_buildings$measuredProperty %in% allowedMeasuredProperties,
    ]
  }
  
  # Filter only buildings that contains EEMs
  if(containsEEMs==T & nrow(devagg_buildings)>0){
    eems_buildings <- get_building_eems(buildingsRdf, unique(devagg_buildings$buildingSubject))
    eems_buildings <- eems_buildings %>% group_by(buildingSubject) %>% summarise(
      EEMexists = sum(is.character(eemSubject))>0
    ) %>% ungroup()
    devagg_buildings <- devagg_buildings %>% left_join(eems_buildings, by = "buildingSubject") %>% 
      filter(EEMexists)
  }
  
  all_buildings_timeseries <- 
    setNames(lapply(unique(devagg_buildings$buildingSubject),
                    function(buildingSubject){
                      aux <- devagg_buildings[devagg_buildings$buildingSubject==buildingSubject,]
                      aux$deviceAggregatorFrequency <- ifelse(
                        is.na(aux$deviceAggregatorFrequency), 
                        "P1M", aux$deviceAggregatorFrequency)
                      largerFrequency <-
                        aux$deviceAggregatorFrequency[
                          which.max(lubridate::seconds(lubridate::period(aux$deviceAggregatorFrequency)))]
                      if(alignGreaterThanHourlyFrequencyToYearly && as.period(largerFrequency)>as.period("PT1H")){
                        largerFrequency <- "P1Y"
                      }
                      dfs <- setNames(lapply(unique(aux$deviceAggregatorName),
                                             function(devAggName){
                                               #devAggName = "totalElectricityConsumption"
                                               df <- compute_device_aggregator_formula(
                                                 buildingsRdf = buildingsRdf,
                                                 timeseriesObject = timeseriesObject,
                                                 buildingSubject = buildingSubject,
                                                 formula = as.character(unique(aux[aux$deviceAggregatorName==devAggName,
                                                                                   "deviceAggregatorFormula"])),
                                                 outputFrequency = largerFrequency,
                                                 aggFunctions = unlist(
                                                   unique(aux[aux$deviceAggregatorName==devAggName,
                                                              "deviceAggregatorTimeAggregationFunction"]), use.names = F),
                                                 useEstimatedValues = useEstimatedValues,
                                                 ratioCorrection = ratioCorrection
                                               )
                                               if(is.null(df)){
                                                 return(NULL)
                                               } else {
                                                 colnames(df) <- ifelse(colnames(df)=="time","time",
                                                                        paste(devAggName, colnames(df), sep="."))
                                                 return(df)
                                               }
                                             }), nm = unique(aux$deviceAggregatorName))
                      dfs <- dfs[mapply(function(l)!is.null(l),dfs)]
                      if(is.null(dfs)) return(NULL)
                      tz <- lubridate::tz(dfs[[1]]$time)
                      dfs <- lapply(dfs,function(x)cbind(x,data.frame("utctime"=lubridate::with_tz(x$time,"UTC"))))
                      ldf <- list(
                        "df"=Reduce(function(df1, df2){merge(df1[,!(colnames(df1)=="time")], 
                                                             df2[,!(colnames(df2)=="time")], 
                                                             by = "utctime", all=T)}, dfs),
                        "metadata"=devagg_buildings[devagg_buildings$buildingSubject==buildingSubject,]
                      )
                      ldf$df$time <- lubridate::with_tz(ldf$df$utctime,tz)
                      ldf
                    }
    ), nm = unique(devagg_buildings$buildingSubject))
  
  return(all_buildings_timeseries)
}

#
# Read Energy Efficiency Measures (EEMs) ----
#

#' Check if exists a project.
#' 
#' This function checks if a certain project subject exists in a BIGG-harmonised dataset.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param projectSubject <uri> of project subject.
#' @param namespaces named <array> that relates simple namespaces and complete 
#' ones.
#' @return <boolean> if the project exists.

exists_project_model <- function(buildingsRdf, projectSubject, namespaces){
  projectSubject <- namespace_integrator(projectSubject, namespaces)
  return(nrow(suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?m
    WHERE {
      ?m a bigg:Project .
      FILTER (?m = <',projectSubject,'>) .
    }'))))>0)
}

#' Get the Energy Efficiency Measures (EEMs) subjects from a set of buildings
#' 
#' This function search for all the available EEMs subjects
#' in a set of buildings. It also relates the EEMs with the 
#' building element.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param buildingSubjects <array> of URIs related with the building subjects.
#' @return <data.frame> containing the building subject, building element and 
#' EEM subject.

get_building_eems <- function(buildingsRdf, buildingSubjects=NULL){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?buildingSubject ?buildingElement ?eemSubject
    WHERE {
      {
        SELECT ?buildingSubject ?buildingElement ?eemSubject
        WHERE {
          ?buildingSubject a bigg:Building .',
    ifelse(!is.null(buildingSubjects),paste0('
            FILTER ( ?buildingSubject IN (<',paste(buildingSubjects,collapse='>,<'),'>) ) .'),
           ''),
    '?buildingSubject bigg:hasSpace ?bs .
          ?bs bigg:isAssociatedWithElement ?buildingElement .
        }
      }
      optional {?buildingElement bigg:isAffectedByMeasure ?eemSubject .}
    }')))
  return(if(length(result)>0) {
    result
  } else {NULL})
}

#' Get the complete metadata from a set of Energy Efficiecy Measures (EEMs)
#' 
#' This function get the metadata of a set of EEMs in a BIGG-harmonised dataset. 
#' The investment cost is always converted to Euros, for benchmarking purposes.
#'
#' @param buildingsRdf <rdf> containing the information of a set of buildings.
#' @param eemSubjects <array> of URIs related with the EEM subjects.
#' @return <data.frame> containing the metadata of EEMs

get_eem_details <- function(buildingsRdf, eemSubjects=NULL){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?eemSubject ?ExchangeRate ?Description ?Investment ?Date ?Currency ?Type ?AffectationShare
    WHERE {
      ?eemSubject a bigg:EnergyEfficiencyMeasure .',
    ifelse(!is.null(eemSubjects),paste0('
            FILTER ( ?eemSubject IN (<',paste(eemSubjects,collapse='>,<'),'>) ) .'),'
            '),
    'optional { ?eemSubject bigg:energyEfficiencyMeasureCurrencyExchangeRate ?ExchangeRate .}
       optional { ?eemSubject bigg:energyEfficiencyMeasureDescription ?Description .}
       optional { ?eemSubject bigg:energyEfficiencyMeasureInvestment ?Investment .}
       optional { ?eemSubject bigg:energyEfficiencyMeasureOperationalDate ?Date .}
       optional { ?eemSubject bigg:hasEnergyEfficiencyMeasureInvestmentCurrency ?Currency .}
       optional { ?eemSubject bigg:hasEnergyEfficiencyMeasureType ?Type .}
       optional { ?eemSubject bigg:shareOfAffectedElement ?AffectationShare .}
    }')))
  result$Investment <- ifelse(is.finite(result$ExchangeRate),
                              result$Investment * result$ExchangeRate,
                              result$Investment)
  result$Currency <- ifelse(is.finite(result$ExchangeRate),
                            "http://qudt.org/vocab/unit/Euro",
                            result$Currency)
  
  return(if(nrow(result)>0) {
    result
  } else {NULL})
}

get_eem_projects <- function(buildingsRdf, buildingSubject, eemSubjects=NULL){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?eemProjectSubject ?buildingSubject ?eemSubject ?Description ?Investment ?DateStart ?DateEnd
    WHERE {
      ?eemProjectSubject a bigg:Project .
      ?eemProjectSubject bigg:affectsBuilding ?buildingSubject .',
      paste0('FILTER ( ?buildingSubject IN (<',paste(buildingSubject,collapse='>,<'),'>) ) .'),'
      ?eemProjectSubject bigg:includesMeasure ?eemSubject .',
      ifelse(!is.null(eemSubjects),paste0('
            FILTER ( ?eemSubject IN (<',paste(eemSubjects,collapse='>,<'),'>) ) .'),'
            '),
      'optional { ?eemProjectSubject bigg:projectDescription ?Description .}
       optional { ?eemProjectSubject bigg:projectInvestment ?Investment .}
       optional { ?eemProjectSubject bigg:projectOperationalDate ?DateEnd .}
       optional { ?eemProjectSubject bigg:projectStartDate ?DateStart .}
    }')))
  if(nrow(result)>0) {
    result$eemProjectId <- factor(result$eemProjectSubject, levels=unique(result$eemProjectSubject), 
                                  labels=c(1:length(unique(result$eemProjectSubject))))
    return(result)
  } else {return(NULL)}
}

#
# Read Key Performance Indicators (KPIs) ----
#

#' Get one Key Performance Indicator (KPI) time series from a certain building.
#' 
#' This function use the harmonised data to extract a specific KPI time series of a certain 
#' building.
#'
#' @param buildingsRdf <rdf> containing all metadata about buildings. 
#' It must be harmonised to BIGG Ontology.
#' @param timeseriesObject <array> of strings with paths to JSON files containing time series,
#' or <list> of time series. It must be harmonised to BIGG Ontology.
#' @param buildingSubject <uri> of the building subject in buildingsRdf.
#' @param name <string> defining the indicator name to be retrieved. 
#' @param fromModel <boolean> defining if the time series should be real (FALSE), or estimated (TRUE)
#' @param frequency <string> defining the frequency to be retrieved. 
#' It must follow ISO 8601 format representing the time step. 
#' Examples: 'P1D' (One day), P1Y' (One year), 'P1M' (One month)
#' @return <data.frame> with columns 'time' and 'value'.

get_KPI_timeseries <- function(buildingsRdf, timeseriesObject, buildingSubject, 
                               name, fromModel, frequency){
  
  KPI_metadata <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?SingleKPI ?uriTimeSeries ?date
    WHERE {
      {
        SELECT ?SingleKPI ?uriTimeSeries
        WHERE {
          <',buildingSubject,'> bigg:assessesSingleKPI ?SingleKPI .
          FILTER regex(str(?SingleKPI),"',name,'") .
          ?SingleKPI bigg:timeSeriesFrequency "',frequency,'".
          ?SingleKPI bigg:hasSingleKPIPoint ?uriTimeSeries .
        }
      }
      OPTIONAL {?SingleKPI bigg:isEstimatedByModel ?est .}
      FILTER (',if(fromModel){""}else{"!"},'BOUND(?est))
      {
        OPTIONAL {?est bigg:modelTrainedDate ?date .}
      } UNION {
        BIND("1970-01-01T00:00:00.000" as ?date)
      }
    }
    ORDER BY DESC(?date) LIMIT 1
  ')))
  

  
  if(nrow(KPI_metadata)==0) return(NULL)
  
  KPI_metadata$uriTimeSeries <- mapply(
    function(i){i[2]},
    strsplit(KPI_metadata$uriTimeSeries,"#"))
  
  if(is.character(timeseriesObject)){
    timeseriesObject_ <- unlist(lapply(
      timeseriesObject[grepl(KPI_metadata$uriTimeSeries[1],timeseriesObject)],
      function(x){jsonlite::fromJSON(x)}),recursive=F)
  } else {
    timeseriesObject_ <- timeseriesObject[[KPI_metadata$uriTimeSeries[1]]]
  }
  
  timeseriesKPI <- timeseriesObject_[[KPI_metadata$uriTimeSeries[1]]]
  timeseriesKPI$start <- parse_iso_8601(timeseriesKPI$start)
  timeseriesKPI$end <- parse_iso_8601(timeseriesKPI$end)
  timeseriesKPI <- timeseriesKPI[order(timeseriesKPI$start),]
  timeseriesKPI <- timeseriesKPI %>% filter(is.finite(value))
  timeseriesKPI$time <- timeseriesKPI$start
  timeseriesKPI$start <- timeseriesKPI$end <- timeseriesKPI$isReal <- NULL
  
  return(timeseriesKPI)
}

#
# Read energy tariffs and emissions data ----
#

#' Get the energy tariff metadata
#' 
#' This function extract the energy tariff metadata related to a certain sensor identifier.
#'
#' @param buildingsRdf <rdf> containing all metadata about buildings. 
#' It must be harmonised to BIGG Ontology.
#' @param sensorId <string> with the sensor identifier 
#' (e.g. related to some consumption time series).
#' @return <data.frame> with the energy tariffs metadata.

get_tariff_metadata <- function(buildingsRdf, sensorId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?tariff
    WHERE {
      ?upd a bigg:UtilityPointOfDelivery.
      ?upd bigg:hasDevice ?dp.
      ?dp bigg:hasSensor ?m.
      ?m bigg:hasMeasurement ?hasMeasurement .
      FILTER regex(str(?hasMeasurement), "',sensorId,'")
      ?upd bigg:hasContractedTariff ?tariff.
    }')))
  metadata_df$sensorId <- sensorId
  return(metadata_df)
}

#' Get the emissions metadata
#' 
#' This function extract the emissions metadata related to a certain sensor identifier.
#'
#' @param buildingsRdf <rdf> containing all metadata about buildings. 
#' It must be harmonised to BIGG Ontology.
#' @param sensorId <string> with the sensor identifier 
#' (e.g. related to some consumption time series).
#' @return <data.frame> with the emissions metadata.

get_emissions_metadata <- function(buildingsRdf, sensorId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?emissions
    WHERE {
      ?upd a bigg:UtilityPointOfDelivery.
      ?upd bigg:hasDevice ?dp.
      ?dp bigg:hasSensor ?m.
      ?m bigg:hasMeasurement ?hasMeasurement .
      FILTER regex(str(?hasMeasurement), "',sensorId,'")
      ?upd bigg:hasCO2EmissionsFactor ?emissions.
    }')))
  metadata_df$sensorId <- sensorId
  return(metadata_df)
}

#' Append the cost and price to some energy time series sensor
#' 
#' This function calculates the cost and append the price to some 
#' energy time series sensor, based on a tariff definition for that
#' sensor in a BIGG-harmonised dataset.
#'
#' @param buildingsRdf <rdf> containing all metadata about buildings. 
#' It must be harmonised to BIGG Ontology.
#' @param timeseriesObject <string> path of JSON files, or <list> of time 
#' series.
#' @param tariffSubject <uri> with the tariff identifier.
#' @param measuredProperty <string> with the energy measured property to 
#' consider for the tariff cost calculation 
#' (e.g. EnergyConsumptionGridElectricity, EnergyConsumptionGas)
#' @param frequency <string> defining the frequency to be considered in the 
#' energy cost calculation. It must follow ISO 8601 format representing the 
#' time step.
#' @param energyTimeseriesSensor <data.frame> output from 
#' get_sensor().
#' 
#' @return <data.frame>, by-passing the input argument energyTimeseriesSensor and 
#' appending columns related to energy cost.

append_cost_to_sensor <- function(buildingsRdf, timeseriesObject, tariffSubject, measuredProperty,
                                frequency, energyTimeseriesSensor){
  tariffSubject <- namespace_integrator(tariffSubject,bigg_namespaces)
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?measuredProperty ?timeSeriesFrequency ?hash ?start ?end
    WHERE {
      {
        SELECT ?ct 
        WHERE {
          ?ct a bigg:ContractedTariff .
          FILTER (?ct = <',tariffSubject,'>) .
        }
      }
      ?ct bigg:hasTariff ?tar .
      ?ct bigg:contractStartDate ?start .
      optional { ?ct bigg:contractEndDate ?end . }
      ?tar bigg:hasTariffComponentList ?tcl.
      ?tcl bigg:hasTariffMeasuredProperty ?measuredProperty.
      ?tcl bigg:timeSeriesFrequency ?timeSeriesFrequency.
      ?tcl bigg:hasTariffComponentPoint ?hash.
    }')))
  if(nrow(metadata_df)==0){
    return(energyTimeseriesSensor)
  } else {
    metadata_df$measuredProperty <- gsub(paste0(bigg_namespaces,collapse="|"),"",
                                         metadata_df$measuredProperty)
    metadata_df$hash <- mapply(function(i){i[2]},strsplit(metadata_df$hash,"#"))
    if(any(metadata_df$timeSeriesFrequency==frequency)){
      metadata_df <- metadata_df[metadata_df$measuredProperty==measuredProperty &
                                   metadata_df$timeSeriesFrequency==frequency,]
    } else {
      frequency_ <- unique(metadata_df$timeSeriesFrequency[
        (as.period(metadata_df$timeSeriesFrequency) < as.period(frequency))])
      frequency_ <- frequency_[which.max(as.numeric(as.period(frequency_)))]
      metadata_df <- metadata_df[metadata_df$measuredProperty==measuredProperty &
                                   as.period(metadata_df$timeSeriesFrequency)==as.period(frequency_),]
    }
    metadata_df <- metadata_df[order(metadata_df$start),]
    prices <- do.call(rbind,lapply(1:nrow(metadata_df), function(i){
      
      timeseriesObject_ <- get_sensor_file(timeseriesObject,metadata_df$hash[i]) 
      timeseriesTariff <- timeseriesObject_[metadata_df$hash[i]][[1]]
      timeseriesTariff$start <- parse_iso_8601(timeseriesTariff$start)
      timeseriesTariff$end <- parse_iso_8601(timeseriesTariff$end)
      timeseriesTariff <- timeseriesTariff[order(timeseriesTariff$start),]
      timeseriesTariff <- timeseriesTariff[
        timeseriesTariff$start > metadata_df$start[i] & 
          if(is.finite(metadata_df$end[i])){
            timeseriesTariff$end < metadata_df$end[i]
          } else {T},
      ]}))
    prices <- prices[!duplicated(prices$start,fromLast=T),]
    prices <- align_time_grid(
      data = prices,
      timeColumn = "start",
      outputFrequency = frequency,
      aggregationFunctions = "AVG",
      aggregationFunctionsSuffix = paste0(measuredProperty,"_EnergyPrice"),
      tz = lubridate::tz(energyTimeseriesSensor$time))
    prices <- prices %>% select(
      "time",
      paste0("AVG_",measuredProperty,"_EnergyPrice"))
    energyTimeseriesSensor <- energyTimeseriesSensor %>% 
      left_join(prices, by="time")
    energyTimeseriesSensor[,"SUM_EnergyCost"] <- 
      if(sum(grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor)),na.rm=T)>1){
        rowSums(
          energyTimeseriesSensor[,grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor))] *
            energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyPrice")], 
          na.rm=T)
      } else {
        energyTimeseriesSensor[,grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor))] *
          energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyPrice")]
      }
    return(energyTimeseriesSensor)
  }
}

#' Append the emissions to some energy time series sensor
#' 
#' This function calculates the emissions related to some 
#' energy time series sensor, based on a tariff definition for that
#' sensor in a BIGG-harmonised dataset.
#'
#' @param buildingsRdf <rdf> containing all metadata about buildings. 
#' It must be harmonised to BIGG Ontology.
#' @param timeseriesObject <string> path of JSON files, or <list> of time 
#' series.
#' @param emissionsSubject <uri> with the emissions identifier.
#' @param measuredProperty <string> with the energy measured property to 
#' consider for the emissions calculation 
#' (e.g. EnergyConsumptionGridElectricity, EnergyConsumptionGas)
#' @param frequency <string> defining the frequency to be considered in the 
#' emissions calculation. It must follow ISO 8601 format representing the 
#' time step.
#' @param energyTimeseriesSensor <data.frame> output from 
#' get_sensor().
#' 
#' @return <data.frame>, by-passing the input argument energyTimeseriesSensor 
#' and appending columns related to emissions.

append_emissions_to_sensor <- function(buildingsRdf, timeseriesObject, emissionsSubject, 
                                       measuredProperty, frequency, energyTimeseriesSensor){
  emissionsSubject <- namespace_integrator(emissionsSubject,bigg_namespaces)
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?measuredProperty ?timeSeriesFrequency ?hash
    WHERE {
      {
        SELECT ?em
        WHERE {
          ?em a bigg:CO2EmissionsFactor .
          FILTER (?em = <',emissionsSubject,'>) .
        }
      }
      ?em bigg:hasCO2EmissionsFactorList ?efl.
      ?efl bigg:hasCO2RelatedMeasuredProperty ?measuredProperty.
      ?efl bigg:timeSeriesFrequency ?timeSeriesFrequency.
      ?efl bigg:hasCO2EmissionsFactorValue ?hash.
    }')))
  if(nrow(metadata_df)==0){
    return(energyTimeseriesSensor)
  } else {
    metadata_df$measuredProperty <- gsub(paste0(bigg_namespaces,collapse="|"),"",
                                       metadata_df$measuredProperty)
    metadata_df$hash <- mapply(function(i){i[2]},strsplit(metadata_df$hash,"#"))
    if(any(metadata_df$timeSeriesFrequency==frequency)){
      metadata_df <- metadata_df[metadata_df$measuredProperty==measuredProperty &
                                 metadata_df$timeSeriesFrequency==frequency,]
    } else {
      frequency_ <- unique(metadata_df$timeSeriesFrequency[
        (as.period(metadata_df$timeSeriesFrequency) < as.period(frequency))])
      frequency_ <- frequency_[which.max(as.numeric(as.period(frequency_)))]
      metadata_df <- metadata_df[metadata_df$measuredProperty==measuredProperty &
                                 as.period(metadata_df$timeSeriesFrequency)==as.period(frequency_),]
    }
    
    timeseriesObject_ <- get_sensor_file(timeseriesObject,metadata_df$hash) 
    emissions <- timeseriesObject_[metadata_df$hash][[1]]
    emissions$start <- parse_iso_8601(emissions$start)
    emissions$end <- parse_iso_8601(emissions$end)
    emissions <- emissions[order(emissions$start),]
    emissions <- emissions[!duplicated(emissions$start,fromLast=T),]
    emissions <- align_time_grid(
      data = emissions,
      timeColumn = "start",
      outputFrequency = frequency,
      aggregationFunctions = "AVG",
      aggregationFunctionsSuffix = paste0(measuredProperty,"_EnergyEmissionsFactor"),
      tz = lubridate::tz(energyTimeseriesSensor$time))
    emissions <- emissions %>% select(
      "time",
      paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor"))
    energyTimeseriesSensor <- energyTimeseriesSensor %>% 
      left_join(emissions, by="time")
    energyTimeseriesSensor[,"SUM_EnergyEmissions"] <- 
      if(sum(grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor)),na.rm=T)>1){
        rowSums(energyTimeseriesSensor[,
            grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor))] *
          energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor")], na.rm=T)
      } else {
        energyTimeseriesSensor[,
          grepl(paste0(measuredProperty,"$"),colnames(energyTimeseriesSensor))] *
        energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor")]
      }
    return(energyTimeseriesSensor)
  }
}