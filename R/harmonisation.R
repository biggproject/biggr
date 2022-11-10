bigg_namespaces <- c("bigg" = "http://bigg-project.eu/ontology#",
                     "unit" = "http://qudt.org/vocab/unit/")

write_rdf <- function(object, file){
  rdf_serialize(object, file,
                namespace = bigg_namespaces,
                format = "turtle")
}

get_KPI_timeseries <- function(buildingsRdf, timeseriesObject, buildingId, 
                               name, fromModel, frequency){
  
  KPI_metadata <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
          SELECT ?buildingId ?SingleKPI ?uriTimeSeries ?date
          WHERE {
            {
            SELECT ?buildingId ?SingleKPI ?uriTimeSeries
            WHERE {
                ?buildingId a bigg:Building .
                ?buildingId bigg:buildingIDFromOrganization "',buildingId,'" .
                ?buildingId bigg:assessesSingleKPI ?SingleKPI .
                FILTER regex(str(?SingleKPI),"',name,'")
                ?SingleKPI bigg:timeSeriesFrequency "',frequency,'".
                ?SingleKPI bigg:hasSingleKPIPoint ?uriTimeSeries .
              }
            }
            optional {?SingleKPI bigg:isEstimatedByModel ?est .}
            FILTER (',if(fromModel){""}else{"!"},'BOUND(?est)).
            optional {?est bigg:modelTrainedDate ?date}
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

get_KPI_by_building <- function(buildingsRdf,timeseriesObject,buildingId,
                                KPI, frequency, ratedBy, localTz){
  
  timeseriesKPI <- get_KPI_timeseries(buildingsRdf, timeseriesObject, buildingId,
                                      name = KPI$Name, fromModel = KPI$FromModel, frequency)
  
  if(is.null(timeseriesKPI)) return(NULL)
  
  if(!is.null(KPI$RatedBy)){
    for (rb in KPI$RatedBy){
      timeseriesKPI <- timeseriesKPI %>%
        left_join(
          get_KPI_timeseries(buildingsRdf, timeseriesObject, buildingId,
                             rb$Name, rb$FromModel, frequency),by="time")
      timeseriesKPI$value <- ifelse(timeseriesKPI$value.y==0, 0,
                                    timeseriesKPI$value.x / timeseriesKPI$value.y)
      timeseriesKPI$value.x <- timeseriesKPI$value.y <- NULL
    }
  }
  timeseriesKPI[,buildingId] <- timeseriesKPI$value
  timeseriesKPI$localtime <- format(with_tz(timeseriesKPI$time, localTz),
                                    format="%Y-%m-%d %H:%M:%S")
  timeseriesKPI[,paste0("utctime_",localTz)] <- timeseriesKPI$time
  timeseriesKPI$time <- timeseriesKPI$value <- NULL
  
  return(timeseriesKPI)
}

get_all_device_aggregators <- function(buildingsRdf){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0(
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?buildingId ?deviceAggregatorName ?deviceAggregatorFormula 
      ?deviceAggregatorFrequency ?deviceAggregatorTimeAggregationFunction
      ?measuredProperty
    WHERE {
      {
        SELECT ?buildingId ?s 
        WHERE {
          ?b a bigg:Building .
          ?b bigg:buildingIDFromOrganization ?buildingId .
          ?b bigg:hasSpace ?bs .
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
  return(result)
}

get_all_buildings_list <- function(buildingsRdf){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
    sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
            bigg_namespaces[i])},
    1:length(bigg_namespaces))),
    '
    SELECT ?buildingId
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?buildingId .
    }')))
  return( if(length(metadata_df)>0) {as.character(metadata_df$buildingId)} else {NULL} )
}

get_tz_building <- function(buildingsRdf, buildingId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?o ?tz
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?o .
      FILTER ( ?o IN ("',paste(buildingId,collapse='","'),'") ) .
      ?b bigg:hasLocationInfo ?l .
      ?l bigg:addressTimeZone ?tz .
    }')))
  return( if(length(metadata_df)>0) {
    setNames(as.character(metadata_df$tz),nm=as.character(metadata_df$o))
    } else {NULL} )
}

get_area_building <- function(buildingsRdf, buildingId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?o ?area
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?o .
      FILTER ( ?o IN ("',paste(buildingId,collapse='","'),'") ) .
      ?b bigg:hasSpace ?s .
      ?s bigg:hasArea ?a .
      ?a bigg:hasAreaType ?types .
      FILTER regex(str(?types),"GrossFloorArea$") .
      ?a bigg:areaValue ?area .
    }')))
  return( if(length(metadata_df)>0) {
    setNames(as.numeric(metadata_df$area),nm=as.character(metadata_df$o))
    } else {NULL} )
}

get_namespace_building <- function(buildingsRdf, buildingId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?b
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?o .
      FILTER ( ?o IN ("',paste(buildingId,collapse='","'),'") ) .
    }')))
  
  return( 
    if(length(metadata_df)>0) {
      paste0(strsplit(metadata_df$b,"#")[[1]][1],"#")
    } else { NULL } 
  )
}

get_subject_building <- function(buildingsRdf, buildingId){
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
      FILTER ( ?o IN ("',paste(buildingId,collapse='","'),'") ) .
    }')))
  
  return( 
    if(length(metadata_df)>0) {
      setNames(as.character(metadata_df$b),nm=as.character(metadata_df$o))
    } else { NULL } 
  )
}

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
  metadata_df$sensorId <- sensorId
  return(metadata_df)
}

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

append_cost_to_sensor <- function(buildingsRdf, timeseriesObject, tariffUri, measuredProperty,
                                frequency, energyTimeseriesSensor){
  tariffUri <- namespace_integrator(tariffUri,bigg_namespaces)
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
          FILTER (?ct = <',tariffUri,'>) .
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
  if(nrow(metadata_df)==0){
    return(energyTimeseriesSensor)
  } else {
    metadata_df <- metadata_df[order(metadata_df$start),]
    prices <- do.call(rbind,lapply(1:nrow(metadata_df), function(i){
      if(is.character(timeseriesObject)){
        jsonFiles <- list.files(timeseriesObject,"json",full.names=T)
        timeseriesObject_ <- unlist(lapply(
          jsonFiles[grepl(metadata_df$hash[i],jsonFiles)],
          function(x){jsonlite::fromJSON(x)}),recursive=F)
      }
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
      aggregationFunctionsSuffix = paste0(measuredProperty,"_EnergyPrice"))
    prices <- prices %>% select(
      "time",
      paste0("AVG_",measuredProperty,"_EnergyPrice"))
    energyTimeseriesSensor <- energyTimeseriesSensor %>% 
      left_join(prices, by="time")
    energyTimeseriesSensor[,"SUM_EnergyCost"] <- 
      if(sum(grepl(measuredProperty,colnames(energyTimeseriesSensor)),na.rm=T)>1){
        rowSums(
          energyTimeseriesSensor[,grepl(measuredProperty,colnames(energyTimeseriesSensor))] *
            energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyPrice")], 
          na.rm=T)
      } else {
        energyTimeseriesSensor[,grepl(measuredProperty,colnames(energyTimeseriesSensor))] *
          energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyPrice")]
      }
    return(energyTimeseriesSensor)
  }
}

append_emissions_to_sensor <- function(buildingsRdf, timeseriesObject, emissionsUri, 
                                       measuredProperty, frequency, energyTimeseriesSensor){
  emissionsUri <- namespace_integrator(emissionsUri,bigg_namespaces)
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
          FILTER (?em = <',emissionsUri,'>) .
        }
      }
      ?em bigg:hasCO2EmissionsFactorList ?efl.
      ?efl bigg:hasCO2RelatedMeasuredProperty ?measuredProperty.
      ?efl bigg:timeSeriesFrequency ?timeSeriesFrequency.
      ?efl bigg:hasCO2EmissionsFactorValue ?hash.
    }')))
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
  if(nrow(metadata_df)==0){
    return(energyTimeseriesSensor)
  } else {
    if(is.character(timeseriesObject)){
      jsonFiles <- list.files(timeseriesObject,"json",full.names=T)
      timeseriesObject_ <- unlist(lapply(
        jsonFiles[grepl(metadata_df$hash,jsonFiles)],
        function(x){jsonlite::fromJSON(x)}),recursive=F)
    }
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
      aggregationFunctionsSuffix = paste0(measuredProperty,"_EnergyEmissionsFactor"))
    emissions <- emissions %>% select(
      "time",
      paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor"))
    energyTimeseriesSensor <- energyTimeseriesSensor %>% 
      left_join(emissions, by="time")
    energyTimeseriesSensor[,"SUM_EnergyEmissions"] <- 
      if(sum(grepl(measuredProperty,colnames(energyTimeseriesSensor)),na.rm=T)>1){
        rowSums(energyTimeseriesSensor[,
            grepl(measuredProperty,colnames(energyTimeseriesSensor))] *
          energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor")], na.rm=T)
      } else {
        energyTimeseriesSensor[,
          grepl(measuredProperty,colnames(energyTimeseriesSensor))] *
        energyTimeseriesSensor[,paste0("AVG_",measuredProperty,"_EnergyEmissionsFactor")]
      }
    return(energyTimeseriesSensor)
  }
}

get_matrix_building_space_use_types <- function(buildingsRdf, splitSublevels=T){
  types_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?buildingId ?typology
    WHERE {
      ?b a bigg:Building .
      ?b bigg:buildingIDFromOrganization ?buildingId .
      ?b bigg:hasSpace ?bs .
      ?bs bigg:hasBuildingSpaceUseType ?typology
    }')))
  if(length(types_df)>0){
    types_df$typology <- mapply(function(x){x[2]}, strsplit(types_df$typology,"#"))
    if(splitSublevels){
      typologies <- lapply(1:nrow(types_df),
                           function(i){
                             elems <- strsplit(types_df$typology[i],split = "\\.")[[1]]
                             mapply(function(j) paste(elems[1:j],collapse="."), 1:length(elems))
                           })
      types_df<-do.call(rbind,
                        lapply(1:nrow(types_df),function(i){
                          data.frame(buildingId=types_df$buildingId[i],typology=typologies[[i]])
                        }))
    }
    types_df <- dummy_cols(types_df,select_columns = "typology")
    types_df$typology <- NULL
    colnames(types_df) <- gsub("typology_","",colnames(types_df))
    types_df <- types_df %>% group_by(buildingId) %>% summarise(across(colnames(types_df)[-1],sum))
    types_df[,"NA"] <- NULL
    types_df$all <- 1
    return(types_df)
  } else {
    return(NULL)
  }  
}

# ## Add fake series
# timeseries <- jsonlite::fromJSON("~/Nextcloud/Beegroup/Projects/Desenvolupaments_TIC_BIGG-ENTRACK/6_Data_analytics/data_example/one_building_series.json")
# 
# pad_df <- padr::pad(data.frame("time"=seq(as.POSIXct("2020-01-01 00:03:51",tz="UTC"),
#                          as.POSIXct("2020-05-01 00:03:55",tz="UTC"),by="hours"),"value"=10+sin(2*pi*(0:2904 %% 24)/24)),
#           period_to_text("PT5M",only_first = T))
# pad_df$value <- zoo::na.approx(pad_df$value)
# pad_df <- pad_df %>% rename(start=time)
# pad_df$end <- pad_df$start+lubridate::minutes(4)+lubridate::seconds(59)
# pad_df <- pad_df[-(4000:4100),]
# pad_df$start <- format(pad_df$start,"%Y-%m-%dT%H:%M:%SZ","UTC")
# pad_df$end <- format(pad_df$end,"%Y-%m-%dT%H:%M:%SZ","UTC")
# timeseries$`101898a024310399210beb4d7f7bb890c568714cc006e7e6429cc6869ccb215c` <- pad_df
# 
# gas_df1 <- data.frame(
#   "start"=c("2020-01-01T00:00:00Z","2020-01-28T00:00:00Z","2020-01-31T00:00:00Z","2020-03-10T00:00:00Z","2020-04-01T00:00:00Z","2020-04-15T00:00:00Z"),
#   "end"=c("2020-01-25T23:59:59Z", "2020-01-30T23:59:59Z","2020-03-09T23:59:59Z","2020-03-31T23:59:59Z","2020-04-10T23:59:59Z","2020-05-01T23:59:59Z"),
#   "value"=c(230.13, 10.24, 580.76, 1050, -700.12,200.4),
#   "isReal"=c(T,T,T,F,T,T)
# )
# gas_df2 <- data.frame(
#   "start"=c("2020-01-01T00:00:00Z","2020-01-28T00:00:00Z","2020-01-31T00:00:00Z","2020-03-10T00:00:00Z","2020-04-01T00:00:00Z","2020-04-15T00:00:00Z","2020-05-02T00:00:00Z"),
#   "end"=c("2020-01-01T00:00:00Z","2020-01-28T00:00:00Z","2020-01-31T00:00:00Z","2020-03-10T00:00:00Z","2020-04-01T00:00:00Z","2020-04-15T00:00:00Z","2020-05-02T00:00:00Z"),
#   "value"=cumsum(c(0, 230.13, 10.24, 580.76, 1050, -700.12,200.4)),
#   "isReal"=c(T,T,T,T,F,T,T)
# )
# timeseries$`401898a024310399210beb4d7f7bb890c568714cc006e7e6429cc6869ccb215c` <- gas_df1
# timeseries$`501898a024310399210beb4d7f7bb890c568714cc006e7e6429cc6869ccb215c` <- gas_df2
# 
# jsonlite::write_json(timeseries,"~/Nextcloud/Beegroup/Projects/Desenvolupaments_TIC_BIGG-ENTRACK/6_Data_analytics/data_example/one_building_series2.json")

# plot(pad_df$time[1:(24*12)],pad_df$value[1:(24*12)],type="l")
# head(pad_df)
# pad_df$time <- lubridate::round_date(
#   pad_df$time,unit = period_to_text("PT5M",only_first = T),
#   week_start = getOption("lubridate.week.start", 7)
# )
# head(pad_df)
# lines(pad_df$time,pad_df$value,col="red")

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

iso8601_period_to_timedelta <- function(x){
  x <- lubridate::period(x)
  return(years(x@year) + months(x@month) + days(x@day) +
           hours(x@hour) + minutes(x@minute) + seconds(lubridate::second(x)))
}

read_and_transform_sensor <- function(timeseriesObject, buildingsRdf, sensorId, tz,
                                      outputFrequency, aggFunctions,
                                      useEstimatedValues){
  
  # Get period and aggregation function specific for the timeseries
  metadata <- get_sensor_metadata(buildingsRdf, sensorId, tz)
  metadata_tariff <- get_tariff_metadata(buildingsRdf, sensorId)
  metadata_emissions <- get_emissions_metadata(buildingsRdf, sensorId)
  
  # Override condition if the estimated values should be used considering the 
  # estimation method defined in the sensor metadata
  if(metadata$considerEstimatedValues==T && useEstimatedValues==F)
    useEstimatedValues <- T
  
  # If timeseriesObject is NULL, read certain sensor
  if(is.character(timeseriesObject)){
    jsonFiles <- list.files(timeseriesObject,"json",full.names=T)
    timeseriesObject_ <- unlist(lapply(
      jsonFiles[grepl(sensorId,jsonFiles)],
      function(x){jsonlite::fromJSON(x)}),recursive=F)
  } else {
    timeseriesObject_ <- timeseriesObject
  }
  
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
    
    timesteps <- list(
      "1" = "S",
      "60" = "T",
      "3600" = "H",
      "86400" = "D",
      "604800" = "W",
      "2419200" = "M",
      "2505600" = "M",
      "2592000" = "M",
      "2678400" = "M",
      "31536000" = "Y",
      "31622400" = "Y"
    )
    
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
      interpolateFrequency <- lubridate::format_ISO8601(lubridate::as.period(
        quantile(difftime(timeseriesSensor$end,timeseriesSensor$start,
                          tz = "UTC",units = "secs"),
            0.1,na.rm=T)))
    }
    metadata$timeSeriesFrequency <- interpolateFrequency
    # Detect the subsets with no internal gaps
    timeseriesSensor$gapAfter <- ifelse(
      difftime(timeseriesSensor$start,lag(timeseriesSensor$end,1),units = "secs") > as.numeric(names(interpolateFrequency)),
      1,0)
    timeseriesSensor$gapAfter <- cumsum(ifelse(is.na(timeseriesSensor$gapAfter),0,timeseriesSensor$gapAfter))
    # Resample the original irregular series to a regular series, among the detected subsets
    dfs <- lapply(split(timeseriesSensor,timeseriesSensor$gapAfter), function(tsChunk){
      if("AVG" %in% aggFunctions){
        tsChunk$iniValue <- tsChunk$value
        tsChunk$iniIsReal <- tsChunk$isReal
      } else {
        tsChunk$value <- cumsum(tsChunk$value)
        tsChunk$iniValue <- lag(tsChunk$value,1)
        tsChunk$iniIsReal <- lag(tsChunk$isReal,1)
        tsChunk$isReal[which.max(tsChunk$isReal)] <- useEstimatedValues
        tsChunk$iniIsReal[1] <- (useEstimatedValues || metadata$timeSeriesIsCumulative)
        tsChunk$iniValue[1] <- if (useEstimatedValues || metadata$timeSeriesIsCumulative) {0} else {NA}
      }
      if(nrow(tsChunk)==1 && is.na(tsChunk$iniValue)){
        return(NULL)
      }
      tsChunk <- rbind(
        data.frame("time"=tsChunk$start,"value"=tsChunk$iniValue,"isReal"=tsChunk$iniIsReal),
        data.frame("time"=tsChunk$end,"value"=tsChunk$value,"isReal"=tsChunk$isReal))
      tsChunk$time <- lubridate::round_date(
        tsChunk$time,
        unit = iso8601_period_to_text(interpolateFrequency,only_first = T),
        week_start = getOption("lubridate.week.start", 7))
      tsChunk <- tsChunk[!duplicated(tsChunk$time),]
      tsChunk <- tsChunk[order(tsChunk$time),]
      if(useEstimatedValues==F) tsChunk <- tsChunk[tsChunk$isReal==T,]
      tsChunk <- tsChunk %>% 
        padr::pad(interval = iso8601_period_to_text(interpolateFrequency,only_first = T),
                  by = "time") %>%
        mutate(#time=as.POSIXct(lubridate::with_tz(time,"UTC"),tz="UTC"),
               value=zoo::na.approx(value),
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
        summarise(value = func(value))
    )
    timeseriesSensor$isReal <- T
  
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
  aggFunctions <- if(lubridate::period(outputFrequency)<lubridate::period("P1D")){
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
    useEstimatedValues = useEstimatedValues,
    tz = metadata$tz
  )
  
  # Add energy cost component
  if(nrow(metadata_tariff)>0){
    timeseriesSensor <- append_cost_to_sensor(
      buildingsRdf, timeseriesObject, 
      tariffUri = metadata_tariff$tariff,
      measuredProperty = metadata$measuredProperty,
      frequency = metadata$timeSeriesFrequency,
      energyTimeseriesSensor = timeseriesSensor)
  }
  # Add emissions component
  if(nrow(metadata_emissions)>0){
    timeseriesSensor <- append_emissions_to_sensor(
      buildingsRdf, timeseriesObject, 
      emissionsUri = metadata_emissions$emissions,
      measuredProperty = metadata$measuredProperty,
      frequency = metadata$timeSeriesFrequency,
      energyTimeseriesSensor = timeseriesSensor)
  }
  
  timeseriesSensor
  
  return(timeseriesSensor)
}

parse_device_aggregator_formula <- function(buildingsRdf, timeseriesObject, 
                                            buildingId, formula, inputFrequency, 
                                            outputFrequency, aggFunctions,
                                            useEstimatedValues, ratioCorrection=F){
  #formula <- "<mi>501898a024310399210beb4d7f7bb890c568714cc006e7e6429cc6869ccb215c</mi><mo>+</mo><mi>501898a024310399210beb4d7f7bb890c568714cc006e7e6429cc6869ccb215c</mi>"
  #formula <- devagg_buildings[4,3]
  result <- data.frame()
  op <- NULL
  tz <- get_tz_building(buildingsRdf, buildingId)
  while (formula!=""){
    if (substr(formula,1,4)=="<mi>"){
      res <- stringr::str_match(formula, "<mi>\\s*(.*?)\\s*</mi>")
      formula <- gsub(res[1,1],"",formula,fixed = T)
      aux_result <- eval(parse(text=
          paste0('read_and_transform_sensor(
            timeseriesObject = timeseriesObject,
            buildingsRdf = buildingsRdf,
            sensorId = "',res[1,2],'",
            tz = "',tz,'",
            outputFrequency = "',outputFrequency,'",
            aggFunctions = ',paste0('c("',paste(aggFunctions,collapse='","'),'")'),',
            useEstimatedValues = ',useEstimatedValues,'
          )')
        ))
      if(ratioCorrection){
        if(any(grepl("^SUM",colnames(aux_result)))){
          for (sum_col in colnames(aux_result)[grepl("^SUM",colnames(aux_result))]){
            aux_result[,sum_col] <- aux_result[,sum_col] / aux_result$RATIO
          }
        }
      }
      if(length(result)==0){
        result <- aux_result
      } else {
        elems <- colnames(result)
        result <- merge(result, aux_result, by="time", suffixes=c("_1","_2"), all=T)
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
        ## To do, update the following operators
        # } else if(op=="-") {
        #   result[,"value_2"] <- -result[,"value_2"]
        #   result[,"value"] <- rowSums(result[,c("value_1","value_2")],na.rm=T)
        # } else if(op=="*") {
        #   result[,"value"] <- matrixStats::rowProds(result[,c("value_1","value_2")],na.rm=T)
        # } else if(op=="/") {
        #   result[,"value_2"] <- 1/result[,"value_2"]
        #   result[,"value"] <- matrixStats::rowProds(result[,c("value_1","value_2")],na.rm=T)
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

get_device_aggregators_by_building <- function(buildingsRdf, timeseriesObject=NULL, 
                                               allowedBuildingId=NULL, 
                                               allowedDeviceAggregators=NULL, 
                                               useEstimatedValues=F,
                                               ratioCorrection=T){
  
  # Get formulas and associated metadata for each building and device aggregator
  devagg_buildings <- get_all_device_aggregators(buildingsRdf)
  
  # Filter by the allowed buildings
  if(!is.null(allowedBuildingId)){
    devagg_buildings <- devagg_buildings[
      devagg_buildings$buildingId %in% allowedBuildingId,
    ]
  }
  
  # Filter by the allowed device aggregator names
  if(!is.null(allowedDeviceAggregators)){
    devagg_buildings <- devagg_buildings[
      devagg_buildings$deviceAggregatorName %in% allowedDeviceAggregators,
    ]
  }
  
  all_buildings_timeseries <- 
    setNames(lapply(unique(devagg_buildings$buildingId),
      function(buildingId){
       #buildingId="04752"
       aux <- devagg_buildings[devagg_buildings$buildingId==buildingId,]
       aux$deviceAggregatorFrequency <- ifelse(
         is.na(aux$deviceAggregatorFrequency), 
         "P1Y", aux$deviceAggregatorFrequency)
       largerFrequency <- aux$deviceAggregatorFrequency[
         which.max(lubridate::seconds(lubridate::period(aux$deviceAggregatorFrequency)))]
       dfs <- setNames(lapply(unique(aux$deviceAggregatorName),
         function(devAggName){
           #devAggName = "totalGasConsumption"
           df <- parse_device_aggregator_formula(
             buildingsRdf = buildingsRdf,
             timeseriesObject = timeseriesObject,
             buildingId = buildingId,
             formula = as.character(unique(aux[aux$deviceAggregatorName==devAggName,
                                  "deviceAggregatorFormula"])),
             inputFrequency = as.character(unique(aux[aux$deviceAggregatorName==devAggName,
                          "deviceAggregatorFrequency"])),
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
       list(
         "df"=Reduce(function(df1, df2){merge(df1, df2, by = "time", all=T)}, dfs),
         "metadata"=devagg_buildings[devagg_buildings$buildingId==buildingId,]
       )
      }
    ), nm = unique(devagg_buildings$buildingId))
  
  return(all_buildings_timeseries)
}

# rdf <- function (storage = c("memory", "BDB", "sqlite", "postgres", 
#                              "mysql", "virtuoso"), host = NULL, port = NULL, user = NULL, 
#                  password = NULL, database = NULL, charset = NULL, dir = NULL, 
#                  dsn = "Local Virtuoso", name = "rdflib", new_db = FALSE, 
#                  fallback = TRUE) 
# {
#   world <- new("World")
#   store <- rdflib:::rdf_storage(storage, world, host, port, user, password, 
#                        database, charset, dir, dsn, name, new_db, fallback)
#   model <- new("Model", world = world, storage = store, options = "")
#   structure(list(world = world, model = model, storage = store), 
#             class = "rdf")
# }
# 
# rdf_parse <- function (doc, format = c("guess", "rdfxml", "nquads", "ntriples", 
#                           "turtle", "jsonld"), rdf = NULL, base = getOption("rdf_base_uri", 
#                                                                             "localhost://"), ...) 
# {
#   format <- match.arg(format)
#   if (format == "guess") {
#     format <- guess_format(doc)
#   }
#   tmp_string <- tempfile()
#   tmp_json <- tempfile()
#   doc <- rdflib:::text_or_url_to_doc(doc, tmp_string)
#   if (format == "jsonld") {
#     x <- jsonld::jsonld_to_rdf(doc, options = list(base = getOption("rdf_base_uri", 
#                                                                     "localhost://"), format = "application/nquads"))
#     writeLines(x, tmp_json)
#     format <- "nquads"
#     doc <- tmp_json
#   }
#   if (is.null(rdf)) {
#     rdf <- rdf()
#   }
#   mimetype <- unname(rdf_mimetypes[format])
#   parser <- new("Parser", rdf$world, name = format, mimeType = mimetype)
#   redland::parseFileIntoModel(parser, rdf$world, doc, rdf$model, 
#                               baseUri = base)
#   redland::freeParser(parser)
#   unlink(tmp_string)
#   unlink(tmp_json)
#   rdf
# }

namespace_integrator <- function(items, namespaces=NULL){
  if(is.null(namespaces)){
    return(items)
  } else {
    return(mapply(function(k){
      nm <- namespaces[mapply(function(i)grepl(paste0("^",i,":"),k),names(namespaces))]
      if(length(nm)==0){ k } else { gsub(paste0("^",names(nm),":"),nm,k) }
    },items))
  }
}

add_item_to_rdf <- function(object, subject, classes = NULL, dataProperties = NULL, 
                            objectProperties = NULL, namespaces=NULL){
  subject <- namespace_integrator(subject, namespaces)
  for(cl in namespace_integrator(classes, namespaces)){
    object %>% rdf_add(
      subject = subject,
      predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
      object = cl,
      subjectType = "uri",objectType = "uri"
    )
  }
  if(!is.null(dataProperties)){
    for(i in 1:length(dataProperties)){
      value <- dataProperties[[i]]
      datetimeDetected <- class(value)[1]=="POSIXct"
      object %>% rdf_add(
        subject = subject,
        predicate = namespace_integrator(names(dataProperties)[i],namespaces),
        object = if(datetimeDetected){parsedate::format_iso_8601(value)} else {value},
        subjectType = "uri",objectType = "literal",
        datatype_uri = if(datetimeDetected){"xsd:dateTime"
        } else {NA}
      )
    }
  }
  if(!is.null(objectProperties)){
    for(i in 1:length(objectProperties)){
      object %>% rdf_add(
        subject = subject,
        predicate = namespace_integrator(names(objectProperties)[i],namespaces),
        object = namespace_integrator(objectProperties[[i]],namespaces),
        subjectType = "uri",objectType = "uri"
      )
    }
  }
  return(object)
}

