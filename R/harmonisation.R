get_all_device_aggregators <- function(buildingsRdf){
  result <- suppressMessages(buildingsRdf %>% rdf_query(paste0('
    PREFIX ns0: <https://bigg-project.eu/ontology#>
    PREFIX ns1: <https://sws.geonames.org/>
    SELECT ?buildingId ?deviceAggregatorName ?deviceAggregatorFormula ?deviceAggregatorFrequency ?deviceAggregatorTimeAggregationFunction
    WHERE {
      ?b a ns0:Building .
      ?b ns0:buildingIDFromOrganization ?buildingId .
      ?b ns0:hasSpace ?bs .
      ?bs ns0:hasDeviceAggregator ?s .
      optional {?s ns0:deviceAggregatorName ?deviceAggregatorName .}
      optional {?s ns0:deviceAggregatorFrequency ?deviceAggregatorFrequency .}
      optional {?s ns0:deviceAggregatorTimeAggregationFunction ?deviceAggregatorTimeAggregationFunction .}
      optional {?s ns0:deviceAggregatorFormula ?deviceAggregatorFormula .}
    }')))
  return(result)
}

get_tz_building <- function(buildingsRdf, buildingId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0('
        PREFIX ns0: <https://bigg-project.eu/ontology#>
        SELECT ?tz
        WHERE {
          ?b a ns0:Building .
          ?b ns0:buildingIDFromOrganization "',buildingId,'" .
          ?b ns0:hasLocationInfo ?l .
          ?l ns0:addressLocalTimeZone ?tz .
        }')))
  return( if(length(metadata_df)>0) {as.character(metadata_df$tz)} else {NULL} )
}

get_sensor_metadata <- function(buildingsRdf, sensorId){
  metadata_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0('
        PREFIX ns0: <https://bigg-project.eu/ontology#>
        PREFIX ns1: <https://sws.geonames.org/>
        SELECT ?sensorId ?sensorFrequency ?sensorIsCumulative 
          ?sensorTimeAggregationFunction ?sensorIsOnChange ?sensorIsRegular
          ?sensorProperty ?sensorEstimationMethod ?tz
        WHERE {
          ?b a ns0:Building .
          ?b ns0:hasLocationInfo ?l .
          ?l ns0:addressLocalTimeZone ?tz .
          ?b ns0:hasSpace ?bs .
          ?bs ns0:isObservedBy ?d .
          ?d ns0:hasSensor ?m .
          ?m ns0:sensorId ?sensorId .
          FILTER regex(?sensorId, "',sensorId,'")
          optional {?m ns0:sensorTimeAggregationFunction ?sensorTimeAggregationFunction .}
          optional {?m ns0:sensorIsCumulative ?sensorIsCumulative .}
          optional {?m ns0:sensorIsOnChange ?sensorIsOnChange .}
          optional {?m ns0:sensorIsRegular ?sensorIsRegular .}
          optional {?m ns0:sensorProperty ?sensorProperty .}
          optional {?m ns0:sensorFrequency ?sensorFrequency .}
          optional {?m ns0:sensorEstimationMethod ?sensorEstimationMethod .}
        }')))
  return(metadata_df)
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

read_and_transform_sensor <- function(timeseriesObject, buildingsRdf, sensorId,
                                      outputFrequency, aggFunctions,
                                      useEstimatedValues){
  
  # Get period and aggregation function specific for the timeseries
  metadata <- get_sensor_metadata(buildingsRdf, sensorId)
  
  timeseriesSensor <- timeseriesObject[sensorId][[1]]
  timeseriesSensor$start <- as.POSIXct(
    timeseriesSensor$start, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  timeseriesSensor$end <- as.POSIXct(
    timeseriesSensor$end, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  timeseriesSensor <- timeseriesSensor[order(timeseriesSensor$start),]
  if(!("isReal" %in% colnames(timeseriesSensor))){
    timeseriesSensor$isReal <- T
  }
  
  # Non-regular timeseries
  if(metadata$sensorIsRegular==F){
    # If value data is cumulative, integrate the series
    if(metadata$sensorIsCumulative){
      timeseriesSensor$start <- lag(timeseriesSensor$start,1)
      timeseriesSensor$end <- timeseriesSensor$end - lubridate::seconds(1)
      timeseriesSensor$value <- timeseriesSensor$value - lag(timeseriesSensor$value,1)
      timeseriesSensor <- timeseriesSensor[is.finite(timeseriesSensor$start),]
    }
    # If values are on change, reconsider the start and end timestamps
    if(metadata$sensorIsOnChange){
      aux <- timeseriesSensor$end + lubridate::period(metadata$sensorFrequency)
      aux2 <- aux < lead(timeseriesSensor$start,1)
      aux2[is.na(aux2)] <- T
      timeseriesSensor$end <- lubridate::as_datetime(
        ifelse(aux2, aux, lead(timeseriesSensor$start,1)))
      interpolateFrequency <- tail(unlist(biggr:::timesteps)[
        as.numeric(names(biggr:::timesteps)) <= 
          min(difftime(lead(timeseriesSensor$start,1),timeseriesSensor$start,tz = "UTC",units = "secs"),
              na.rm=T)],1)
    } else {
      # Calculate the minimum frequency for series interpolation
      interpolateFrequency <- tail(unlist(biggr:::timesteps)[
        as.numeric(names(biggr:::timesteps)) <= 
          min(difftime(timeseriesSensor$end,timeseriesSensor$start,tz = "UTC",units = "secs"),
              na.rm=T)],1)
    }
    metadata$sensorFrequency <- interpolateFrequency
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
        tsChunk$iniIsReal[1] <- (useEstimatedValues || metadata$sensorIsCumulative)
        tsChunk$iniValue[1] <- if (useEstimatedValues || metadata$sensorIsCumulative) {0} else {NA}
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
        metadata$sensorFrequency,only_first = T))
  }
  # Align time grid
  timeseriesSensor$time <- lubridate::round_date(
    timeseriesSensor$time,
    unit = iso8601_period_to_text(metadata$sensorFrequency,only_first = T),
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
    # if(any(c("HDD", "CDD") %in% aggFunctions)){
    #   unique(c("AVG",aggFunctions[aggFunctions %in% c("SUM","MIN","MAX")]))
    # } else { 
    #   aggFunctions[aggFunctions %in% c("SUM","AVG","MIN","MAX")] 
    # },
    useEstimatedValues = useEstimatedValues,
    tz = metadata$tz
  )
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
  while (formula!=""){
    if (substr(formula,1,4)=="<mi>"){
      res <- stringr::str_match(formula, "<mi>\\s*(.*?)\\s*</mi>")
      formula <- gsub(res[1,1],"",formula,fixed = T)
      aux_result <- eval(parse(text=
          paste0('read_and_transform_sensor(
            timeseriesObject = timeseriesObject,
            buildingsRdf = buildingsRdf,
            sensorId = "',res[1,2],'",
            outputFrequency = "',outputFrequency,'",
            aggFunctions = ',paste0('c("',paste(aggFunctions,collapse='","'),'")'),',
            useEstimatedValues = ',useEstimatedValues,'
          )')
        ))
      if(ratioCorrection){
        if("SUM" %in% colnames(aux_result)){
          aux_result$SUM <- aux_result$SUM / aux_result$RATIO
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
            if(elem %in% c("AVG","RATIO","GAPS")){
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
        result[,endsWith(colnames(result),"_1") | endsWith(colnames(result),"_2")] <- NULL
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

get_device_aggregators_by_building <- function(buildingsRdf, timeseriesObject, 
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
       #buildingId="00109"
       aux <- devagg_buildings[devagg_buildings$buildingId==buildingId,]
       largerFrequency <- aux$deviceAggregatorFrequency[
         which.max(lubridate::seconds(lubridate::period(aux$deviceAggregatorFrequency)))]
       dfs <- setNames(lapply(unique(aux$deviceAggregatorName),
         function(devAggName){
           #devAggName = "outdoorTemperature"
           df <- parse_device_aggregator_formula(
             buildingsRdf = buildingsRdf,
             timeseriesObject = timeseriesObject,
             buildingId = buildingId,
             formula = unique(aux[aux$deviceAggregatorName==devAggName,
                                  "deviceAggregatorFormula"]),
             inputFrequency = unique(aux[aux$deviceAggregatorName==devAggName,
                          "deviceAggregatorFrequency"]),
             outputFrequency = largerFrequency,
             aggFunctions = unlist(
               unique(aux[aux$deviceAggregatorName==devAggName,
                "deviceAggregatorTimeAggregationFunction"]),use.names = F),
             useEstimatedValues = useEstimatedValues,
             ratioCorrection = ratioCorrection
           )
           colnames(df) <- ifelse(colnames(df)=="time","time",
                             paste(devAggName, colnames(df), sep="."))
           df
         }), nm = unique(aux$deviceAggregatorName))
       Reduce(function(df1, df2){merge(df1, df2, by = "time", all=T)}, dfs)
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

