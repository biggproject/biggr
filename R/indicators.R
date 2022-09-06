generate_longitudinal_benchmarking_indicators <- function(
  data, indicators, energyType, energyComponent, frequencies, 
  buildingId, buildingSubject, timeColumn, localTimeZone, 
  consumptionColumn, inputRDF=NULL, getResultantRDF=T, writeResultantRDF=F,
  buildingGrossFloorArea = 0, outdoorTemperatureColumn = NULL,
  heatingDegreeDays18Column = NULL, coolingDegreeDays21Column = NULL,
  carbonEmissionsColumn = NULL, energyPriceColumn = NULL, 
  dailyFixedPriceColumn = NULL, modelName = NULL, modelId = NULL,
  modelLocation = NULL, estimateWhenAggregate = T, outputDirectory = ""){
  
  # data = df
  # inputRDF = NULL
  # indicators = unlist(settings$Indicators)
  # energyComponent = "Total"
  # energyType = energyType
  # frequencies = settings$Frequencies[
  #   as.period(settings$Frequencies)>=as.period(detect_time_step(df$time))]
  # buildingId = buildingId
  # buildingSubject = buildingSubject
  # localTimeZone = tz
  # timeColumn = "time"
  # consumptionColumn = "Qe"
  # writeResultantRDF = F
  # getResultantRDF = T
  # buildingGrossFloorArea = buildingArea
  # outdoorTemperatureColumn = "temperature"
  # heatingDegreeDays18Column = if(sum(colnames(df) %in% c("HDD18"))>0) {
  #   colnames(df)[colnames(df) %in% c("HDD18")]
  # } else {NULL}
  # coolingDegreeDays21Column = if(sum(colnames(df) %in% c("CDD21"))>0) {
  #   colnames(df)[colnames(df) %in% c("CDD21")]
  # } else {NULL}
  # carbonEmissionsColumn = NULL
  # energyPriceColumn = NULL
  # dailyFixedPriceColumn = NULL
  # modelName = NULL
  # modelId = NULL
  # estimateWhenAggregate = T
  # outputDirectory = settings$OutputDataDirectory
  
  buildingNamespace <- paste0(strsplit(buildingSubject,"#")[[1]][1],"#")
  namespaces <- bigg_namespaces
  namespaces["biggresults"] <- buildingNamespace
  
  obj <- if(is.null(inputRDF)){rdf()} else {inputRDF}
  
  # Create the analytical model to RDF, if needed
  
  if(!is.null(modelId)){
    
    modelSubject <- sprintf(
      "biggresults:MODEL-%s-%s", buildingId, modelId)
    
    obj %>%
      add_item_to_rdf(
        subject = modelSubject,
        classes = c("bigg:AnalyticalModel"),
        dataProperties = list(
          "bigg:modelLocation"= modelLocation,
          #"bigg:modelId"= modelId,
          "bigg:modelName"= modelName),
        objectProperties = list(
          "bigg:hasModelStorageInfrastructure" = "bigg:MODELINFRA-MLFlow"),
        namespaces = namespaces
      )
    
    # Link the building with the model
    obj %>%
      add_item_to_rdf(
        subject = buildingSubject,
        objectProperties = list("bigg:hasAnalyticalModel"= modelSubject),
        namespaces = namespaces
      )
  }
  
  for (indicator in indicators){
    
    frequencies_ <- frequencies
    originalDataPeriod <- detect_time_step(data[,timeColumn])
    
    valueInd <- 
      if( indicator == "EnergyUse" ){
        data[,consumptionColumn]
        
      } else if( indicator == "EnergyUseIntensity" && buildingGrossFloorArea>0 ){
        data[,consumptionColumn] / buildingGrossFloorArea
        
      } else if( indicator == "EnergyCost" && !is.null(energyPriceColumn) &&
                 !is.null(dailyFixedPriceColumn)){
        (data[,consumptionColumn] * data[,energyPriceColumn]) + 
          data[,dailyFixedPriceColumn]
        
      } else if( indicator == "EnergyCostIntensity" && !is.null(energyPriceColumn) &&
                 !is.null(dailyFixedPriceColumn) && buildingGrossFloorArea>0){
        ((data[,consumptionColumn] * data[,energyPriceColumn]) + 
          data[,dailyFixedPriceColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "CarbonEmissions" && !is.null(carbonEmissionsColumn)){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn])
        
      } else if( indicator == "CarbonEmissionsIntensity" && !is.null(carbonEmissionsColumn) && 
                 buildingGrossFloorArea>0){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn]) /
          buildingGrossFloorArea
      } else if( indicator == "HeatingDegreeDays" && !is.null(heatingDegreeDays18Column) ){
        # Method: https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm
        ifelse(data[,heatingDegreeDays18Column]>3,data[,heatingDegreeDays18Column],0)
      } else if( indicator == "CoolingDegreeDays" && !is.null(coolingDegreeDays21Column) ){
        # Method: https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm
        ifelse(data[,coolingDegreeDays21Column]>3,data[,coolingDegreeDays21Column],0)
      } else {
        NULL
      }
    
    # In case the HDD or CDD needs to be computed (When frequency is higher or equal to P1D)
    # Method: https://ec.europa.eu/eurostat/cache/metadata/en/nrg_chdd_esms.htm
    if( (indicator %in% c("HeatingDegreeDays","CoolingDegreeDays")) && !is.null(outdoorTemperatureColumn) && 
        is.null(valueInd) ){
      indDf <- data.frame("time" = as.Date(data[,timeColumn],tz=localTimeZone),
                          "temperature" = data[,outdoorTemperatureColumn]) %>% 
        group_by(time) %>%
        summarise(temperature=mean(temperature)) %>%
        mutate(degree_days(.,"temperature",localTimeZone,
                           if(indicator=="HeatingDegreeDays"){21}else{18},
                           if(indicator=="HeatingDegreeDays"){"heating"}else{"cooling"},
                           outputFrequency = "P1D", outputFeaturesName = "ind",
                           fixedOutputFeaturesName = T)) %>%
        select(time,ind)
      frequencies_ <- frequencies[
        frequencies>=as.period("P1D")]
      originalDataPeriod <- "P1D"
    # All the other cases with data
    } else if (!is.null(valueInd)){
      indDf <- data.frame(
        "time" = data[,timeColumn],
        "ind" = valueInd
      )
    # Case when no data is available
    } else {
      next
    }
    
    for(frequency in frequencies_){
      n <- hourly_timesteps(as.numeric(as.period(frequency))/3600,
                            originalDataPeriod)
      indDfAux <- indDf %>% 
        group_by(
          start=floor_date(time,unit = frequency,
                     week_start = getOption("lubridate.week.start", 1))
        ) %>%
        summarise(
          estimated = mean(ind,na.rm=T)*n,
          real = sum(ind)
        ) %>% mutate(
          value = if(estimateWhenAggregate==T){
            estimated
          } else {
            real
          },
          isReal = if(is.null(modelId)){
            ifelse(is.finite(real),T,
                   ifelse(is.finite(value),F,NA))
          } else {
            ifelse(is.finite(value),F,NA)
          }
        ) %>% select(-real,-estimated)
      indDfAux$start <- with_tz(indDfAux$start, "UTC")
      indDfAux$end <- with_tz(
        with_tz(indDfAux$start,localTimeZone) + 
          iso8601_period_to_timedelta(frequency) - seconds(1),
        "UTC"
      )
      
      # Create the SingleKPIAssessment object
      keyPerformanceIndicatorName <- if(indicator %in% c("HeatingDegreeDays","CoolingDegreeDays")){
        indicator
      } else {
        paste(indicator, energyComponent, energyType, sep=".")
      }
      keyPerformanceIndicatorSubject <- paste("bigg:KPI",
                                                keyPerformanceIndicatorName,
                                              sep="-")
      singleKPISubject <- paste("biggresults:SingleKPI",
                                  buildingId, keyPerformanceIndicatorName,
                                  modelName, modelId, frequency, sep="-")
      singleKPISubjectHash <- digest(
        namespace_integrator(singleKPISubject, namespaces), "sha256", 
        serialize=T
      )
      singleKPIPointSubject <- paste0("biggresults:",singleKPISubjectHash)
        
      obj %>%
        add_item_to_rdf(
          subject = singleKPISubject,
          classes = c("bigg:SingleKPIAssessment",
                      "bigg:KPIAssessment",
                      "bigg:TimeSeriesList"),
          dataProperties = list(
            "bigg:timeSeriesIsRegular" = T,
            "bigg:timeSeriesIsOnChange" = F,
            "bigg:timeSeriesIsCumulative" = F,
            "bigg:timeSeriesStart" = min(indDfAux$start,na.rm=T),
            "bigg:timeSeriesEnd" = max(indDfAux$end,na.rm=T),
            "bigg:timeSeriesFrequency" = frequency,
            "bigg:timeSeriesTimeAggregationFunction" = "SUM"
          ),
          objectProperties = if(is.null(modelId)){
              list(
                "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
                "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject
              )
            } else {
              list(
                "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
                "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject,
                "bigg:estimatesKPI" = modelSubject
              )
            },
          namespaces = namespaces
        )
      # Link the building with the SingleKPIAssessment
      obj %>%
        add_item_to_rdf(
          subject = buildingSubject,
          objectProperties = list("bigg:assessesSingleKPI"= singleKPISubject),
          namespaces = namespaces
        )
      
      # Write the output JSON file
      indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
      indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
      dir.create(outputDirectory,F)
      
      write(jsonlite::toJSON(
        setNames(list(indDfAux),singleKPISubjectHash),
        dataframe = "rows",na = "null"),
            file=paste(outputDirectory,
                       sprintf("%s.json",singleKPISubjectHash),
                       sep="/") )
    }
  }
  
  # Output RDF
  if(writeResultantRDF){
    write_rdf(
      object = obj,
      file = paste(outputDirectory,
                   sprintf("%s.ttl", digest(
                     paste0(buildingSubject, modelName, modelId, indicators,
                            energyComponent, energyType, collapse="~"),
                     algo = "sha256", serialize = T)), sep="/"))
  } 
  if (getResultantRDF){
    return(obj)
  }
}
´