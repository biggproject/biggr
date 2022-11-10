generate_longitudinal_benchmarking_indicators <- function(
  data, indicators, energyType, energyComponent, frequencies, 
  buildingId, buildingSubject, timeColumn, localTimeZone, 
  consumptionColumn, indicatorsUnitsURIs, baselineConsumptionColumn = NULL, 
  inputRDF = NULL, getResultantRDF=T, writeResultantRDF=F,
  buildingGrossFloorArea = 0, outdoorTemperatureColumn = NULL,
  heatingDegreeDays18Column = NULL, coolingDegreeDays21Column = NULL,
  carbonEmissionsColumn = NULL, energyPriceColumn = NULL,
  modelName = NULL, modelId = NULL,
  modelLocation = NULL, modelStorageInfrastructureURI=NULL,
  modelTypeURI = NULL, modelBaselineYear=NULL, estimateWhenAggregate = T, outputDirectory = ""){
  
  # data = model$data
  # indicators = unlist(settings$TrendIndicators)
  # indicatorsUnitsURIs = settings$IndicatorsUnitsURIs
  # energyComponent = names(energyComponentsColumns)[i]
  # energyType = energyType
  # frequencies = settings$Frequencies[
  #   as.period(settings$Frequencies) >= as.period(detect_time_step(model$data$time))]
  # buildingId = buildingId
  # buildingSubject = buildingSubject
  # inputRDF = resultantRdf
  # localTimeZone = tz
  # timeColumn = "time"
  # consumptionColumn = energyComponentsColumns[i]
  # energyPriceColumn = "Qe_price"
  # carbonEmissionsColumn = "Qe_emissionsFactor"
  # baselineConsumptionColumn = baselineEnergyComponentsColumns[i]
  # writeResultantRDF = F
  # getResultantRDF = T
  # buildingGrossFloorArea = buildingArea
  # # outdoorTemperatureColumn =
  # # carbonEmissionsColumn =
  # # energyPriceColumn =
  # # dailyFixedPriceColumn =
  # modelName = model$name
  # modelId = model$id
  # modelLocation = model$uri
  # modelTypeURI = sprintf("bigg:MODELTYPE-%s",modelType)
  # modelStorageInfrastructureURI = "bigg:MODELINFRA-MLFlow"
  # estimateWhenAggregate = T
  # outputDirectory = settings$OutputDataDirectory
  # modelBaselineYear=NULL
  
  buildingNamespace <- paste0(strsplit(buildingSubject,"#")[[1]][1],"#")
  namespaces <- bigg_namespaces
  namespaces["biggresults"] <- buildingNamespace
  
  obj <- if(is.null(inputRDF)){rdf()} else {inputRDF}
  
  # Create the analytical model to RDF, if needed
  
  if(!is.null(modelId)){
    
    modelSubject <- sprintf(
      "biggresults:MODEL-%s-%s", buildingId, modelId)
    
    if(!exists_analytical_model(obj, modelSubject, namespaces)){
      # Create the model object
      obj <- obj %>%
        add_item_to_rdf(
          subject = modelSubject,
          classes = c("bigg:AnalyticalModel"),
          dataProperties = if(is.null(modelBaselineYear)){
              list(
                "bigg:modelLocation"= modelLocation,
                "bigg:modelTrainedDate"= parsedate::format_iso_8601(lubridate::now("UTC")),
                "bigg:modelName"= modelName
              )
            } else {
              list(
                "bigg:modelLocation"= modelLocation,
                "bigg:modelTrainedDate"= parsedate::format_iso_8601(lubridate::now("UTC")),
                "bigg:modelBaselineYear" = modelBaselineYear,
                "bigg:modelName"= modelName
              )
            },
          objectProperties = list(
            "bigg:hasModelStorageInfrastructure" = modelStorageInfrastructureURI,
            "bigg:hasModelType"= modelTypeURI
          ),
          namespaces = namespaces
        )
      # Link the building with the model
      obj <- obj %>%
        add_item_to_rdf(
          subject = buildingSubject,
          objectProperties = list("bigg:hasAnalyticalModel"= modelSubject),
          namespaces = namespaces
        )
    }
  }
  
  for (indicator in indicators){
    
    frequencies_ <- frequencies
    originalDataPeriod <- detect_time_step(data[,timeColumn])
    
    valueInd <- 
      if( indicator == "EnergyUse"){
        data[,consumptionColumn]
        
      } else if( indicator == "EnergyUseIntensity" && buildingGrossFloorArea>0 ){
        data[,consumptionColumn] / buildingGrossFloorArea
      
      } else if( indicator == "EnergyUseSavings" ){
        data[,consumptionColumn]-data[,baselineConsumptionColumn]
        
      } else if( indicator == "EnergyUseSavingsIntensity" && buildingGrossFloorArea>0 ){
        (data[,consumptionColumn]-data[,baselineConsumptionColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyCost" && !is.null(energyPriceColumn)){
        (data[,consumptionColumn] * data[,energyPriceColumn])
        
      } else if( indicator == "EnergyCostIntensity" && !is.null(energyPriceColumn) &&
                 buildingGrossFloorArea>0){
        (data[,consumptionColumn] * data[,energyPriceColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyCostSavings" && !is.null(energyPriceColumn)){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,energyPriceColumn])
        
      } else if( indicator == "EnergyCostSavingsIntensity" && !is.null(energyPriceColumn) && 
                 buildingGrossFloorArea>0){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,energyPriceColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyEmissions" && !is.null(carbonEmissionsColumn)){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn])
        
      } else if( indicator == "EnergyEmissionsIntensity" && !is.null(carbonEmissionsColumn) && 
                 buildingGrossFloorArea>0){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn]) /
          buildingGrossFloorArea
        
      } else if( indicator == "EnergyEmissionsSavings" && !is.null(carbonEmissionsColumn)){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,carbonEmissionsColumn])
        
      } else if( indicator == "EnergyEmissionsSavingsIntensity" && !is.null(carbonEmissionsColumn) && 
                 buildingGrossFloorArea>0){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,carbonEmissionsColumn]) /
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
      if(as.period(frequency) >= as.period("P1D")){
        indDfAux$end <- with_tz(
          with_tz(indDfAux$start,localTimeZone) + 
            iso8601_period_to_timedelta(frequency) - seconds(1),
          "UTC"
        )
      } else {
        indDfAux$end <- indDfAux$start + 
          iso8601_period_to_timedelta(frequency) - seconds(1)
      }
      
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
              "bigg:hasKPIUnit" = indicatorsUnitsURIs[[indicator]],
              "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
              "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject
            )
          } else {
            list(
              "bigg:hasKPIUnit" = indicatorsUnitsURIs[[indicator]],
              "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
              "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject,
              "bigg:isEstimatedByModel" = modelSubject
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


generate_cross_sectional_benchmarking_indicator <- function(
  data, indicator, frequencies, groupSubject, timeColumn, 
  indicatorsUnitsURIs, inputRDF = NULL, getResultantRDF=T, 
  writeResultantRDF=F, estimateWhenAggregate = T, outputDirectory = ""){
  
  # data = model$data
  # indicator = KPI$
  # indicatorsUnitsURIs = settings$IndicatorsUnitsURIs
  # energyComponent = names(energyComponentsColumns)[i]
  # energyType = energyType
  # frequencies = settings$Frequencies[
  #   as.period(settings$Frequencies) >= as.period(detect_time_step(df$time))]
  # buildingId = buildingId
  # buildingSubject = buildingSubject
  # inputRDF = resultantRdf
  # localTimeZone = tz
  # timeColumn = "time"
  # consumptionColumn = energyComponentsColumns[i]
  # energyPriceColumn = "Qe_price"
  # carbonEmissionsColumn = "Qe_emissionsFactor"
  # baselineConsumptionColumn = baselineEnergyComponentsColumns[i]
  # writeResultantRDF = F
  # getResultantRDF = T
  # buildingGrossFloorArea = buildingArea
  # # outdoorTemperatureColumn = 
  # # carbonEmissionsColumn = 
  # # energyPriceColumn = 
  # # dailyFixedPriceColumn =
  # modelName = model$name
  # modelId = model$id
  # modelLocation = model$uri
  # modelTypeURI = sprintf("bigg:MODELTYPE-%s",modelType)
  # modelStorageInfrastructureURI = "bigg:MODELINFRA-MLFlow"
  # estimateWhenAggregate = T
  # outputDirectory = settings$OutputDataDirectory
  # modelBaselineYear=NULL
  
  buildingNamespace <- paste0(strsplit(buildingSubject,"#")[[1]][1],"#")
  namespaces <- bigg_namespaces
  namespaces["biggresults"] <- buildingNamespace
  
  obj <- if(is.null(inputRDF)){rdf()} else {inputRDF}
  
  for (indicator in indicators){
    
    frequencies_ <- frequencies
    originalDataPeriod <- detect_time_step(data[,timeColumn])
    
    valueInd <- 
      if( indicator == "EnergyUse"){
        data[,consumptionColumn]
        
      } else if( indicator == "EnergyUseIntensity" && buildingGrossFloorArea>0 ){
        data[,consumptionColumn] / buildingGrossFloorArea
        
      } else if( indicator == "EnergyUseSavings" ){
        data[,consumptionColumn]-data[,baselineConsumptionColumn]
        
      } else if( indicator == "EnergyUseSavingsIntensity" && buildingGrossFloorArea>0 ){
        (data[,consumptionColumn]-data[,baselineConsumptionColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyCost" && !is.null(energyPriceColumn)){
        (data[,consumptionColumn] * data[,energyPriceColumn])
        
      } else if( indicator == "EnergyCostIntensity" && !is.null(energyPriceColumn) &&
                 buildingGrossFloorArea>0){
        (data[,consumptionColumn] * data[,energyPriceColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyCostSavings" && !is.null(energyPriceColumn)){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,energyPriceColumn])
        
      } else if( indicator == "EnergyCostSavingsIntensity" && !is.null(energyPriceColumn) && 
                 buildingGrossFloorArea>0){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,energyPriceColumn]) / buildingGrossFloorArea
        
      } else if( indicator == "EnergyEmissions" && !is.null(carbonEmissionsColumn)){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn])
        
      } else if( indicator == "EnergyEmissionsIntensity" && !is.null(carbonEmissionsColumn) && 
                 buildingGrossFloorArea>0){
        (data[,consumptionColumn] * data[,carbonEmissionsColumn]) /
          buildingGrossFloorArea
        
      } else if( indicator == "EnergyEmissionsSavings" && !is.null(carbonEmissionsColumn)){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,carbonEmissionsColumn])
        
      } else if( indicator == "EnergyEmissionsSavingsIntensity" && !is.null(carbonEmissionsColumn) && 
                 buildingGrossFloorArea>0){
        ((data[,consumptionColumn]-data[,baselineConsumptionColumn]) * data[,carbonEmissionsColumn]) /
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
      if(as.period(frequency) >= as.period("P1D")){
        indDfAux$end <- with_tz(
          with_tz(indDfAux$start,localTimeZone) + 
            iso8601_period_to_timedelta(frequency) - seconds(1),
          "UTC"
        )
      } else {
        indDfAux$end <- indDfAux$start + 
          iso8601_period_to_timedelta(frequency) - seconds(1)
      }
      
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
              "bigg:hasKPIUnit" = indicatorsUnitsURIs[[indicator]],
              "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
              "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject
            )
          } else {
            list(
              "bigg:hasKPIUnit" = indicatorsUnitsURIs[[indicator]],
              "bigg:hasSingleKPIPoint" = singleKPIPointSubject,
              "bigg:quantifiesKPI" = keyPerformanceIndicatorSubject,
              "bigg:isEstimatedByModel" = modelSubject
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
