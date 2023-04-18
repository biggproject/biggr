get_eem_lifespan <- function(lifespans, eemTypes){
  
  defaultLifespan <- lifespans$default
  lifespans <- lifespans[-which(names(lifespans)=="default")]
  
  return (
    mapply(eemTypes,FUN=function(et){
      possibleLifespans <- mapply(names(lifespans), FUN=function(nl) grepl(nl,et,fixed=T))
      if(!any(possibleLifespans)){
        defaultLifespan
      } 
      else {
        max(unlist(lifespans[possibleLifespans]),na.rm=T)
      }
    })
  )
}

calculate_indicator_not_aggregable_by_time <- function(indicator, annualEnergySavings, annualCostSavings, affectedBuildingArea, 
                                                       investment, discountRate, lifespan){
  
  valueInd <- if(indicator == "NormalisedInvestmentCost"){
    investment / affectedBuildingArea
  }
  else if(indicator == "AvoidanceCost"){
    investment / (-annualEnergySavings * lifespan)
  }
  else if(indicator == "SimplePayback"){
    investment / -annualCostSavings
  }
  else if(indicator == "NetPresentValue"){
    FinCal::npv(
      c(-investment, rep(-annualCostSavings, lifespan)), r = discountRate/100
    )
  }
  else if(indicator == "ProfitabilityIndex"){
    (FinCal::npv(
      c(-investment, rep(-annualCostSavings, lifespan)), r = discountRate/100
    ) - investment) / investment
  }
  else if(indicator == "NetPresentValueQuotient"){
    FinCal::npv(
      c(-investment, rep(-annualCostSavings, lifespan)), r = discountRate/100
    ) / investment
  }
  else if(indicator == "InternalRateOfReturn"){
    FinCal::irr(
      c(-investment, rep(-annualCostSavings, lifespan))
    ) * 100
  }
  return(valueInd)
}

calculate_indicator <-  function(data, indicator, consumptionColumn, baselineConsumptionColumn, energyPriceColumn, 
                                 carbonEmissionsColumn, buildingGrossFloorArea, heatingDegreeDays18Column, coolingDegreeDays21Column){
  if (indicator == "EnergyUse") {
    valueInd <- data.frame("ind"=data[, consumptionColumn])
  }
  else if (indicator == "EnergyUseIntensity") {
    valueInd <- data.frame("ind"=data[, consumptionColumn]/buildingGrossFloorArea)
  }
  else if (indicator == "EnergyUseSavings") {
    valueInd <- data.frame("ind"=data[, consumptionColumn] - data[, baselineConsumptionColumn])
  }
  else if (indicator == "EnergyUseSavingsRelative") {
    valueInd <- data.frame(
      "ind"=(data[, consumptionColumn] - data[, baselineConsumptionColumn])*100 / data[, baselineConsumptionColumn],
      "weights"=data[, baselineConsumptionColumn])
  }
  else if (indicator == "EnergyUseSavingsIntensity") {
    valueInd <- data.frame("ind"=(data[, consumptionColumn] - data[, baselineConsumptionColumn]) / buildingGrossFloorArea)
  }
  else if (indicator == "EnergyCost" && !is.null(energyPriceColumn)) {
    valueInd <- data.frame("ind"=(data[, consumptionColumn] * data[, energyPriceColumn]))
  }
  else if (indicator == "EnergyCostIntensity" && !is.null(energyPriceColumn)) {
    valueInd <- data.frame("ind"=(data[, consumptionColumn] * data[, energyPriceColumn])/buildingGrossFloorArea)
  }
  else if (indicator == "EnergyCostSavings" && !is.null(energyPriceColumn)) {
    valueInd <- data.frame("ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, energyPriceColumn]))
  }
  else if (indicator == "EnergyCostSavingsRelative" && !is.null(energyPriceColumn)) {
    valueInd <- data.frame(
      "ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, energyPriceColumn])*100 / (data[, baselineConsumptionColumn] * data[, energyPriceColumn]),
      "weights"=data[, baselineConsumptionColumn] * data[, energyPriceColumn])
  }
  else if (indicator == "EnergyCostSavingsIntensity" && 
           !is.null(energyPriceColumn)) {
    valueInd <- data.frame("ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, energyPriceColumn])/buildingGrossFloorArea)
  }
  else if (indicator == "EnergyEmissions" && !is.null(carbonEmissionsColumn)) {
    valueInd <- data.frame("ind"=(data[, consumptionColumn] * data[, carbonEmissionsColumn]))
  }
  else if (indicator == "EnergyEmissionsIntensity" && 
           !is.null(carbonEmissionsColumn)) {
    valueInd <- data.frame("ind"=(data[, consumptionColumn] * data[, carbonEmissionsColumn])/buildingGrossFloorArea)
  }
  else if (indicator == "EnergyEmissionsSavingsRelative" && !is.null(carbonEmissionsColumn)) {
    valueInd <- data.frame("ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, carbonEmissionsColumn])*100 / (data[, baselineConsumptionColumn] * data[, carbonEmissionsColumn]),
    "weights" = data[, baselineConsumptionColumn] * data[, carbonEmissionsColumn])
  }
  else if (indicator == "EnergyEmissionsSavings" && !is.null(carbonEmissionsColumn)) {
    valueInd <- data.frame("ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, carbonEmissionsColumn]))
  }
  else if (indicator == "EnergyEmissionsSavingsIntensity" && 
           !is.null(carbonEmissionsColumn)) {
    valueInd <- data.frame("ind"=((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
                           data[, carbonEmissionsColumn])/buildingGrossFloorArea)
  }
  else if (indicator == "HeatingDegreeDays" && !is.null(heatingDegreeDays18Column)) {
    valueInd <- data.frame("ind"=ifelse(data[, heatingDegreeDays18Column] > 3, data[, heatingDegreeDays18Column], 0))
  }
  else if (indicator == "CoolingDegreeDays" && !is.null(coolingDegreeDays21Column)) {
    valueInd <- data.frame("ind"=ifelse(data[, coolingDegreeDays21Column] > 3, data[, coolingDegreeDays21Column], 0))
  }
  else {
    valueInd <- NULL
  }
  
  return(valueInd)
}

generate_longitudinal_benchmarking_indicators <- function (
  data, indicators, measuredProperty, measuredPropertyComponent, frequencies, 
  buildingId, buildingSubject, timeColumn, localTimeZone, 
  consumptionColumn, indicatorsUnitsSubjects, baselineConsumptionColumn = NULL, 
  buildingGrossFloorArea = 0, outdoorTemperatureColumn = NULL, 
  heatingDegreeDays18Column = NULL, coolingDegreeDays21Column = NULL, 
  carbonEmissionsColumn = NULL, energyPriceColumn = NULL, 
  modelName = NULL, modelId = NULL, modelLocation = NULL, 
  modelStorageInfrastructureSubject = NULL, modelTypeSubject = NULL, 
  modelBaselineYear = NULL, estimateWhenAggregate = T,
  prevResults = NULL) {
  
  buildingNamespace <- paste0(strsplit(buildingSubject, "#")[[1]][1], "#")
  namespaces <- bigg_namespaces
  namespaces["biggresults"] <- buildingNamespace
  
  if (is.null(prevResults)) {
    prevResults <- list(results_rdf=rdf(), results_ts=list()) 
  }
  
  obj <- prevResults$results_rdf
  results_ts <- prevResults$results_ts
  
  if (!is.null(modelId)) {
    modelSubject <- sprintf("biggresults:MODEL-%s-%s", buildingId, modelId)
    if (!exists_analytical_model(obj, modelSubject, namespaces)) {
      obj <- obj %>% add_item_to_rdf(subject = modelSubject, 
                                     classes = c("bigg:AnalyticalModel"), dataProperties = c(
                                       list(`bigg:modelLocation` = modelLocation, 
                                            `bigg:modelTrainedDate` = parsedate::format_iso_8601(lubridate::now("UTC")), 
                                            `bigg:modelName` = modelName),
                                       if (!is.null(modelBaselineYear)) {
                                         list(`bigg:modelBaselineYear` = modelBaselineYear)
                                       }), 
                                     objectProperties = list(
                                       `bigg:hasModelStorageInfrastructure` = modelStorageInfrastructureSubject, 
                                       `bigg:hasModelType` = modelTypeSubject), namespaces = namespaces)
      obj <- obj %>% add_item_to_rdf(subject = buildingSubject, 
                                     objectProperties = list(`bigg:hasAnalyticalModel` = modelSubject), 
                                     namespaces = namespaces)
    }
  } else { modelSubject <- NA }
  for (indicator in indicators) {
    frequencies_ <- frequencies
    originalDataPeriod <- detect_time_step(data[, timeColumn])
    valueInd <- calculate_indicator(data, indicator, consumptionColumn, baselineConsumptionColumn, energyPriceColumn, 
                                    carbonEmissionsColumn, buildingGrossFloorArea, heatingDegreeDays18Column, coolingDegreeDays21Column)
    
    # If the HDD or CDD are not available in 'data' object, calculate it based on the outdoor temperature
    if ((indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays")) && 
        !is.null(outdoorTemperatureColumn) && is.null(valueInd)) {
      indDf <- data.frame(time = as.Date(data[, timeColumn], 
                                         tz = localTimeZone), temperature = data[, outdoorTemperatureColumn]) %>% 
        group_by(time) %>% summarise(temperature = mean(temperature)) %>% 
        mutate(degree_days(., "temperature", localTimeZone, 
                           if (indicator == "HeatingDegreeDays") {21} else {18}, 
                           if (indicator == "HeatingDegreeDays") {"heating"} else {"cooling"}, 
                           outputFrequency = "P1D", outputFeaturesName = "ind", 
                           fixedOutputFeaturesName = T)) %>% 
        select(time,ind)
      frequencies_ <- frequencies[frequencies >= as.period("P1D")]
      originalDataPeriod <- "P1D"
    } else if (!is.null(valueInd)) {
      indDf <- data.frame(time = data[, timeColumn], ind = valueInd$ind, weights = 
                            if("weights" %in% colnames(valueInd)){valueInd$weights}else{rep(1,nrow(valueInd))})
    } else {
      next
    }
    for (frequency in frequencies_) {
      n <- hourly_timesteps(as.numeric(as.period(frequency))/3600, 
                            originalDataPeriod)
      indDfAux <- indDf %>% 
        group_by(start = floor_date(time, unit = frequency, 
                  week_start = getOption("lubridate.week.start", 1))) %>% 
        summarise(estimated = mean(ind, na.rm = T) * n, 
                  real = sum(ind)) %>% 
        mutate(
          value = if (estimateWhenAggregate == T) { estimated } else { real }, 
          isReal = if (is.null(modelId)) {
            ifelse(is.finite(real), T, ifelse(is.finite(value), F, NA))
          } else {
            ifelse(is.finite(value), F, NA)
          }) %>% 
        select(-real, -estimated)
      indDfAux$start <- with_tz(indDfAux$start, "UTC")
      if (as.period(frequency) >= as.period("P1D")) {
        indDfAux$end <- with_tz(with_tz(indDfAux$start, 
                                        localTimeZone) + iso8601_period_to_timedelta(frequency) - 
                                  seconds(1), "UTC")
      }
      else {
        indDfAux$end <- indDfAux$start + iso8601_period_to_timedelta(frequency) - 
          seconds(1)
      }
      keyPerformanceIndicatorSubject <- paste("bigg:KPI", indicator, sep = "-")
      singleKPISubject <- paste("biggresults:SingleKPI", buildingId, 
        if (indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays")) { indicator
        } else { paste(indicator, measuredPropertyComponent, measuredProperty, sep = "-") }, 
        modelName, modelId, frequency, sep = "-")
      singleKPISubjectHash <- digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
      singleKPIPointSubject <- paste0("biggresults:", singleKPISubjectHash)
      obj %>% add_item_to_rdf(subject = singleKPISubject, 
                              classes = c("bigg:SingleKPIAssessment", "bigg:KPIAssessment", 
                                          "bigg:TimeSeriesList"), 
                              dataProperties = c(list(
                                `bigg:timeSeriesIsRegular` = T, 
                                `bigg:timeSeriesIsOnChange` = F, 
                                `bigg:timeSeriesIsCumulative` = F, 
                                `bigg:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                                `bigg:timeSeriesEnd` = max(indDfAux$end,  na.rm = T), 
                                `bigg:timeSeriesFrequency` = frequency, 
                                `bigg:timeSeriesTimeAggregationFunction` = "SUM"),
                                if (!(indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays"))) { list(
                                  `bigg:measuredPropertyComponent` = measuredPropertyComponent) }), 
                              objectProperties = c(list(
                                `bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicator]], 
                                `bigg:hasSingleKPIPoint` = singleKPIPointSubject, 
                                `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject),
                                if (!is.null(modelId)) { list(
                                  `bigg:isEstimatedByModel` = modelSubject)
                                },
                                if (!(indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays"))) { list(
                                  `bigg:hasMeasuredProperty` = if(startsWith(measuredProperty,"bigg:")){ measuredProperty
                                  } else {paste0("bigg:",measuredProperty)}) }
                              ), namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = buildingSubject, 
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
      indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(as.period(frequency)>=as.period("P1M")){
        indDfAuxMeta <- data.frame(
          `individualSubject` = namespace_integrator(buildingSubject, namespaces),
          `keyPerformanceIndicator` = indicator,
          `measuredProperty` = namespace_integrator(paste0("bigg:",measuredProperty), namespaces),
          `measuredPropertyComponent` = namespace_integrator(paste0("bigg:",measuredPropertyComponent), namespaces),
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicator]], namespaces),
          `frequency` = frequency,
          `modelSubject` = namespace_integrator(modelSubject, namespaces)
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% filter(is.finite(value)) %>%
          {
            if(as.period(frequency)>=as.period("P1Y")){
              mutate(
                .,
                year = year(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone))
              )
            } else {
              mutate(
                .,
                year = year(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone)),
                month = month(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone))
              )
            }
          }
      }
    }
  }
  return(list(results_rdf=obj, results_ts=results_ts))
}

generate_eem_assessment_indicators <- function(
    data, indicators, indicatorsNotAggregableByTime, measuredProperty, measuredPropertyComponent, frequencies, 
    buildingId, buildingSubject, timeColumn, localTimeZone, eemProjectDf,
    consumptionColumn, indicatorsTimeAggregationFunctions, indicatorsUnitsSubjects, baselineConsumptionColumn = NULL, 
    buildingGrossFloorArea = NA, carbonEmissionsColumn = NULL, energyPriceColumn = NULL, 
    modelName = NULL, modelId = NULL, modelLocation = NULL, modelStorageInfrastructureSubject = NULL, 
    modelTypeSubject = NULL, estimateWhenAggregate = T, prevResults = NULL) {
  
  # data = results
  # indicators = unlist(settings$Indicators[c("Energy","Cost","Emissions")])
  # indicatorsNotAggregableByTime = if(measuredPropertyComponent=="Total"){settings$Indicators$NotAggregableByTime}else{NULL}
  # measuredProperty = measuredProperty
  # measuredPropertyComponent = measuredPropertyComponent
  # frequencies = settings$Frequencies
  # buildingId = buildingId
  # buildingSubject = buildingSubject
  # timeColumn = "time"
  # localTimeZone = tz
  # consumptionColumn =
  #   paste0(measuredPropertyComponentsMapping[["after_eem"]][measuredPropertyComponent],".after_eem")
  # indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects
  # indicatorsTimeAggregationFunctions = settings$IndicatorsTimeAggregationFunctions
  # baselineConsumptionColumn =
  #   paste0(measuredPropertyComponentsMapping[["counterfactual"]][measuredPropertyComponent],".counterfactual")
  # buildingGrossFloorArea = buildingGrossFloorArea
  # eemProjectDf = eems[eems$eemProjectId==strsplit(eemProject,"\\.")[[1]][2],]
  # carbonEmissionsColumn = "Qe_emissionsFactor"
  # energyPriceColumn = "Qe_price"
  # modelName = modelName
  # modelId = modelId
  # modelLocation = modelSubject
  # modelTypeSubject = "bigg:MODELTYPE-BaselineModel"
  # modelStorageInfrastructureSubject = "bigg:MODELINFRA-MLFlow"
  # estimateWhenAggregate = T
  # prevResults = NULL
  
  buildingNamespace <- paste0(strsplit(buildingSubject, "#")[[1]][1], "#")
  namespaces <- bigg_namespaces
  namespaces["biggresults"] <- buildingNamespace
  
  if (is.null(prevResults)) {
    prevResults <- list(results_rdf=rdf(), results_ts=list()) 
  }
  
  obj <- prevResults$results_rdf
  results_ts <- prevResults$results_ts
  # EEM Projects
  projectId <- unique(eemProjectDf$eemProjectId)[1]
  projectSubject <- sprintf("biggresults:PROJECT-%s-%s",buildingId,projectId)
  obj <- obj %>% add_item_to_rdf(subject = buildingSubject, 
                                 objectProperties = list(`bigg:hasProject` = projectSubject), 
                                 namespaces = namespaces)
  if (!exists_project_model(obj, projectSubject, namespaces)) {
    obj <- obj %>% add_item_to_rdf(subject = projectSubject, 
                                   classes = c("bigg:Project"),
                                   dataProperties = c(list(
                                     `bigg:projectName` = sprintf("Project %s",projectId),
                                     `bigg:projectDescription` = "Automatically created by the EEM assessment pipeline",
                                     `bigg:projectAutoGenerated` = T,
                                     `bigg:projectGenerationDate` = parsedate::format_iso_8601(lubridate::now("UTC")),
                                     `bigg:projectOperationalDate` = parsedate::format_iso_8601(lubridate::with_tz(max(eemProjectDf$Date),"UTC")),
                                     `bigg:projectStartDate` = parsedate::format_iso_8601(lubridate::with_tz(min(eemProjectDf$Date),"UTC")),
                                     `bigg:projectEndDate` = parsedate::format_iso_8601(lubridate::with_tz(max(eemProjectDf$Date),"UTC")),
                                     `bigg:projectNumberEEMs` = nrow(eemProjectDf)),
                                     if(length(unique(eemProjectDf$Currency))==1){
                                       list(`bigg:projectInvestment` = sum(eemProjectDf$Investment,na.rm=T))
                                     }),
                                   objectProperties = c(
                                     if(length(unique(eemProjectDf$Currency))==1){
                                       list(`bigg:hasProjectInvestmentCurrency` = eemProjectDf$Currency[1])
                                     },
                                     setNames(as.list(eemProjectDf$eemSubject),rep("bigg:includesMeasure",nrow(eemProjectDf))),
                                     setNames(as.list(eemProjectDf$Type[order(eemProjectDf$Investment,decreasing = T)]),
                                              rep("bigg:hasEnergyEfficiencyMeasureType",nrow(eemProjectDf))),
                                     list(`bigg:affectsBuilding` = buildingSubject)), 
                                   namespaces = namespaces)
  }
  # Baseline and after EEM models
  if (!is.null(modelId)) {
    modelSubject <- sprintf("biggresults:MODEL-%s-%s", buildingId, modelId)
    if (!exists_analytical_model(obj, modelSubject, namespaces)) {
      obj <- obj %>% add_item_to_rdf(subject = modelSubject, 
                                     classes = c("bigg:AnalyticalModel"), dataProperties =
                                       list(`bigg:modelLocation` = modelLocation, 
                                            `bigg:modelTrainedDate` = parsedate::format_iso_8601(lubridate::now("UTC")), 
                                            `bigg:modelName` = modelName), 
                                     objectProperties = list(
                                       `bigg:hasModelStorageInfrastructure` = modelStorageInfrastructureSubject, 
                                       `bigg:hasModelType` = modelTypeSubject), namespaces = namespaces)
      obj <- obj %>% add_item_to_rdf(subject = buildingSubject, 
                                     objectProperties = list(`bigg:hasAnalyticalModel` = modelSubject), 
                                     namespaces = namespaces)
    }
  } else { modelSubject <- NA }
  
  annualEnergySavings <- NA
  annualCostSavings <- NA
  annualStartTimestamp <- NA
  annualEndTimestamp <- NA
  for (indicator in indicators) {
    originalDataPeriod <- detect_time_step(data[, timeColumn])
    valueInd <- calculate_indicator(data, indicator, consumptionColumn, baselineConsumptionColumn, energyPriceColumn, 
                                    carbonEmissionsColumn, buildingGrossFloorArea, heatingDegreeDays18Column, coolingDegreeDays21Column)
    
    if (!is.null(valueInd)) {
      indDf <- data.frame(time = data[, timeColumn], ind = valueInd$ind, weights = 
                            if("weights" %in% colnames(valueInd)){valueInd$weights}else{rep(1,nrow(valueInd))})
    } else {
      next
    }
    
    for (frequency in frequencies) {
      if(frequency!=""){
        n <- hourly_timesteps(as.numeric(as.period(frequency))/3600, 
                         originalDataPeriod)
      } else {
        # When no frequency, calculate the indicators only considering the first 365 days
        indDf <- indDf %>% filter(time <= min(time,na.rm=T)+days(365))
        n <- nrow(indDf)
      }
      indDfAux <- indDf %>%  {
          if(frequency==""){
            group_by(., start = first(time))
          } else {
            group_by(., start = floor_date(time, unit = frequency, 
                        week_start = getOption("lubridate.week.start", 1)))
          }
        } %>% {
          if(indicatorsTimeAggregationFunctions[[indicator]]=="SUM") {
            summarise(., estimated = mean(ind, na.rm = T) * n, real = sum(ind, na.rm = T))
          } else if(indicatorsTimeAggregationFunctions[[indicator]]=="AVG"){
            summarise(., estimated = mean(ind, na.rm = T), real = mean(ind, na.rm = T))
          } else if(indicatorsTimeAggregationFunctions[[indicator]]=="WEIGHTED-AVG"){
            summarise(., estimated = weighted.mean(ind, weights, na.rm = T), 
                         real = weighted.mean(ind, weights, na.rm = T))
          } else {
            .
          }
        } %>% 
        mutate(
          value = if (estimateWhenAggregate == T) { estimated } else { real }, 
          isReal = if (is.null(modelId)) {
            ifelse(is.finite(real), T, ifelse(is.finite(value), F, NA))
          } else {
            ifelse(is.finite(value), F, NA)
          }) %>% 
        select(-real, -estimated)
      indDfAux$start <- with_tz(indDfAux$start, "UTC")
      if(frequency==""){
        indDfAux$end <- last(indDf$time)
      } else {
        if (as.period(frequency) >= as.period("P1D")) {
          indDfAux$end <- with_tz(with_tz(indDfAux$start, 
                                          localTimeZone) + iso8601_period_to_timedelta(frequency) - 
                                    seconds(1), "UTC")
        } else {
          indDfAux$end <- indDfAux$start + iso8601_period_to_timedelta(frequency) - 
            seconds(1)
        }
      }
      # Force to zero saving values
      # indDfAux$value <- ifelse(indDfAux$value>0, 0, indDfAux$value)
      
      if(frequency=="" && indicator=="EnergyUseSavings" && measuredPropertyComponent=="Total"){
        annualEnergySavings <- indDfAux$value
        annualStartTimestamp <- indDfAux$start
        annualEndTimestamp <- indDfAux$end
      }
      if(frequency=="" && indicator=="EnergyCostSavings"){
        annualCostSavings <- indDfAux$value
      }
      keyPerformanceIndicatorSubject <- paste("bigg:KPI", indicator, sep = "-")
      singleKPISubject <- paste("biggresults:SingleKPI", buildingId, projectId,
                                paste(indicator, measuredPropertyComponent, measuredProperty, sep = "-"), 
                                modelName, modelId, frequency, sep = "-")
      singleKPISubjectHash <- digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
      singleKPIPointSubject <- paste0("biggresults:", singleKPISubjectHash)
      obj %>% add_item_to_rdf(subject = singleKPISubject, 
                              classes = c("bigg:SingleKPIAssessment", "bigg:KPIAssessment", 
                                          "bigg:TimeSeriesList"), 
                              dataProperties = c(list(
                                `bigg:timeSeriesIsRegular` = if(frequency==""){F}else{T}, 
                                `bigg:timeSeriesIsOnChange` = F, 
                                `bigg:timeSeriesIsCumulative` = F, 
                                `bigg:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                                `bigg:timeSeriesEnd` = max(indDfAux$end,  na.rm = T),
                                `bigg:timeSeriesTimeAggregationFunction` = indicatorsTimeAggregationFunctions[[indicator]],
                                `bigg:measuredPropertyComponent` = measuredPropertyComponent),
                              if(frequency!=""){list(
                                `bigg:timeSeriesFrequency` = frequency
                              )}), 
                              objectProperties = c(list(
                                `bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicator]], 
                                `bigg:hasSingleKPIPoint` = singleKPIPointSubject, 
                                `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject,
                                `bigg:hasMeasuredProperty` = if(startsWith(measuredProperty,"bigg:")){ 
                                  measuredProperty
                                } else {paste0("bigg:",measuredProperty)}),
                                if (!is.null(modelId)) { list(
                                  `bigg:isEstimatedByModel` = modelSubject)
                                }
                              ), namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = projectSubject, 
                              classes = "bigg:KPICalculationItem",
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
      indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(frequency=="" | as.period(frequency)>=as.period("P1M")){
        indDfAuxMeta <- data.frame(
          `individualSubject` = namespace_integrator(projectSubject, namespaces),
          `keyPerformanceIndicator` = indicator,
          `measuredProperty` = namespace_integrator(paste0("bigg:",measuredProperty), namespaces),
          `measuredPropertyComponent` = namespace_integrator(paste0("bigg:",measuredPropertyComponent), namespaces),
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicator]], namespaces),
          `frequency` = frequency,
          `modelSubject` = namespace_integrator(modelSubject, namespaces)
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% filter(is.finite(value)) %>%
          {
            if(frequency==""){
              .
            } else if(as.period(frequency)>=as.period("P1Y")){
              mutate(
                .,
                year = year(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone))
              )
            } else {
              mutate(
                .,
                year = year(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone)),
                month = month(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone))
              )
            }
          }
      }
    }
  }
  if(any(is.na(frequencies)) && is.finite(annualEnergySavings) && 
     is.finite(annualCostSavings) && measuredPropertyComponent=="Total"){
    frequency <- ""
    for (indicatorNABT in indicatorsNotAggregableByTime){
      indDfAux <- data.frame(
        "start" = annualStartTimestamp,
        "end" = annualEndTimestamp,
        "value" = calculate_indicator_not_aggregable_by_time(
          indicator = indicatorNABT,
          annualEnergySavings = annualEnergySavings, 
          annualCostSavings = annualCostSavings, 
          affectedBuildingArea = buildingGrossFloorArea * 
            weighted.mean(eemProjectDf$AffectationShare/100,
                          w = eemProjectDf$Investment, na.rm=T), 
          investment = sum(eemProjectDf$Investment, na.rm=T), 
          discountRate = weighted.mean(eemProjectDf$DiscountRate,
                                       w = eemProjectDf$Investment,na.rm=T), 
          lifespan = ceiling(weighted.mean(eemProjectDf$Lifespan,
                                           w = eemProjectDf$Investment,na.rm=T))),
        "isReal" = F)
      keyPerformanceIndicatorSubject <- paste("bigg:KPI", indicatorNABT, sep = "-")
      singleKPISubject <- paste("biggresults:SingleKPI", buildingId, projectId,
                                paste(indicatorNABT, "Total", measuredProperty, sep = "-"), 
                                modelName, modelId, frequency, sep = "-")
      singleKPISubjectHash <- digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
      singleKPIPointSubject <- paste0("biggresults:", singleKPISubjectHash)
      obj %>% add_item_to_rdf(subject = singleKPISubject, 
                              classes = c("bigg:SingleKPIAssessment", "bigg:KPIAssessment", 
                                          "bigg:TimeSeriesList"), 
                              dataProperties = list(
                                `bigg:timeSeriesIsRegular` = F, 
                                `bigg:timeSeriesIsOnChange` = F, 
                                `bigg:timeSeriesIsCumulative` = F, 
                                `bigg:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                                `bigg:timeSeriesEnd` = max(indDfAux$end,  na.rm = T),
                                `bigg:measuredPropertyComponent` = "Total"),
                              objectProperties = c(list(
                                `bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicatorNABT]], 
                                `bigg:hasSingleKPIPoint` = singleKPIPointSubject, 
                                `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject,
                                `bigg:hasMeasuredProperty` = if(startsWith(measuredProperty,"bigg:")){ 
                                  measuredProperty
                                } else {paste0("bigg:",measuredProperty)}),
                                if (!is.null(modelId)) { list(
                                  `bigg:isEstimatedByModel` = modelSubject)
                                }
                              ), namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = projectSubject, 
                              classes = "bigg:KPICalculationItem",
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
      indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(frequency=="" | as.period(frequency)>=as.period("P1M")){
        indDfAuxMeta <- data.frame(
          `individualSubject` = namespace_integrator(projectSubject, namespaces),
          `keyPerformanceIndicator` = indicatorNABT,
          `measuredProperty` = namespace_integrator(paste0("bigg:",measuredProperty), namespaces),
          `measuredPropertyComponent` = namespace_integrator("bigg:Total", namespaces),
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicatorNABT]], namespaces),
          `frequency` = frequency,
          `modelSubject` = namespace_integrator(modelSubject, namespaces)
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% filter(is.finite(value))
      }
    }
  }
  
  return(list(results_rdf=obj, results_ts=results_ts))
}

generate_cross_sectional_benchmarking_indicator <- function(
    data, isReal, indicator, frequency, groupSubject, prefixUtcTimeColumns,
    indicatorsUnitsSubjects, prevResults = NULL){
  
  groupNamespace <- paste0(strsplit(groupSubject,"#")[[1]][1],"#")
  groupId <- strsplit(groupSubject,"#")[[1]][2]
  
  if (is.null(prevResults)) {
    prevResults <- list(results_rdf=rdf(), results_ts=list()) 
  }
  
  obj <- prevResults$results_rdf
  results_ts <- prevResults$results_ts
  
  distinct_local_tz <- 
    gsub(prefixUtcTimeColumns,"",colnames(data)[grepl(prefixUtcTimeColumns,colnames(data))])
  value_columns <- colnames(data)[!grepl(paste0("localtime|",prefixUtcTimeColumns),colnames(data))]
  data$localtime <- NULL
  
  for (local_tz in distinct_local_tz){
    indDfAux <- data.frame(
        "start" = data[,paste0("utctime_",local_tz)],
        "value" = mapply(function(i){
          toJSON(as.list(data[i, !grepl(prefixUtcTimeColumns,colnames(data))]),
                 auto_unbox = T)}, 
          1:nrow(data)), 
        "isReal" = isReal)
    if (as.period(frequency) >= as.period("P1D")) {
      indDfAux$end <- with_tz(with_tz(indDfAux$start, local_tz) + 
                                iso8601_period_to_timedelta(frequency) - 
                                seconds(1), "UTC")
    } else {
      indDfAux$end <- indDfAux$start + iso8601_period_to_timedelta(frequency) - 
        seconds(1)
    }
    keyPerformanceIndicatorName <- indicator
    keyPerformanceIndicatorSubject <- paste("bigg:KPI", 
                                            keyPerformanceIndicatorName, sep = "-")
    aggKPISubject <- paste(paste0(groupNamespace,"AggregatedKPI"), 
                           groupId, keyPerformanceIndicatorName, isReal, frequency, sep = "-")
    aggKPISubjectHash <- digest(namespace_integrator(aggKPISubject, 
                                                        bigg_namespaces), "sha256", serialize = T)
    aggKPIPointSubject <- paste0(groupNamespace,aggKPISubjectHash)
    obj %>% add_item_to_rdf(
      subject = aggKPISubject, 
      classes = c("bigg:AggregatedKPIAssessment", "bigg:KPIAssessment", 
                  "bigg:TimeSeriesList"), 
      dataProperties = list(`bigg:timeSeriesIsRegular` = T, 
                            `bigg:timeSeriesIsOnChange` = F, 
                            `bigg:timeSeriesIsCumulative` = F, 
                            `bigg:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                            `bigg:timeSeriesEnd` = max(indDfAux$end,  na.rm = T), 
                            `bigg:timeSeriesFrequency` = frequency, 
                            `bigg:localTimeZone` = local_tz,
                            `bigg:timeSeriesTimeAggregationFunction` = "AVG"), 
      objectProperties = 
        list(`bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicator]], 
             `bigg:hasAggregatedKPIPoint` = aggKPIPointSubject, 
             `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject), 
      namespaces = bigg_namespaces)
    obj %>% add_item_to_rdf(subject = groupSubject, 
                            objectProperties = list(`bigg:assessesAggregatedKPI` = aggKPISubject), 
                            namespaces = bigg_namespaces)
    indDfAux$start <- parsedate::format_iso_8601(indDfAux$start)
    indDfAux$end <- parsedate::format_iso_8601(indDfAux$end)
    
    results_ts[[aggKPISubjectHash]]<-indDfAux
  }
  return(list(results_rdf=obj, results_ts=results_ts))
}
