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
    valueInd <- if (indicator == "EnergyUse") {
      data[, consumptionColumn]
    }
    else if (indicator == "EnergyUseIntensity" && buildingGrossFloorArea > 0) {
      data[, consumptionColumn]/buildingGrossFloorArea
    }
    else if (indicator == "EnergyUseSavings") {
      data[, consumptionColumn] - data[, baselineConsumptionColumn]
    }
    else if (indicator == "EnergyUseSavingsIntensity" && buildingGrossFloorArea > 0) {
      (data[, consumptionColumn] - data[, baselineConsumptionColumn]) / buildingGrossFloorArea
    }
    else if (indicator == "EnergyCost" && !is.null(energyPriceColumn)) {
      (data[, consumptionColumn] * data[, energyPriceColumn])
    }
    else if (indicator == "EnergyCostIntensity" && !is.null(energyPriceColumn) && buildingGrossFloorArea > 0) {
      (data[, consumptionColumn] * data[, energyPriceColumn])/buildingGrossFloorArea
    }
    else if (indicator == "EnergyCostSavings" && !is.null(energyPriceColumn)) {
      ((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
         data[, energyPriceColumn])
    }
    else if (indicator == "EnergyCostSavingsIntensity" && 
             !is.null(energyPriceColumn) && buildingGrossFloorArea > 0) {
      ((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
         data[, energyPriceColumn])/buildingGrossFloorArea
    }
    else if (indicator == "EnergyEmissions" && !is.null(carbonEmissionsColumn)) {
      (data[, consumptionColumn] * data[, carbonEmissionsColumn])
    }
    else if (indicator == "EnergyEmissionsIntensity" && 
             !is.null(carbonEmissionsColumn) && buildingGrossFloorArea > 0) {
      (data[, consumptionColumn] * data[, carbonEmissionsColumn])/buildingGrossFloorArea
    }
    else if (indicator == "EnergyEmissionsSavings" && !is.null(carbonEmissionsColumn)) {
      ((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
         data[, carbonEmissionsColumn])
    }
    else if (indicator == "EnergyEmissionsSavingsIntensity" && 
             !is.null(carbonEmissionsColumn) && buildingGrossFloorArea > 0) {
      ((data[, consumptionColumn] - data[, baselineConsumptionColumn]) * 
         data[, carbonEmissionsColumn])/buildingGrossFloorArea
    }
    else if (indicator == "HeatingDegreeDays" && !is.null(heatingDegreeDays18Column)) {
      ifelse(data[, heatingDegreeDays18Column] > 3, data[, heatingDegreeDays18Column], 0)
    }
    else if (indicator == "CoolingDegreeDays" && !is.null(coolingDegreeDays21Column)) {
      ifelse(data[, coolingDegreeDays21Column] > 3, data[, coolingDegreeDays21Column], 0)
    }
    else {
      NULL
    }
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
      indDf <- data.frame(time = data[, timeColumn], ind = valueInd)
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
          `measuredProperty` = measuredProperty,
          `measuredPropertyComponent` = measuredPropertyComponent,
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicator]], namespaces),
          `frequency` = frequency,
          `modelSubject` = modelSubject
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% filter(is.finite(value)) %>%
          mutate(
            year = year(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone)),
            month = month(lubridate::with_tz(parsedate::parse_iso_8601(start), localTimeZone))
          )
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
