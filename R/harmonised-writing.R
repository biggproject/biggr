#
# General utils for harmonised data writing ----
#

bigg_namespaces <- c("bigg" = "http://bigg-project.eu/ontology#",
                     "unit" = "http://qudt.org/vocab/unit/",
                     "xsd" =  "http://www.w3.org/2001/XMLSchema#"
)

#' Parse datetime to ISO 8601 format
#' 
#' This function parses the format of a POSIXct object to ISO 8601 with Zulu as UTC timezone definition. 
#' For instance: 2020-01-01 03:00:00 Europe/Madrid --> "2020-01-01T02:00:00Z"
#' 
#' @param datetimes <array> of POSIXct in any timezone.
#' 
#' @return <array> of string with ISO 8601 format.

format_iso_8601z <- function(datetimes){gsub("+00:00","Z",parsedate::format_iso_8601(datetimes),fixed = T)}

#' Namespace integrator
#' 
#' This function converts the simplified namespace subtring of multiple URIs to his complete URI namespace.
#'
#' @param items <array> of strings with the URIs to complete.
#' @param namespaces named <array>. Example: 'biggr::bigg_namespaces'.
#' @return <array> of strings with the complete namespace integrated.

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

#' Export an RDF to Turtle format (TTL)
#' 
#' Export a knowledge graph, represented in RDF, to a file in Turtle format (TTL)
#' 
#' @param object <rdf> knowledge graph
#' @param file <string> containing the path and filename where to store the data in TTL format. 
#' @return <boolean> TRUE if the file writing process succeed

write_rdf <- function(object, file){
  tryCatch({
    serialized <- rdf_serialize(object,
                    namespace = bigg_namespaces,
                    format = "turtle")
    #correct xsd:datetime rdflib error
    serialized <- gsub("\\^\\^<xsd:dateTime>", "^^xsd:dateTime", serialized)
    write(serialized, file)
    }, 
    error=function(e){
      return(F)
    })
  return(T)
}

#' Add triplet to and RDF object
#' 
#' This function adds a triplet to an RDF object. Classes, data properties and 
#' object properties can be defined to a certain subject.
#'
#' @param object <rdf> base object.
#' @param subject <uri> containing the subject of the new item.
#' @param classes <uri(s)> containing the subject(s) name(s) in the 
#' ontology related with this item.
#' @param dataProperties <list> containing data properties of the triplet. 
#' It is literal information, no connections to other subjects of the RDF.
#' @param objectProperties <list> containing object properties of the triplet. 
#' It consists on links to other items of the RDF.
#' @param namespaces named <array> that relates simple namespaces and complete 
#' ones.
#' 
#' @return <rdf> object with the new item added.

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
      if(!is.null(value)){
        datetimeDetected <- class(value)[1]=="POSIXct"
        object %>% rdf_add(
          subject = subject,
          predicate = namespace_integrator(names(dataProperties)[i],namespaces),
          object = if(datetimeDetected){format_iso_8601z(value)} else {value},
          subjectType = "uri",objectType = "literal",
          datatype_uri = if(datetimeDetected){"xsd:dateTime"
          } else {as.character(NA)}
        )
      }
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

#' Get the Energy Efficiency Measures (EEM) lifespan
#' 
#' From a list of lifespans, obtain the list of lifespans of a set of EEM types
#'
#' @param lifespans a named <array> defining the default lifespans to consider for each type of EEM.
#' @param eemTypes an <array> that defines the EEM types.
#' 
#' @return <array> of lifespans for each EEM type.

get_eem_lifespan <- function(lifespans=NULL, eemTypes){
  
  # Default lifespans
  if(is.null(lifespans)){
    lifespans <- c(
      "BuildingFabricMeasure"= 25,
      "LightingMeasure"= 10,
      "RenewableGenerationMeasure"= 25,
      "HVACAndHotWaterMeasure"= 15,
      "default"= 10
    )
  }
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

#' Calculate a KPI not aggregable by time.
#' 
#' This function computes the value of a certain indicator that is not aggregable by time (normally, those related to financial). 
#'
#' @param indicator <string> defining the KPI to calculate. Possible values: 
#' NormalisedInvestmentCost, AvoidanceCost, SimplePayback, NetPresentValue, 
#' ProfitabilityIndex, NetPresentValueQuotient, InternalRateOfReturn.
#' @param annualEnergySavings <float> definining the accumulated energy saving for a 
#' whole year.
#' @param annualCostSavings <float> definining the accumulated cost saving for a 
#' whole year.
#' @param affectedBuildingArea <float> definining building area affected by the EEM, 
#' or project of EEMs.
#' @param investment <float> defining the total initial investment cost of EEM, or 
#' project of EEMs.
#' @param discountRate <float> defining the discount rate for the Net Present value. 
#' Unit: \%.
#' @param lifespan <int> defining the lifespan of a EEM, or a project of EEMs.
#' 
#' @return <float> with the value of the KPI.
#' 
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

#' Calculate time series KPI.
#' 
#' This function calculates a large list of energy-related KPIs considering a set predicted and real energy measurements.
#' It only accounts for KPI that can be time-aggregable, so input and output data are time series.
#'
#' @param data <data.frame> that contains all resultant time series.
#' @param indicator <string> defining the KPI to calculate. Possible values: EnergyUse, EnergyUseIntensity,
#' EnergyUseSavings, EnergyUseSavingsRelative, EnergyUseSavingsIntensity, EnergyCost, EnergyCostIntensity,
#' EnergyCostSavings, EnergyCostSavingsRelative, EnergyCostSavingsIntensity, EnergyEmissions, EnergyEmissionsIntensity,
#' EnergyEmissionsSavings, EnergyEmissionsSavingsRelative, EnergyEmissionsSavingsIntensity, HeatingDegreeDays,
#' CoolingDegreeDays
#' @param consumptionColumn <string> defining the energy consumption column name in data.
#' @param baselineConsumptionColumn <string> defining the counterfactual energy consumption column name in data.
#' @param energyPriceColumn <string> defining the column in data related with the price of energy consumption.
#' @param carbonEmissionsColumn <string> defining the column in data related with the CO2 emissions factor of energy consumption.
#' @param buildingGrossFloorArea <float> defining the gross floor area of the building. 
#' @param heatingDegreeDays18Column <string> defining the column in data related with HDD with 18º as base temperature.
#' @param coolingDegreeDays21Column <string> defining the column in data related with CDD with 21º as base temperature.
#' 
#' @return <data.frame> with a column 'ind' specifying the indicator time series and an optional 'weights' column specifying 
#' the weights that should be considered when time-aggregate the results.

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

#
# Specific functions to harmonise resultant datasets to BIGG Ontology ----
#

#' Generate the harmonised longitudinal benchmarking results
#' 
#' Generate the results in harmonised format to BIGG ontology for the energy consumption benchmarking of 
#' buildings. In this case, the focus is to generate the results for the longitudinal benchmarking, 
#' so input data must be related with one single building. 
#' 
#' @param data <data.frame> that contains all resultant time series.
#' @param indicators <array> of strings defining the KPIs to calculate. Possible values: 
#' EnergyUse, EnergyUseIntensity, EnergyUseSavings, EnergyUseSavingsRelative, 
#' EnergyUseSavingsIntensity, EnergyCost, EnergyCostIntensity, EnergyCostSavings, 
#' EnergyCostSavingsRelative, EnergyCostSavingsIntensity, EnergyEmissions, 
#' EnergyEmissionsIntensity, EnergyEmissionsSavings, EnergyEmissionsSavingsRelative, 
#' EnergyEmissionsSavingsIntensity, HeatingDegreeDays, CoolingDegreeDays
#' @param measuredProperty <uri> defining the energy consumption measured property. 
#' (E.g.: http://bigg-project.eu/ontology#EnergyConsumptionGridElectricity)
#' @param measuredPropertyComponent <uri> defining the energy consumption component name. 
#' (E.g.: "http://bigg-project.eu/ontology#Heating").
#' @param frequencies <array> of strings defining the frequencies used to resample the results. 
#' They must follow ISO 8601 format representing the time step. Examples: 'P1D' (One day), 
#' 'P1Y' (One year), 'P1M' (One month), 'P1DT12H' (One day and a half)...
#' @param buildingId <string> building unique identifier.
#' @param buildingSubject <uri> building unique URI.
#' @param timeColumn <string> of the time column name.
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones).
#' @param consumptionColumn <string> defining the energy consumption column name in data.
#' @param indicatorsUnitsSubjects named <array> of URIs containing the units for each indicator.
#' @param baselineConsumptionColumn <string> defining the counterfactual energy consumption 
#' column name in data. By default it is not considered.
#' @param buildingGrossFloorArea <float> defining the gross floor area of the building. 
#' By default it is not considered.
#' @param outdoorTemperatureColumn <string> defining the column in data related to outdoor 
#' temperature. By default it is not considered.
#' @param heatingDegreeDays18Column <string> defining the column in data related with HDD 
#' with 18º as base temperature. By default it is not considered.
#' @param coolingDegreeDays21Column <string> defining the column in data related with CDD 
#' with 21º as base temperature. By default it is not considered.
#' @param carbonEmissionsColumn <string> defining the column in data related with the CO2 
#' emissions factor of energy consumption. By default it is not considered.
#' @param energyPriceColumn <string> defining the column in data related with the price of 
#' energy consumption. By default it is not considered.
#' @param modelName <string> Name of the model used to predict the consumption. By default 
#' results are not based by any model.
#' @param modelId <string> Identifier of the model used to predict the consumption. By default 
#' results are not based by any model.
#' @param modelLocation <string> containing the model path in the model storage infrastructure. 
#' By default results are not based by any model.
#' @param modelStorageInfrastructureSubject <uri> of the model infrastructure type. By default 
#' results are not based by any model.
#' @param modelTypeSubject <uri> of the model type depending on the data training strategy 
#' (Dynamic or Baseline). By default results are not based by any model.
#' @param modelBaselineYear <array> of integers containing the baseline years considered 
#' in model training. Only useful when the model type is baseline. By default results are 
#' not based by any model.
#' @param estimateWhenAggregate <boolean> defining if a linear estimation should be made 
#' when gaps are found in the time-aggregation of the results. By default, the estimation is done.
#' @param prevResults <list> with the previous results of this function. By default, no 
#' previous results are considered.
#' @return <list> containing a knowledge graph with all resultant metadata and time series 
#' objects, that later can be transformed to TTL and JSON files.

generate_longitudinal_benchmarking_indicators <- function (
  data, indicators, measuredProperty, measuredPropertyComponent, frequencies, 
  buildingId, buildingSubject, timeColumn, localTimeZone, 
  consumptionColumn, indicatorsUnitsSubjects, baselineConsumptionColumn = NULL, 
  buildingGrossFloorArea = NULL, outdoorTemperatureColumn = NULL, 
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
                                            `bigg:modelTrainedDate` = format_iso_8601z(lubridate::now("UTC")), 
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
      frequencies_ <- frequencies[frequencies >= lubridate::as.period("P1D")]
      originalDataPeriod <- "P1D"
    } else if (!is.null(valueInd)) {
      indDf <- data.frame(time = data[, timeColumn], ind = valueInd$ind, weights = 
                            if("weights" %in% colnames(valueInd)){valueInd$weights}else{rep(1,nrow(valueInd))})
    } else {
      next
    }
    for (frequency in frequencies_) {
      n <- hourly_timesteps(as.numeric(lubridate::as.period(frequency))/3600, 
                            originalDataPeriod)
      indDfAux <- indDf %>% 
        group_by(start = lubridate::floor_date(time, lubridate::as.period(frequency), 
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
      indDfAux$start <- lubridate::with_tz(indDfAux$start, "UTC")
      if (lubridate::as.period(frequency) >= lubridate::as.period("P1D")) {
        indDfAux$end <- lubridate::with_tz(lubridate::with_tz(indDfAux$start, 
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
      singleKPISubjectHash <- digest::digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
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
                                `bigg:timeSeriesTimeAggregationFunction` = "SUM")), 
                              objectProperties = c(list(
                                `bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicator]], 
                                `bigg:hasSingleKPIPoint` = singleKPIPointSubject, 
                                `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject),
                                if (!is.null(modelId)) { list(
                                  `bigg:isEstimatedByModel` = modelSubject)
                                },
                                if (!(indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays"))) { list(
                                  `bigg:hasMeasuredProperty` = if(startsWith(measuredProperty,"bigg:")){ measuredProperty
                                  } else {paste0("bigg:",measuredProperty)}) },
                                if (!(indicator %in% c("HeatingDegreeDays", "CoolingDegreeDays"))) { list(
                                  `bigg:hasMeasuredPropertyComponent` = paste0("bigg:",measuredPropertyComponent)) }
                              ), namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = singleKPIPointSubject,
                              classes = c("bigg:TimeSeriesPoint","bigg:SingleKPIAssessmentPoint"),
                              namespaces=namespaces)
      obj %>% add_item_to_rdf(subject = buildingSubject, 
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- format_iso_8601z(indDfAux$start)
      indDfAux$end <- format_iso_8601z(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(lubridate::as.period(frequency)>=lubridate::as.period("P1M")){
        indDfAuxMeta <- data.frame(
          `individualSubject` = namespace_integrator(buildingSubject, namespaces),
          `keyPerformanceIndicator` = indicator,
          `measuredProperty` = namespace_integrator(paste0("bigg:",measuredProperty), namespaces),
          `measuredPropertyComponent` = namespace_integrator(paste0("bigg:",measuredPropertyComponent), namespaces),
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicator]], namespaces),
          `frequency` = frequency,
          `modelSubject` = namespace_integrator(modelSubject, namespaces),
          `modelName` = if(is.na(modelSubject)){NA}else{namespace_integrator(paste0("bigg:",modelName), namespaces)},
          `modelBased` = !is.na(modelSubject)
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% 
          filter(is.finite(value)) %>%
          mutate(
            start = parsedate::parse_iso_8601(start),
            end = parsedate::parse_iso_8601(end)
          ) %>%
          {
            if(lubridate::as.period(frequency)>=lubridate::as.period("P1Y")){
              mutate(
                .,
                year = year(lubridate::with_tz(start, localTimeZone))
              )
            } else {
              mutate(
                .,
                year = year(lubridate::with_tz(start, localTimeZone)),
                month = month(lubridate::with_tz(start, localTimeZone))
              )
            }
          }
      }
    }
  }
  return(list(results_rdf=obj, results_ts=results_ts))
}

#' Generate the harmonised results of the Energy Efficiency Measures (EEM) assessment
#' 
#' Generate the results in harmonised format to BIGG ontology for the EEM assessment in buildings. 
#' Results are always related to project of EEMs, which are single or combinations of multiple EEMs. 
#' 
#' @param data <data.frame> that contains all resultant time series.
#' @param indicators <array> of strings defining the KPIs to calculate. Possible values: 
#' EnergyUseSavings, EnergyUseSavingsRelative, EnergyUseSavingsIntensity, 
#' EnergyCostSavings, EnergyCostSavingsRelative, EnergyCostSavingsIntensity, 
#' EnergyEmissionsSavings, EnergyEmissionsSavingsRelative, EnergyEmissionsSavingsIntensity.
#' @param indicatorsNotAggregableByTime <string> defining the KPI to calculate. 
#' Possible values: NormalisedInvestmentCost, AvoidanceCost, SimplePayback, NetPresentValue, 
#' ProfitabilityIndex, NetPresentValueQuotient, InternalRateOfReturn.
#' @param measuredProperty <uri> defining the energy consumption measured property. 
#' (E.g.: http://bigg-project.eu/ontology#EnergyConsumptionGridElectricity)
#' @param measuredPropertyComponent <uri> defining the energy consumption component name. 
#' (E.g.: "http://bigg-project.eu/ontology#Heating").
#' @param frequencies <array> of strings defining the frequencies used to resample the results. 
#' They must follow ISO 8601 format representing the time step. 
#' Examples: 'P1D' (One day), 'P1Y' (One year), 'P1M' (One month), 'P1DT12H' (One day and a half)...
#' @param buildingId <string> building unique identifier.
#' @param buildingSubject <uri> building unique URI.
#' @param timeColumn <string> of the time column name.
#' @param localTimeZone <string> specifying the local time zone related to
#' the building in analysis. The format of this time zones are defined by
#' the IANA Time Zone Database (https://www.iana.org/time-zones).
#' @param eemProjectDf <data.frame> defining the single EEMs applied to the project. 
#' Columns that must be available: eemSubject, ExchangeRate, Investment, Date, Currency, 
#' Type, AffectationShare, buildingSubject, buildingElement, Lifespan, DiscountRate, eemProjectId
#' @param consumptionColumn <string> defining the energy consumption column name in data.
#' @param indicatorsTimeAggregationFunctions named <array> of the aggregation function used 
#' for each indicator. Possible values are: "SUM", "AVG", "WEIGHTED-AVG".
#' @param indicatorsUnitsSubjects named <array> of URIs containing the units for each indicator.
#' @param baselineConsumptionColumn <string> defining the counterfactual energy consumption column 
#' name in data. By default it is not considered.
#' @param buildingGrossFloorArea <float> defining the gross floor area of the building. By default 
#' it is not considered.
#' @param carbonEmissionsColumn <string> defining the column in data related with the CO2 emissions 
#' factor of energy consumption. By default it is not considered.
#' @param energyPriceColumn <string> defining the column in data related with the price of energy 
#' consumption. By default it is not considered.
#' @param modelName <string> Name of the model used to predict the consumption. By default results 
#' are not based by any model.
#' @param modelId <string> Identifier of the model used to predict the consumption. By default 
#' results are not based by any model.
#' @param modelLocation <string> containing the model path in the model storage infrastructure. 
#' By default results are not based by any model.
#' @param modelStorageInfrastructureSubject <uri> of the model infrastructure type. 
#' By default results are not based by any model.
#' @param estimateWhenAggregate <boolean> defining if a linear estimation should be made when 
#' gaps are found in the time-aggregation of the results. By default, the estimation is done.
#' @param prevResults <list> with the previous results of this function. By default, no previous 
#' results are considered.
#' @return <list> containing a knowledge graph with all resultant metadata and time series 
#' objects, that later can be transformed to TTL and JSON files.

generate_eem_assessment_indicators <- function(
    data, indicators, indicatorsNotAggregableByTime, measuredProperty, measuredPropertyComponent, frequencies, 
    buildingId, buildingSubject, timeColumn, localTimeZone, eemProjectDf,
    consumptionColumn, indicatorsTimeAggregationFunctions, indicatorsUnitsSubjects, baselineConsumptionColumn = NULL, 
    buildingGrossFloorArea = NA, carbonEmissionsColumn = NULL, energyPriceColumn = NULL, 
    modelName = NULL, modelId = NULL, modelLocation = NULL, modelStorageInfrastructureSubject = NULL, 
    estimateWhenAggregate = T, prevResults = NULL) {
  
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
                                     `bigg:projectGenerationDate` = format_iso_8601z(lubridate::now("UTC")),
                                     `bigg:projectOperationalDate` = format_iso_8601z(lubridate::with_tz(max(eemProjectDf$Date),"UTC")),
                                     `bigg:projectStartDate` = format_iso_8601z(lubridate::with_tz(min(eemProjectDf$Date),"UTC")),
                                     `bigg:projectEndDate` = format_iso_8601z(lubridate::with_tz(max(eemProjectDf$Date),"UTC")),
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
                                            `bigg:modelTrainedDate` = format_iso_8601z(lubridate::now("UTC")), 
                                            `bigg:modelName` = modelName), 
                                     objectProperties = list(
                                       `bigg:hasModelStorageInfrastructure` = modelStorageInfrastructureSubject, 
                                       `bigg:hasModelType` = "bigg:BaselineModel"), namespaces = namespaces)
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
        n <- hourly_timesteps(as.numeric(lubridate::as.period(frequency))/3600, 
                         originalDataPeriod)
      } else {
        # When no frequency, calculate the indicators only considering the first 365 days
        indDf <- indDf %>% filter(time <= min(time,na.rm=T)+days(365))
        n <- nrow(indDf)
      }
      indDfAux <- indDf %>%  {
          if(frequency==""){
            group_by(., start = dplyr::first(time))
          } else {
            group_by(., start = lubridate::floor_date(time, lubridate::as.period(frequency), 
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
      indDfAux$start <- lubridate::with_tz(indDfAux$start, "UTC")
      if(frequency==""){
        indDfAux$end <- last(indDf$time)
      } else {
        if (lubridate::as.period(frequency) >= lubridate::as.period("P1D")) {
          indDfAux$end <- lubridate::with_tz(lubridate::with_tz(indDfAux$start, 
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
      singleKPISubjectHash <- digest::digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
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
      obj %>% add_item_to_rdf(singleKPIPointSubject, 
                              classes=c("bigg:TimeSeriesPoint","bigg:SingleKPIAssessmentPoint"),
                              namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = projectSubject, 
                              classes = "bigg:KPICalculationItem",
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- format_iso_8601z(indDfAux$start)
      indDfAux$end <- format_iso_8601z(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(frequency=="" | lubridate::as.period(frequency)>=lubridate::as.period("P1M")){
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
            } else if(lubridate::as.period(frequency)>=lubridate::as.period("P1Y")){
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
      singleKPISubjectHash <- digest::digest(namespace_integrator(singleKPISubject, namespaces), "sha256", serialize = T)
      singleKPIPointSubject <- paste0("biggresults:", singleKPISubjectHash)
      obj %>% add_item_to_rdf(subject = singleKPISubject, 
                              classes = c("bigg:SingleKPIAssessment", "bigg:KPIAssessment", 
                                          "bigg:TimeSeriesList"), 
                              dataProperties = list(
                                `bigg:timeSeriesIsRegular` = F, 
                                `bigg:timeSeriesIsOnChange` = F, 
                                `bigg:timeSeriesIsCumulative` = F, 
                                `bigg:timeSeriesStart` = min(indDfAux$start, na.rm = T), 
                                `bigg:timeSeriesEnd` = max(indDfAux$end,  na.rm = T)),
                              objectProperties = c(list(
                                `bigg:hasKPIUnit` = indicatorsUnitsSubjects[[indicatorNABT]], 
                                `bigg:hasSingleKPIPoint` = singleKPIPointSubject, 
                                `bigg:quantifiesKPI` = keyPerformanceIndicatorSubject,
                                `bigg:hasMeasuredPropertyComponent` = "bigg:Total",
                                `bigg:hasMeasuredProperty` = if(startsWith(measuredProperty,"bigg:")){ 
                                  measuredProperty
                                } else {paste0("bigg:",measuredProperty)}),
                                if (!is.null(modelId)) { list(
                                  `bigg:isEstimatedByModel` = modelSubject)
                                }
                              ), namespaces = namespaces)
      obj %>% add_item_to_rdf(singleKPIPointSubject, 
                              classes=c("bigg:TimeSeriesPoint","bigg:SingleKPIAssessmentPoint"),
                              namespaces = namespaces)
      obj %>% add_item_to_rdf(subject = projectSubject, 
                              classes = "bigg:KPICalculationItem",
                              objectProperties = list(`bigg:assessesSingleKPI` = singleKPISubject), 
                              namespaces = namespaces)
      indDfAux$start <- format_iso_8601z(indDfAux$start)
      indDfAux$end <- format_iso_8601z(indDfAux$end)
      
      results_ts[[singleKPISubjectHash]] <- list()
      results_ts[[singleKPISubjectHash]]$basic <- indDfAux
      if(frequency=="" | lubridate::as.period(frequency)>=lubridate::as.period("P1M")){
        indDfAuxMeta <- data.frame(
          `individualSubject` = namespace_integrator(projectSubject, namespaces),
          `keyPerformanceIndicator` = indicatorNABT,
          `measuredProperty` = namespace_integrator(paste0("bigg:",measuredProperty), namespaces),
          `measuredPropertyComponent` = namespace_integrator("bigg:Total", namespaces),
          `unit` = namespace_integrator(indicatorsUnitsSubjects[[indicatorNABT]], namespaces),
          `frequency` = frequency,
          `modelSubject` = namespace_integrator(modelSubject, namespaces),
          `modelName` = namespace_integrator(paste0("bigg:",modelName), namespaces),
          `modelBased` = !is.na(modelSubject)
        )
        results_ts[[singleKPISubjectHash]]$full <- cbind(indDfAux,indDfAuxMeta)
        results_ts[[singleKPISubjectHash]]$full <- 
          results_ts[[singleKPISubjectHash]]$full %>% 
          filter(is.finite(value)) %>%
          mutate(
            start = parsedate::parse_iso_8601(start),
            end = parsedate::parse_iso_8601(end)
          )
      }
    }
  }
  
  return(list(results_rdf=obj, results_ts=results_ts))
}

