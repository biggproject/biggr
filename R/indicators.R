calculate_and_load_indicators <- function(data, indicators, usage,
                                         frequencies, buildingsRdf, buildingId,
                                         timeColumn, localTimeZone, 
                                         consumptionColumn,
                                         buildingGrossFloorArea = 0,
                                         outdoorTemperatureColumn = NULL, 
                                         carbonEmissionsColumn = NULL, 
                                         energyPriceColumn = NULL, 
                                         dailyFixedPriceColumn = NULL,
                                         modelName = NULL,
                                         modelUri = NULL,
                                         estimateWhenAggregate = T,
                                         outputDirectory = ""
                                         ){
  # data <- df
  # frequencies <- c("PT1H","P1D","P1M","P1Y")
  # buildingGrossFloorArea <- 1000
  # usage <- "Total"
  # indicators <- c("EnergyUse","EnergyUseIntensity")
  # modelId <- NULL
  
  for (indicator in indicators){
    
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
    } else {
      NULL
    }
    indicatorNameJson <- paste0(usage,indicator,"_",buildingId,"_",
                                if(is.null(modelUri)){"Real_"}else{
                                  paste0("Estimated_",modelUri)}
                                ,"_%s.json")
    if(!is.null(valueInd)){
      indDf <- data.frame(
        "time" = data[,timeColumn],
        "ind" = valueInd
      )
      for(frequency in frequencies){
        n <- hourly_timesteps(as.numeric(as.period(frequency))/3600,
                              detect_time_step(data$time))
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
            isReal = if(is.null(modelUri)){
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
        write(jsonlite::toJSON(indDfAux,dataframe = "rows",POSIXt = "ISO8601",na = "null"),
              file=paste(outputDirectory,sprintf(indicatorNameJson,frequency),sep="/"))
      }
    }
    
  }
}
