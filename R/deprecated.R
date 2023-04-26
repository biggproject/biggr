#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

get_KPI_by_building <- function(buildingsRdf,timeseriesObject,buildingSubject,
                                KPI, frequency, ratedBy, localTz){
  
  timeseriesKPI <- get_KPI_timeseries(buildingsRdf, timeseriesObject, buildingSubject,
                                      name = KPI$Name, fromModel = KPI$FromModel, frequency)
  
  if(is.null(timeseriesKPI)) return(NULL)
  
  if(!is.null(KPI$RatedBy)){
    for (rb in KPI$RatedBy){
      timeseriesKPI <- timeseriesKPI %>%
        left_join(
          get_KPI_timeseries(buildingsRdf, timeseriesObject, buildingSubject,
                             rb$Name, rb$FromModel, frequency),by="time")
      timeseriesKPI$value <- ifelse(timeseriesKPI$value.y==0, 0,
                                    timeseriesKPI$value.x / timeseriesKPI$value.y)
      timeseriesKPI$value.x <- timeseriesKPI$value.y <- NULL
    }
  }
  timeseriesKPI[,buildingSubject] <- timeseriesKPI$value
  timeseriesKPI$localtime <- format(with_tz(timeseriesKPI$time, localTz),
                                    format="%Y-%m-%d %H:%M:%S")
  timeseriesKPI$time <- timeseriesKPI$value <- NULL
  
  return(timeseriesKPI)
}

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

get_analytical_groups_by_building_subject <- function(buildingsRdf){
  analytical_groups_by_building <- 
    suppressMessages(buildingsRdf %>% rdf_query(paste0(    
      paste0(mapply(function(i){
        sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
                bigg_namespaces[i])},
        1:length(bigg_namespaces))),
      '
      SELECT ?buildingSubject ?group
      WHERE {
        ?buildingSubject a bigg:Building .
        ?buildingSubject bigg:groupsForAnalytics ?group
      }')))
  if(length(analytical_groups_by_building)>0){
    analytical_groups_by_building <- dummy_cols(analytical_groups_by_building,select_columns = "group")
    analytical_groups_by_building$group <- NULL
    colnames(analytical_groups_by_building) <- gsub("group_","",colnames(analytical_groups_by_building))
    analytical_groups_by_building <- analytical_groups_by_building %>% 
      group_by(buildingSubject) %>% 
      summarise(across(colnames(analytical_groups_by_building)[-1],sum))
    analytical_groups_by_building[,"NA"] <- NULL
    return(analytical_groups_by_building)
  } else {
    return(NULL)
  }  
  return( analytical_groups_by_building )
}

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

get_matrix_building_space_use_types <- function(buildingsRdf, splitSublevels=T){
  types_df <- suppressMessages(buildingsRdf %>% rdf_query(paste0(    
    paste0(mapply(function(i){
      sprintf('PREFIX %s: <%s>', names(bigg_namespaces)[i],
              bigg_namespaces[i])},
      1:length(bigg_namespaces))),
    '
    SELECT ?buildingSubject ?typology
    WHERE {
      ?buildingSubject a bigg:Building .
      ?buildingSubject bigg:hasSpace ?bs .
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
                          data.frame(buildingSubject=types_df$buildingSubject[i],typology=typologies[[i]])
                        }))
    }
    types_df <- dummy_cols(types_df,select_columns = "typology")
    types_df$typology <- NULL
    colnames(types_df) <- gsub("typology_","",colnames(types_df))
    types_df <- types_df %>% group_by(buildingSubject) %>% summarise(across(colnames(types_df)[-1],sum))
    types_df[,"NA"] <- NULL
    types_df$all <- 1
    return(types_df)
  } else {
    return(NULL)
  }  
}