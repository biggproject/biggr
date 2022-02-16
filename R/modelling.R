###
### Model candidates auxiliar functions ----
###

AR_term <- function(features,orders){
  paste(
    mapply(features,FUN=function(feat){
      if(grepl(":",feat)){
        #feat=c("h_sin_1:s1","h_sin_2:s1")
        #orders=2
        paste(mapply(function(l){
          if(l==0){
            feat
          } else {
            paste(paste0(strsplit(feat,":")[[1]],"_l",l),collapse=":")
          }
        },orders), collapse=" + ")
      } else {
        paste(mapply(function(l)if(l==0){feat}else{paste0(feat,"_l",l)}, orders), collapse=" + ")
      }
    }),
    collapse=" + "
  )
}

###
### Model candidates definition ----
###

PenalisedLM <- function(input_parameters){
  input_parameters$label <- input_parameters$parameters
  modelFramework <- list(
    label = "penalisedRegression",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = 
      function(x, y, len = NULL, search = "grid") {
        p <- ncol(x)
        r <- nrow(x)
        if(search == "grid") {
          grid <- expand.grid(mapply(function(k)1:len,1:r))
          colnames(grid) <- colnames(x)
        } else {
          grid <- expand.grid(mapply(function(k)sample(1:p, size = len),1:r))
          colnames(grid) <- colnames(x)
        }
      },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, forcePositiveTerms=NULL) {
      # x<<-x
      # y<<-y
      # params <<- param
      # #param <- params
      # formulaTerms <<- formulaTerms
      # transformationSentences <<- transformationSentences
      # forcePositiveTerms <<- forcePositiveTerms
      
      features <- all.vars(formulaTerms)[2:length(all.vars(formulaTerms))]
      outputName <- all.vars(formulaTerms)[1]
      
      # Join x and y in a single data.frame
      data <- if(is.data.frame(x)) x else as.data.frame(x)
      data[,outputName] <- y
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=data, features=features, transformationSentences = transformationSentences, 
        param = param)
      data <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      transformationItems <- transformation$items
      transformationResults <- transformation$results
      
      # Generate the lags, if needed
      data <- data[complete.cases(data[,featuresAll]),]
      
      # Generate the formula
      form <- as.formula(
        sprintf("%s ~ %s",
                outputName,
                paste("0",
                      paste0(do.call(
                        c,
                        lapply(
                          features,
                          function(f){
                            f_ <- f
                            if(f %in% names(transformationItems)){
                              f <- transformationItems[[f]]$formula
                            }
                            f
                          }
                        )
                      ),
                      collapse="+"),
                      sep=" + ")
        )
      )
      positivityOfTerms <- do.call(
        c,
        lapply(
          features,
          function(f){
            f_ <- f
            if(f %in% names(transformationItems)){
              f <- transformationItems[[f]]$formula
            }
            if(f_ %in% forcePositiveTerms){
              rep(T,length(f))
            } else {
              rep(F,length(f))
            }
          }
        )
      )
      
      # Train the model
      y <- model.frame(form,data)[,1]
      y <- log(y)
      x <- as.matrix(model.matrix(form,data))
      mod <- list("model"=tryCatch({
        penalized::penalized(
          y,x,~0,positive = positivityOfTerms,
          lambda1 = 0,lambda2 = 0,
          #startbeta = ifelse(grepl("temperature|GHI|windSpeed",colnames(x)),0,1),
          trace=F
        )
      }, error=function(e){
        penalized::penalized(
          y,x,~0,positive = positivityOfTerms,
          lambda1 = 0.5,lambda2 = 0.5,
          #startbeta = ifelse(grepl("temperature|GHI|windSpeed",colnames(x)),0,1),
          trace=F
        )
      }))

      # Store the meta variables
      mod$meta <- list(
        features = features,
        outputName = outputName,
        formula = form,
        param = param,
        transformationSentences = transformationSentences,
        transformationResults = transformationResults
      )
      mod
      
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL) {
      
      newdata <- as.data.frame(newdata)
      features <- modelFit$meta$features
      param <- modelFit$meta$param
      
      # Initialize the global input features if needed
      # Change the inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Load the model input and output initialisation features directly from the model
      transformationSentences <- modelFit$meta$transformationSentences
      transformationResults <- modelFit$meta$transformationResults
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=newdata, features=features, transformationSentences = transformationSentences, 
        transformationResults = transformationResults, param = param)
      newdata <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      
      # Change the transformed inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Predict
      options(na.action='na.pass')
      newdata[,modelFit$meta$outputName] <- exp(as.numeric(
        (
          as.matrix(
            model.matrix(modelFit$meta$formula[-2],newdata)
          )[,names(coef(modelFit$model))]
        ) %*% coef(modelFit$model)
      ))
      newdata[,modelFit$meta$outputName]
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

ARX <- function(input_parameters){
  input_parameters$label <- input_parameters$parameters
  modelFramework <- list(
    label = "ARX",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = 
      function(x, y, len = NULL, search = "grid") {
        p <- ncol(x)
        r <- nrow(x)
        if(search == "grid") {
          grid <- expand.grid(mapply(function(k)1:len,1:r))
          colnames(grid) <- colnames(x)
        } else {
          grid <- expand.grid(mapply(function(k)sample(1:p, size = len),1:r))
          colnames(grid) <- colnames(x)
        }
      },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, transformationSentences=NULL) {
      
      features <- all.vars(formulaTerms)[2:length(all.vars(formulaTerms))]
      outputName <- all.vars(formulaTerms)[1]
      if(paste0("AR_",outputName) %in% colnames(param)){
        if(as.logical(param[paste0("AR_",outputName)]==0)){
          param <- param[,-which(colnames(param) %in% paste0("AR_",outputName))]
        }
        features <- c(outputName, features)
      }
      # Join x and y in a single data.frame
      data <- if(is.data.frame(x)) x else as.data.frame(x)
      data[,outputName] <- y
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=data, features=features, transformationSentences = transformationSentences, 
        param = param)
      data <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      transformationItems <- transformation$items
      transformationResults <- transformation$results
      
      # Generate the lags, if needed
      if(any(grepl("^AR_",names(param)))){
        maxLag <- max(param[grepl("^AR_",names(param))])
      } else {
        maxLag <- 0
      }
      data <- lag_components(data,maxLag,featuresAll)
      data <- data[complete.cases(data[,featuresAll]),]
      
      # Generate the lagged formula for the ARX
      ARX_form <- as.formula(
        sprintf("%s ~ %s",
                outputName,
                paste("0",
                      paste0(do.call(
                        c,
                        lapply(
                          features,
                          function(f){
                            f_ <- f
                            if(f %in% names(transformationItems)){
                              f <- transformationItems[[f]]$formula
                            }
                            AR_term(f,
                                    if(!(paste0("AR_",f_) %in% colnames(param))){
                                      0
                                    } else if(f[1]==outputName){
                                      1:param[,paste0("AR_",f_)]
                                    } else {
                                      0:param[,paste0("AR_",f_)]
                                    })
                          }
                        )
                      ),
                      collapse="+"),
                      sep=" + ")
        )
      )
      
      # Train the model
      mod <- lm(
        ARX_form,
        data
      )
      
      # Store the meta variables
      mod$meta <- list(
        features = features,
        outputName = outputName,
        outputInit = setNames(
          list(data[min(nrow(data),nrow(data)-maxLag+1):nrow(data),outputName]),
          outputName
        ),
        inputInit = setNames(
          lapply(features[!(features %in% outputName)],function(f){data[min(nrow(data),nrow(data)-maxLag+1):nrow(data),f]}),
          features[!(features %in% outputName)]
        ),
        param = param,
        maxLag = maxLag,
        transformationSentences = transformationSentences,
        transformationResults = transformationResults
      )
      mod
      
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL,
                       forceInitOutputFeatures=NULL) {
      
      newdata <- as.data.frame(newdata)
      features <- modelFit$meta$features
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      
      # Initialize the global input features if needed
      # Change the inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Load the model input and output initialisation features directly from the model
      forceInitInputFeatures <- if(is.null(forceInitInputFeatures)){modelFit$meta$inputInit}else{forceInitInputFeatures}
      forceInitOutputFeatures <- if(is.null(forceInitOutputFeatures)){modelFit$meta$outputInit}else{forceInitOutputFeatures}
      transformationSentences <- modelFit$meta$transformationSentences
      transformationResults <- modelFit$meta$transformationResults
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=newdata, features=features, transformationSentences = transformationSentences, 
        transformationResults = transformationResults, param = param)
      newdata <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      
      # Change the transformed inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = featuresAll,#modelFit$meta$features, 
                                forceGlobalInputFeatures = forceGlobalInputFeatures)
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(paste("AR",modelFit$meta$outputName,sep="_") %in% colnames(param)){
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = featuresAll,
                                    predictionStep = i-1,
                                    forceInitInputFeatures = forceInitInputFeatures,
                                    forceInitOutputFeatures = forceInitOutputFeatures)
          newdata[i,modelFit$meta$outputName] <- predict(modelFit,newdata[i,])
        }
      } else {
        newdata[,modelFit$meta$outputName] <- predict(modelFit,newdata)
      }
      newdata[,modelFit$meta$outputName]
      
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

RLS <- function(input_parameters){
  input_parameters$label <- input_parameters$parameters
  modelFramework <- list(
    label = "RLS",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = 
      function(x, y, len = NULL, search = "grid") {
        p <- ncol(x)
        r <- nrow(x)
        if(search == "grid") {
          grid <- expand.grid(mapply(function(k)1:len,1:r))
          colnames(grid) <- colnames(x)
        } else {
          grid <- expand.grid(mapply(function(k)sample(1:p, size = len),1:r))
          colnames(grid) <- colnames(x)
        }
      },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL) {
      
      x <<- x
      y <<- y
      transformationSentences <<- transformationSentences
      formulaTerms <<- formulaTerms

      features <- all.vars(formulaTerms)[2:length(all.vars(formulaTerms))]
      outputName <- all.vars(formulaTerms)[1]
      if(paste0("AR_",outputName) %in% colnames(param)){
        if(as.logical(param[paste0("AR_",outputName)]==0)){
          param <- param[,-which(colnames(param) %in% paste0("AR_",outputName))]
        }
        features <- c(outputName, features)
      }
      
      # Join x and y in a single data.frame
      data <- if(is.data.frame(x)) x else as.data.frame(x)
      data[,outputName] <- y
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=data, features=features, transformationSentences = transformationSentences, 
        param = param)
      data <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      transformationItems <- transformation$items
      transformationResults <- transformation$results
      
      # Generate the lags, if needed
      if(any(grepl("^AR_",names(param)))){
        maxLag <- max(param[grepl("^AR_",names(param))])
      } else {
        maxLag <- 0
      }
      data <- lag_components(data,maxLag,featuresAll)
      data <- data[complete.cases(data[,featuresAll]),]
      
      # Generate the lagged formula for the ARX
      ARX_form <- as.formula(
        sprintf("%s ~ %s",
                outputName,
                paste("0",
                      paste0(do.call(
                        c,
                        lapply(
                          features,
                          function(f){
                            f_ <- f
                            if(f %in% names(transformationItems)){
                              f <- transformationItems[[f]]$formula
                            }
                            AR_term(f,
                                    if(!(paste0("AR_",f_) %in% colnames(param))){
                                      0
                                    } else if(f[1]==outputName){
                                      1:param[,paste0("AR_",f_)]
                                    } else {
                                      0:param[,paste0("AR_",f_)]
                                    })
                          }
                        )
                      ),
                      collapse="+"),
                      sep=" + ")
        )
      )
      
      data <- data[order(data$time),]
      
      modlm <- lm(ARX_form, data)
      data2 <- data
      
      data2$outliers <- detect_ts_calendar_model_outliers(data = data, localTimeColumn="localtime", valueColumn=outputName,
                                        window = 2*31*24*60*60)
      outlier_dates <- (data2[,c("localtime","outliers")] %>% group_by(date = as.Date(localtime)) %>%
        summarise(outliers = sum(outliers)) %>% filter(outliers > 0) %>% select(date))$date
      
      # data2$pred <- predict.lm(modlm,data)
      # data2$rmse <- sqrt((data2[,outputName]-data2$pred)^2)
      # data3 <- data2[,c("season","localtime",outputName)] %>% 
      #   group_by(year_weekday=strftime(localtime,"%Y-%w"),season) %>% 
      #   summarise(median_output=mean(Qe,na.rm=T))
      # data2 <-
      # data2 <- aggregate(data2[,c(outputName,"pred","rmse")],by=list(date=as.Date(data2$localtime)),
      #                    FUN=mean,na.rm=T)
      # data2$daily_rmse <- sqrt((data2[,outputName]-data2$pred)^2)
      # 
      # # data2$rmse <- na.locf(na.locf(
      # #   rollmean(data2$rmse,24,align="center",partial=T,fill = c(NA,NA,NA)),
      # #   na.rm=F
      # # ),fromLast = T,na.rm=F)
      # data2$window <- strftime(data2$date,tz="UTC","%Y-%m")
      # roll_window <- as.numeric(names(sort(table(table(data2$window)),decreasing = T))[1])
      # data2$rmse_outliers <- 
      #   detect_ts_zscore_outliers(setNames(data2[,c("date","rmse")],c("time","value")),
      #                           zScoreThreshold = 2,
      #                           zScoreExtremesSensitive = T,window = roll_window*24*60*60) #roll window in seconds
      # # ggplotly(ggplot(data2)+geom_line(aes(date,Qe))+geom_line(aes(date,pred),col="red")+
      # #            geom_point(aes(date,ifelse(rmse_outliers,Qe,NA)),col="yellow")+
      # #            geom_line(aes(date,rmse),col="blue",alpha=0.3) + geom_line(aes(date,daily_rmse),col="green",alpha=0.3)
      # # )
      # outlier_dates <- data2$date[data2$rmse_outliers]
      # # outlier_dates <- (data2[,c("localtime","rmse_outliers")] %>% group_by(date = as.Date(localtime)) %>%
      # #   summarise(outliers = sum(rmse_outliers)) %>% filter(outliers > 0) %>% select(date))$date
      
      # Generate the expanded dataset for inputs
      data <- data[!(as.Date(data$localtime) %in% outlier_dates),]
      data_matrix <- model.matrix(ARX_form,data)
      colnames(data_matrix) <- gsub(":","_",colnames(data_matrix))
      
      # Create the model object
      model <- forecastmodel$new()
      model$output <- outputName
      do.call(model$add_inputs,as.list(setNames(colnames(data_matrix),colnames(data_matrix))))
      model$add_regprm("rls_prm(lambda=0.9)")
      model$kseq <- 0
      
      # Data transformation for RLS framework
      data_for_rls <- setNames(
        lapply(colnames(data_matrix),
               function(x) data.frame("k0"=data_matrix[,x])),
        colnames(data_matrix)
      )
      data_for_rls[["t"]] <- data$time
      data_for_rls[[outputName]] <- data[,outputName]
      data_for_rls$scoreperiod <- sample(c(F,T),length(data_for_rls$t),replace = T,prob = c(0.5,0.5))
      
      # Fit the RLS model and obtain the time-varying coefficients
      mod_rls <- rls_fit(c("lambda"=param$lambda),model, data_for_rls, scorefun = rmse, printout = F)
      
      # Store the meta variables
      mod <- list()
      mod$coefficients <- mod_rls$Lfitval$k0
      mod$time <- mod_rls$data$t
      mod$yreal <- mod_rls$data$Qe
      mod$yhat <- mod_rls$Yhat$k0
      mod$meta <- list(
        features = features,
        outputName = outputName,
        formula = ARX_form,
        outputInit = setNames(
          list(data[min(nrow(data),nrow(data)-maxLag+1):nrow(data),outputName]),
          outputName
        ),
        inputInit = setNames(
          lapply(features[!(features %in% outputName)],function(f){data[min(nrow(data),nrow(data)-maxLag+1):nrow(data),f]}),
          features[!(features %in% outputName)]
        ),
        param = param,
        maxLag = maxLag,
        transformationSentences = transformationSentences,
        transformationResults = transformationResults
      )
      mod
      # colnames(mod$coefficients)[60]
      # ggplotly(ggplot()+geom_line(aes(mod$time,mod$coefficients[,60])))
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL,
                       forceInitOutputFeatures=NULL, model_horizon_in_hours=1, model_window="%Y-%m-%d", model_selection="rmse") {
      # modelFit <<- modelFit
      # newdata <<- newdata
      newdata <- as.data.frame(newdata)
      newdata <- newdata[order(newdata$time),]
      features <- modelFit$meta$features
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      
      # Initialize the global input features if needed
      # Change the inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Load the model input and output initialisation features directly from the model
      forceInitInputFeatures <- if(is.null(forceInitInputFeatures)){modelFit$meta$inputInit}else{forceInitInputFeatures}
      forceInitOutputFeatures <- if(is.null(forceInitOutputFeatures)){modelFit$meta$outputInit}else{forceInitOutputFeatures}
      transformationSentences <- modelFit$meta$transformationSentences
      transformationResults <- modelFit$meta$transformationResults
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=newdata, features=features, transformationSentences = transformationSentences, 
        transformationResults = transformationResults, param = param)
      newdata <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      
      # Change the transformed inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==nrow(newdata))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          newdata[,f] <- forceGlobalInputFeatures[[f]]
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = featuresAll,#modelFit$meta$features, 
                                forceGlobalInputFeatures = forceGlobalInputFeatures)
      
      # Data transformation for RLS framework
      model_formula <- update.formula(modelFit$meta$formula,NULL~.)
      newdata_matrix <- model.matrix(model_formula,newdata)
      colnames(newdata_matrix) <- gsub(":","_",colnames(newdata_matrix))
      
      all_times <- suppressMessages(pad(data.frame("time"=newdata$time),by = "time"))
      mod_coef <- suppressMessages(cbind(
        data.frame("yreal" = modelFit$yreal),
        data.frame("time" = modelFit$time),
        modelFit$coefficients) %>% full_join(all_times))
      mod_coef <- mod_coef[order(mod_coef$time),]
      mod_coef <- zoo::na.locf(mod_coef)
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(model_horizon_in_hours == 1){
        if(paste("AR",modelFit$meta$outputName,sep="_") %in% colnames(param)){
          for (i in 1:nrow(newdata)){
            newdata <- lag_components(data = newdata,
                                      maxLag = maxLag,
                                      featuresNames = featuresAll,
                                      predictionStep = i-1,
                                      forceInitInputFeatures = forceInitInputFeatures,
                                      forceInitOutputFeatures = forceInitOutputFeatures)
            newdata[i,modelFit$meta$outputName] <- sum(
              newdata_matrix[i,colnames(modelFit$coefficients)] * 
                mod_coef[mod_coef$time==newdata$time[i],colnames(modelFit$coefficients)]
            )
          }
        } else {
          newdata[,modelFit$meta$outputName] <- rowSums(
            newdata_matrix[,colnames(modelFit$coefficients)] * 
              mod_coef[mod_coef$time %in% newdata$time,colnames(modelFit$coefficients)],
            na.rm=T
          )
        }
        # return the output
        newdata[,modelFit$meta$outputName]
      } else {
        # When predicting a fixed horizon with each model coefficients set
        if(paste("AR",modelFit$meta$outputName,sep="_") %in% colnames(param)){
          stop("Horizons per step higher than 1 are not allowed when predicting multiple step ahead of ARX models")
        } else {
          mod_coef_timestep <- detect_time_step(mod_coef)
          mod_coef <- mod_coef[mod_coef$time %in% newdata$time,]
          mod_coef$yhat <- rowSums(
            newdata_matrix[,colnames(modelFit$coefficients)] * 
              mod_coef[,colnames(modelFit$coefficients)],
            na.rm=T
          )
          mod_coef$window <- strftime(mod_coef$time,tz="UTC",model_window)
          roll_window <- as.numeric(names(sort(table(table(mod_coef$window)),decreasing = T))[1])
          mod_coef$rmse <- sqrt((mod_coef$yhat-mod_coef$yreal)**2)
          mod_coef$rmse <- zoo::rollapply(mod_coef$rmse,width = roll_window,align = "center",fill = c(NA,NA,NA),partial=T,
                                          FUN=function(a){mean(a,na.rm=T)})
          mod_coef_summary <- mod_coef %>% 
            group_by(window) %>% 
            summarise(
              min_rmse = time[which.min(rmse)],
              random_select = sample(time,size = 1)
            )
          if(model_selection=="random"){
            mod_coef <- mod_coef %>% 
              left_join(mod_coef_summary,by = "window") %>%
              filter(time==random_select)
          } else if(model_selection=="rmse"){
            mod_coef <- mod_coef %>% 
              left_join(mod_coef_summary,by = "window") %>%
              filter(time==min_rmse)
          }
          multiple_preds <- lapply(1:nrow(mod_coef),
                 function(x){
                   mod_coef_aux <- mod_coef[x,]
                   allowed_times <- newdata$time >= mod_coef_aux[1,"time"] & 
                       newdata$time <= mod_coef_aux[1,"time"] + hours(model_horizon_in_hours)
                   setNames(
                     data.frame(
                      newdata$time[allowed_times],
                      newdata_matrix[allowed_times,colnames(modelFit$coefficients)] %*% 
                        t(mod_coef_aux[,colnames(modelFit$coefficients)])
                     ), c("time",
                        paste0("yhat_",strftime(mod_coef_aux[1,"time"],format="%Y%m%dT%H%M%S",tz = "UTC")))
                   )
                 })
          all_preds <- Reduce(
            function(x, y, ...) merge(x, y, all = TRUE, ...),
            multiple_preds
          )
          cbind(
            "time"=all_preds$time,
            "n"=sum(!(colnames(all_preds) %in% "time" )) - 
              matrixStats::rowCounts(
                as.matrix(all_preds[,!(colnames(all_preds) %in% "time" )]),value=NA),
            as.data.frame(
              matrixStats::rowQuantiles(as.matrix(all_preds[,!(colnames(all_preds) %in% "time" )]),
                                    probs = c(0.05,0.1,0.25,0.375,0.5,0.625,0.75,0.9,0.95),na.rm = T)),
            setNames(data.frame(
              matrixStats::rowRanges(as.matrix(all_preds[,!(colnames(all_preds) %in% "time" )]),na.rm=T)),
              c("min","max")),
            setNames(data.frame(
              matrixStats::rowMeans2(as.matrix(all_preds[,!(colnames(all_preds) %in% "time" )]),na.rm=T)),
              c("mean"))
          )
        }
        
      }
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

###
### Reformulation of Caret package functions ----
###

train.formula <- function (form, data, weights, subset, na.action = na.fail, 
                           contrasts = NULL, ...) 
{
  m <- match.call(expand.dots = FALSE)
  # Add intercept if needed
  if (!("intercept" %in% colnames(data))){
    data$intercept <- 1
  }
  # Add features that are not directly specified in data, but are defined during
  # the transformation process
  transformationSentences <- m$...$transformationSentences
  if (!is.null(transformationSentences)){
    for(f in all.vars(m$form)[!(all.vars(m$form) %in% colnames(data))]){
      if(any(f %in% names(transformationSentences))){
        data[,f] <- 0
      } else {
        stop(sprintf("Feature %s was not found in data, neither specified in transformationSentences argument", f))
      }
    }
  }
  m$data <- data
  # continue caret official source code...
  if (is.matrix(eval.parent(m$data)))
    m$data <- as.data.frame(m$data, stringsAsFactors = TRUE)
  m$... <- m$contrasts <- NULL
  caret:::check_na_conflict(match.call(expand.dots = TRUE))
  if (!("na.action" %in% names(m)))
    m$na.action <- quote(na.fail)
  m[[1]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  if (nrow(m) < 1)
    stop("Every row has at least one missing value were found", 
         call. = FALSE)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  int_flag <- grepl("(Intercept)", colnames(x))
  
  if (any(int_flag)) 
    x <- x[, !int_flag, drop = FALSE]
  w <- as.vector(model.weights(m))
  y <- model.response(m)
  # Force the inclusion of all data columns, they might be needed by some transformation procedure
  if (!is.null(transformationSentences)){
    x <- cbind(x,data[,!(colnames(data) %in% colnames(m))])
  }
  # continue caret official source code...
  res <- train(x, y, weights = w, formulaTerms=Terms, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  if (!is.null(res$trainingData)) {
    res$trainingData <- data[, all.vars(Terms), drop = FALSE]
    isY <- names(res$trainingData) %in% as.character(form[[2]])
    if (any(isY)) 
      colnames(res$trainingData)[isY] <- ".outcome"
  }
  class(res) <- c("train", "train.formula")
  res
}

predict.train <- function (object, newdata = NULL, type = "raw", na.action = na.omit, ...){
  if (all(names(object) != "modelInfo")) {
    object <- update(object, param = NULL)
  }
  if (!is.null(object$modelInfo$library))
    for (i in object$modelInfo$library) do.call("requireNamespaceQuietStop", list(package = i))
  if (!(type %in% c("raw", "prob"))) 
    stop("type must be either \"raw\" or \"prob\"")
  if (type == "prob") {
    if (is.null(object$modelInfo$prob))
      stop("only classification models that produce probabilities are allowed")
  }
  if (!is.null(newdata)) {
    newdata_ini <- newdata
    if (inherits(object, "train.formula")) {
      newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
      # Add intercept if needed
      if(!("intercept" %in% colnames(newdata_ini))){
        newdata_ini$intercept <- 1
      }
      # Add features that are not directly specified in data, but are defined during
      # the transformation process
      neededXvars <- object$finalModel$meta$features[!(object$finalModel$meta$features %in% object$finalModel$meta$outputName)]
      neededXvars <- neededXvars[!(neededXvars %in% colnames(newdata))]
      if (length(neededXvars)>0){
        for(f in neededXvars){
          newdata[,f] <- 0
        }
      }
      # continue normal caret operation...
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses"))) 
        .checkMFClasses(cl, m)
      keep <- match(row.names(m), rn)
      newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
      xint <- match("(Intercept)", colnames(newdata), 
                    nomatch = 0)
      if (xint > 0) 
        newdata <- newdata[, -xint, drop = FALSE]
    }
  }
  else if (object$control$method != "oob") {
    if (!is.null(object$trainingData)) {
      if (object$method == "pam") {
        newdata <- object$finalModel$xData
      }
      else {
        newdata <- object$trainingData
        newdata$.outcome <- NULL
        if ("train.formula" %in% class(object) && any(unlist(lapply(newdata, 
                                                                    is.factor)))) {
          newdata <- model.matrix(~., data = newdata)[,-1]
          newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
        }
      }
    }
    else stop("please specify data via newdata")
  }
  if ("xNames" %in% names(object$finalModel) & is.null(object$preProcess$method$pca) & 
      is.null(object$preProcess$method$ica))
    newdata <- newdata[, colnames(newdata) %in% object$finalModel$xNames,drop = FALSE]
  if ("outputName" %in% names(object$finalModel$meta))
    newdata_ini <- newdata_ini[,!(colnames(newdata_ini) %in% c(object$finalModel$meta$outputName))]
  newdata <- cbind(
    newdata[,!(colnames(newdata) %in% colnames(newdata_ini))],
    newdata_ini
  )
  if (type == "prob") {
    out <- probFunction(method = object$modelInfo, modelFit = object$finalModel, 
                        newdata = newdata, preProc = object$preProcess, ...)
    obsLevels <- levels(object)
    out <- out[, obsLevels, drop = FALSE]
  }
  else {
    out <- predictionFunction(method = object$modelInfo, 
                              modelFit = object$finalModel, newdata = newdata, 
                              preProc = object$preProcess,...)
    if (object$modelType == "Regression") {
      out <- caret:::trimPredictions(pred = out, mod_type = object$modelType, 
                                     bounds = object$control$predictionBounds, limits = object$yLimit)
    }
    else {
      if (!("levels" %in% names(object))) 
        object$levels <- levels(object)
      out <- outcome_conversion(as.character(out), lv = object$levels)
    }
  }
  out
}

predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param=NULL, ...){
  if (!is.null(newdata) && !is.null(preProc))
    newdata <- predict(preProc, newdata)
  out <- method$predict(modelFit = modelFit, newdata = newdata, 
                        submodels = param, ...)
  out
}
