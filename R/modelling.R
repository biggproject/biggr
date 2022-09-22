###
### Model candidates auxiliar functions ----
###

estimate_occupancy <- function(real=NULL, predicted, minOccupancy, timestep){
  
  if(is.null(real)){
    return((minOccupancy+1)/2)
  }
  
  residuals <- real - predicted
  
  mean_rolled_residuals <- loess(y~x,
                                 data.frame("x"=1:length(residuals),"y"=residuals),
                                 na.action = na.omit)
  mean_norm_residuals <- residuals - predict(mean_rolled_residuals,
                                             data.frame("x"=1:length(residuals)))
  
  sd_residuals <- roll_sd(mean_norm_residuals,
                          width = hourly_timesteps(168,timestep),
                          center = T,
                          complete_obs = T)
  
  density_sd_res <- density(sd_residuals,na.rm=T)
  tp <- pastecs::turnpoints(ts(density_sd_res$y))
  importance <- density_sd_res$y[tp$tppos]
  x_values <- density_sd_res$x[tp$tppos]
  shifted_importance <- c(0,shift(importance,1)[is.finite(shift(importance,1))])
  min_max <- ifelse(importance-shifted_importance>0,"max","min")
  sd_threshold <- min(x_values[x_values > x_values[which.max(importance)] &
                                 min_max =="min"],na.rm=T)
  
  theoretical_occupancy <- normalise_range(
    ifelse(sd_residuals>=sd_threshold, 
            normalise_range(
              rollmean(mean_norm_residuals,k=hourly_timesteps(168,timestep),
                     align="left",fill=c(NA,NA,NA),partial=T),
              0,1),
             # rollapply(mean_norm_residuals,
             #          FUN=function(x){mean(normalise_range(x,0,1),na.rm=T)},
             #          width=hourly_timesteps(31*24,timestep),
             #          align = "center", fill = c(NA,NA,NA), 
             #          partial=T),
           0),
    minOccupancy,1)
  
  return(theoretical_occupancy)
}

regression_metrics <- function(data, lev=NULL, model=NULL){
  c(
    CVRMSE = -(RMSE(data$obs,data$pred,na.rm = T)/mean(data$obs,na.rm=T)),
    RMSE = -RMSE(data$obs,data$pred,na.rm = T),
    R2 = cor(data$obs,data$pred,use = "na.or.complete")^2
  ) 
}

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
                   transformationSentences=NULL, forcePositiveTerms=NULL, logOutput=F) {
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
      if(logOutput) y <- log(y)
      x <- as.matrix(model.matrix.lm(form,data,na.action=na.pass))
      y <- y[complete.cases(x)] 
      x <- x[complete.cases(x), ]
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
        logOutput = logOutput,
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
      logOutput <- modelFit$meta$logOutput
      
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
      newdata[,outputName] <- as.numeric(
        (
          as.matrix(
            model.matrix(modelFit$meta$formula[-2],newdata)
          )[,names(coef(modelFit$model))]
        ) %*% coef(modelFit$model)
      )
      if(logOutput) newdata[,outputName] <- exp(newdata[,outputName])
      newdata[,outputName]
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
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, logOutput=T) {
      
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
      if(logOutput) {data[,outputName] <- log(y)} else {data[,outputName] <- y}
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=data, 
        features=features, 
        transformationSentences = transformationSentences, 
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
        logOutput = logOutput,
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
      logOutput <- modelFit$meta$logOutput
      
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
      if(logOutput) { 
        exp(newdata[,modelFit$meta$outputName])
      } else {
        newdata[,modelFit$meta$outputName]
      }
      
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
                   transformationSentences=NULL, logOutput=F, 
                   minMonthsTraining=0, continuousTime=T,
                   maxPredictionValue=NULL, weatherDependenceByCluster=NULL,
                   clusteringResults=NULL,...
                   #estimateTheoreticalOccupancy=F, 
                   #theoreticalOccupancyColumnName="theoreticalOccupancy"
                   ) {
      # x <<- x
      # y <<- y
      # transformationSentences <<- transformationSentences
      # formulaTerms <<- formulaTerms
      # params <<- param
      # param <- params#mod$bestTune
      print(paste(paste(colnames(param),param[1,],sep=": "),collapse=", "))
      
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
        param = param, weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clusteringResults)
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
      
      data <- data[order(data$localtime),]
      
      # if(estimateTheoreticalOccupancy){
      #   if(!("minocc" %in% colnames(param)))
      #     stop("minocc param is needed when estimating the theoretical 
      #          occupancy")
      #   mod_lm <- lm(ARX_form,data)
      #   theoretical_occupancy <- estimate_occupancy(
      #     real = data[,outputName], 
      #     predicted = predict(mod_lm,data), 
      #     minOccupancy = param$minocc,
      #     timestep = detect_time_step(data$localtime))
      #   # ggplotly(
      #   #   ggplot() +
      #   #     geom_line(aes(1:length(theoretical_occupancy),theoretical_occupancy)) +
      #   #     geom_line(aes(1:nrow(data),data$Qe/max(data$Qe,na.rm=T)),alpha=0.5,col="red")
      #   #   )
      #   data[,theoreticalOccupancyColumnName] <- theoretical_occupancy
      # }
      # plot(mod_lm$model$Qe, type="l")
      # lines(mod_lm$fitted.values,col="red")
      
      # Generate the expanded dataset for inputs
      
      data <- data[complete.cases(
        model.frame(ARX_form,data,na.action = 'na.pass')),]
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
      data_for_rls[[outputName]] <- if (logOutput){
        log(ifelse(data[,outputName]>0,data[,outputName],0.01))
      } else { data[,outputName] }
      if(continuousTime==F){
        data_for_rls[["t"]] <- seq(min(data$localtime),
                                   min(data$localtime) + hours(length(data$localtime)), by="hour")
        data_for_rls[["t"]] <- data_for_rls[["t"]][1:length(data_for_rls[[outputName]])] 
      } else {
        data_for_rls[["t"]] <- data$localtime
      }
      data_for_rls$scoreperiod <- sample(c(F,T),length(data_for_rls$t),replace = T,prob = c(0.9,0.1))
      
      # Fit the RLS model and obtain the time-varying coefficients
      mod_rls <- rls_fit(c("lambda"=param$lambda),model, data_for_rls, scorefun = rmse, printout = F)
      if(continuousTime==F){
        mod_rls$data$t <- data$localtime
      }
      # Store the meta variables
      mod <- list()
      if((min(mod_rls$data$t) + months(minMonthsTraining)) < max(mod_rls$data$t)){
        mod$coefficients <- mod_rls$Lfitval$k0[mod_rls$data$t >= 
                                                 (min(mod_rls$data$t) + months(minMonthsTraining)),]
        mod$coefficients <- rbind(
          setNames(
            as.data.frame(
              matrix(
                rep(as.numeric(mod$coefficients[1,]), sum(mod_rls$data$t < 
                                           (min(mod_rls$data$t) + months(minMonthsTraining)))),
                ncol = ncol(mod$coefficients), byrow = T)
            ),
            colnames(mod$coefficients)),
          mod$coefficients
        )
      } else {
        mod$coefficients <- mod_rls$Lfitval$k0
      }
      mod$localtime <- mod_rls$data$t
      mod$yreal <- if(logOutput){exp(mod_rls$data[[outputName]])}else{mod_rls$data[[outputName]]}
      mod$yhat <- if(logOutput){exp(mod_rls$Yhat$k0)}else{mod_rls$Yhat$k0}
      mod$meta <- list(
        features = features,
        outputName = outputName,
        formula = ARX_form,
        logOutput = logOutput,
        minMonthsTraining = minMonthsTraining,
        maxPredictionValue = maxPredictionValue,
        # estimateTheoreticalOccupancy = if(estimateTheoreticalOccupancy){
        #   list("mod" = mod_lm, "minocc"=param$minocc, 
        #        "columnName" = theoreticalOccupancyColumnName)
        # } else {
        #   list()
        # },
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
        transformationResults = transformationResults,
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clusteringResults
      )
      mod
      
      # ggplotly(ggplot()+geom_line(aes(mod$localtime,mod$yreal)) +
      #            geom_line(aes(mod$localtime,mod$yhat),col="red",alpha=0.4)+
      #   geom_line(aes(mod$localtime,data$coolingLpf), col="blue",alpha=0.3))

    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, 
                       forceInitInputFeatures=NULL, forceInitOutputFeatures=NULL, 
                       modelMinMaxHorizonInHours=1, modelWindow="%Y-%m-%d", 
                       modelSelection="rmse") {
      modelFit <<- modelFit
      newdata <<- newdata
      # newdata <- df_for_pred
      newdata <- as.data.frame(newdata)
      newdata$localtime <- lubridate::with_tz(newdata$time,
                                              lubridate::tz(modelFit$localtime))
      newdata <- newdata[order(newdata$localtime),]
      features <- modelFit$meta$features
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      logOutput <- modelFit$meta$logOutput
      outputName <- modelFit$meta$outputName
      maxPredictionValue <- modelFit$meta$maxPredictionValue
      # estimateTheoreticalOccupancy <- modelFit$meta$estimateTheoreticalOccupancy
      
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
      weatherDependenceByCluster <- modelFit$meta$weatherDependenceByCluster
      clusteringResults <- modelFit$meta$clusteringResults
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=newdata, features=features, transformationSentences = transformationSentences, 
        transformationResults = transformationResults, param = param, 
        weatherDependenceByCluster = weatherDependenceByCluster,
        clusteringResults = clusteringResults)
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
      model_formula <- modelFit$meta$formula#update.formula(modelFit$meta$formula,NULL~.)
      newdata[,outputName] <- NA
      newdata_matrix <- model.matrix.lm(modelFit$meta$formula, newdata, na.action=na.pass)
      newdata[,outputName] <- NULL
      colnames(newdata_matrix) <- gsub(":","_",colnames(newdata_matrix))
      
      all_times <- suppressMessages(pad(data.frame("localtime"=newdata$localtime),by = "localtime"))
      mod_coef <- suppressMessages(cbind(
        data.frame("yhat" = modelFit$yhat),
        data.frame("yreal" = modelFit$yreal),
        data.frame("localtime" = modelFit$localtime),
        modelFit$coefficients) %>% full_join(all_times))
      mod_coef <- mod_coef[order(mod_coef$localtime),]
      mod_coef <- zoo::na.locf(mod_coef)
      
      # if(length(estimateTheoreticalOccupancy)>0){
      #   theoretical_occupancy <- estimate_occupancy(
      #     real = newdata[[outputName]], 
      #     predicted = predict(estimateTheoreticalOccupancy$mod,newdata), 
      #     minOccupancy = estimateTheoreticalOccupancy$minocc,
      #     timestep = detect_time_step(newdata$localtime))
      #   # ggplotly(
      #   #   ggplot() +
      #   #     geom_line(aes(1:length(theoretical_occupancy),theoretical_occupancy)) +
      #   #     geom_line(aes(1:nrow(newdata),newdata$Qe/max(newdata$Qe,na.rm=T)),alpha=0.5,col="red")
      #   #   )
      #   newdata[,estimateTheoreticalOccupancy$columnName] <- theoretical_occupancy
      # }
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(paste("AR",outputName,sep="_") %in% colnames(param)){
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = featuresAll,
                                    predictionStep = i-1,
                                    forceInitInputFeatures = forceInitInputFeatures,
                                    forceInitOutputFeatures = forceInitOutputFeatures)
          newdata[i,outputName] <- sum(
            newdata_matrix[i,colnames(modelFit$coefficients)] * 
              mod_coef[mod_coef$localtime==newdata$localtime[i],colnames(modelFit$coefficients)]
          )
        }
      } else {
        newdata[newdata$localtime %in% mod_coef$localtime, 
                outputName] <- 
          rowSums(
            newdata_matrix[newdata$localtime %in% mod_coef$localtime,
                           colnames(modelFit$coefficients)] * 
              as.matrix(mod_coef[mod_coef$localtime %in% newdata$localtime,colnames(modelFit$coefficients)]),
          na.rm=T
        )
      }
        # return the output
      if(identical(modelMinMaxHorizonInHours,1)){
        result <- 
          if (logOutput) {
            exp(newdata[,outputName])
          } else { 
            newdata[,outputName] 
          }
        if (!is.null(maxPredictionValue)){
          ifelse(result > maxPredictionValue, maxPredictionValue, result)
        } else {
          result
        }
      } else {
        # When predicting a fixed horizon with each model coefficients set
        if(paste("AR",outputName,sep="_") %in% colnames(param)){
          stop("Horizons per step higher than 1 are not allowed when predicting multiple step ahead of ARX models")
        } else {
          mod_coef <- mod_coef[
            mod_coef$localtime >= min(newdata$localtime)-hours(max(modelMinMaxHorizonInHours)) &
            mod_coef$localtime <= max(newdata$localtime),
          ]
          mod_coef$window <- strftime(mod_coef$localtime,modelWindow)
          roll_window <- as.numeric(names(sort(table(table(mod_coef$window)),decreasing = T))[1])
          mod_coef$rmse <- sqrt((mod_coef$yhat-mod_coef$yreal)**2)
          mod_coef$rmse <- zoo::rollapply(mod_coef$rmse,width = roll_window,align = "center",fill = c(NA,NA,NA),partial=T,
                                          FUN=function(a){mean(a,na.rm=T)})
          mod_coef_summary <- mod_coef %>% 
            group_by(window) %>% 
            summarise(
              min_rmse = localtime[which.min(rmse)],
              random_select = sample(localtime,size = 1)
            )
          
          if(modelSelection=="random"){
            mod_coef <- mod_coef %>% 
              left_join(mod_coef_summary,by = "window") %>%
              filter(localtime==random_select)
          } else if(modelSelection=="rmse"){
            mod_coef <- mod_coef %>% 
              left_join(mod_coef_summary,by = "window") %>%
              filter(localtime==min_rmse)
          }
          multiple_preds <- lapply(1:nrow(mod_coef),
                 function(x){
                   mod_coef_aux <- mod_coef[x,]
                   allowed_times <- newdata$localtime >= (mod_coef_aux[1,"localtime"]  + hours(min(modelMinMaxHorizonInHours))) & 
                       newdata$localtime <= (mod_coef_aux[1,"localtime"] + hours(max(modelMinMaxHorizonInHours)))
                   result <- setNames(
                     data.frame(
                      newdata$localtime[allowed_times],
                      if (logOutput) {
                        exp(
                          # mapply(function(i){
                          #   sum(
                          #     newdata_matrix[i,colnames(modelFit$coefficients)] * 
                          #       mod_coef_aux[1,colnames(modelFit$coefficients)],
                          #   na.rm=T)},which(allowed_times))
                          #   
                          newdata_matrix[allowed_times,colnames(modelFit$coefficients)] %*%
                          t(mod_coef_aux[1,colnames(modelFit$coefficients)])
                        )
                      } else {
                        rowSums(
                          newdata_matrix[allowed_times,colnames(modelFit$coefficients)] %*%
                            t(mod_coef_aux[1,colnames(modelFit$coefficients)]),
                          na.rm=T)
                      }
                     ), c("localtime",
                        paste0("yhat_",strftime(mod_coef_aux[1,"localtime"],format="%Y%m%dT%H%M%S",tz = "UTC")))
                   )
                   if(!is.null(maxPredictionValue)){
                    result[,2] <- ifelse(result[,2] > maxPredictionValue, 
                                         maxPredictionValue, 
                                         result[,2])
                   }
                   result
                 })
          all_preds <- Reduce(
            function(x, y, ...) merge(x, y, all = TRUE, ...),
            multiple_preds
          )
          # all_preds <- all_preds[all_preds$localtime %in% newdata$localtime,]
          # all_preds <- all_preds %>% 
          #   right_join(data.frame("localtime"=newdata$localtime),"localtime")
          timeN <- data.frame(
            "localtime"=all_preds$localtime,
            "n"=sum(!(colnames(all_preds) %in% "localtime" )) - 
              matrixStats::rowCounts(
                as.matrix(all_preds[,!(colnames(all_preds) %in% "localtime" )]),value=NA)
          )
          timePred <- 
            data.frame(
              "localtime"=newdata$localtime,
              "pred"= if (logOutput) { exp(newdata[,outputName]) } 
                      else { newdata[,outputName] }
            )
          if(!is.null(maxPredictionValue)){
            timePred[,"pred"] <- ifelse(timePred[,"pred"] > maxPredictionValue, 
                                        maxPredictionValue, 
                                        timePred[,"pred"])
          }
          probs <- c(0.025,0.05,0.1,0.25,0.375,0.5,0.625,0.75,0.9,0.95,0.975)
          timeDistribPred <- cbind(
            "localtime"=all_preds$localtime,
            setNames(data.frame(
              matrixStats::rowQuantiles(as.matrix(all_preds[,!(colnames(all_preds) %in% "localtime" )]),
                                    probs = probs,na.rm = T)),
              mapply(function(x)sprintf("%0.1f%%",x*100),probs)),
            setNames(data.frame(
              matrixStats::rowRanges(as.matrix(all_preds[,!(colnames(all_preds) %in% "localtime" )]),na.rm=T)),
              c("min","max")),
            setNames(data.frame(
              matrixStats::rowMeans2(as.matrix(all_preds[,!(colnames(all_preds) %in% "localtime" )]),na.rm=T)),
              c("mean"))
          )
          result <- merge(timeN, timePred,by="localtime", all=T)
          result <- merge(result, timeDistribPred,by="localtime", all=T)
          result
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
### Optimization of parameters of model candidates ----
###

#' Get single attribute from each of the features in the features list
#'
#' Get single attribute from each of the features in the features list
#'
#' @param features <list> List of features to get attributes from
#' Feature description
#' feature = list(
#'    datatype = <type of data> (discrete | integer | float)
#'    min = <min value>,
#'    max = <max value>,
#'    nlevels = <number of levels>,
#'    levels = <levels> (optional: discrete type specific)
#' )
#' @param name <string> Name of the attribute to get
#' @return <list> of features with the single attribute requested

getAttribute <- function(features, name) {
  lapply(function(feature) {
    if ((name == "nlevels") & (any("levels" %in% names(feature)))) {
      length(feature[["levels"]])
    } else {
      feature[[name]]
    }
  },
  X = features
  )
}

#' Binary encoding of a value (integer representation)
#'
#' Binary encoding of a value (integer representation)
#'
#' @param x <integer> Value to be binary encoded
#' @return <integer> representation of binary coded value

toBin <- function(x) {
  as.integer(paste(rev(as.integer(intToBits(x))), collapse = ""))
}

#' Decode binary representation to value
#'
#' Decode binary representation to value
#'
#' @param binary <integer> Binary encoded value
#' @param features <list> List of features to decode
#' Feature description
#' feature = list(
#'    datatype = <type of data> (discrete | integer | float)
#'    min = <min value>,
#'    max = <max value>,
#'    nlevels = <number of levels>,
#'    levels = <levels> (optional: discrete type specific)
#' )
#' @return <list> of decoded values

decodeValueFromBin <- function(binary, features) {
  datatype <- getAttribute(features, "datatype")
  nlevels <- getAttribute(features, "nlevels")
  levels <- getAttribute(features, "levels")
  min <- getAttribute(features, "min")
  max <- getAttribute(features, "max")

  bitOrders <- mapply(function(x) {
    nchar(toBin(x))
  }, nlevels)

  seq_bitOrders <- c(1)
  if (length(bitOrders) > 1) {
      seq_bitOrders <- seq.int(bitOrders)
  }
  binary <- split(
    binary,
    rep.int(seq_bitOrders, times = bitOrders)
  )

  orders <- sapply(binary, function(x) {
    binary2decimal(gray2binary(x))
  })

  orders <- mapply(function(x) {
    min(orders[x], nlevels[[x]])
  }, 1:length(orders))

  orders <- lapply(
    function(x) {
      switch(datatype[[x]],
        "discrete" = levels[[x]][which.min(abs((1:length(levels[[x]])) - orders[[x]]))],
        "integer" = floor(seq(min[[x]], max[[x]],
          by = if (nlevels[[x]] > 0) {
            (max[[x]] - min[[x]]) / (nlevels[[x]])
          } else {
            1
          }
        )[orders[[x]] + 1]),
        "float" = seq(min[[x]], max[[x]],
          by = if (nlevels[[x]] > 0) {
            (max[[x]] - min[[x]]) / (nlevels[[x]])
          } else {
            1
          }
        )[orders[[x]] + 1]
      )
    },
    X = 1:length(orders)
  )
  return(setNames(orders, nm = names(features)))
}

#' Encode value to binary representation
#'
#' Encode value to binary representation
#'
#' @param values <list> List of values to encode
#' @param features <list> List of features to encode
#' Feature description
#' feature = list(
#'    datatype = <type of data> (discrete | integer | float)
#'    min = <min value>,
#'    max = <max value>,
#'    nlevels = <number of levels>,
#'    levels = <levels> (optional: discrete type specific)
#' )
#' @return <list> of encoded values

decodeBinFromValue <- function(values, features) {
  datatype <- getAttribute(features, "datatype")
  nlevels <- getAttribute(features, "nlevels")
  levels <- getAttribute(features, "levels")
  min <- getAttribute(features, "min")
  max <- getAttribute(features, "max")

  values <- mapply(
    function(x) {
      switch(datatype[[x]],
        "discrete" = which(levels[[x]] %in% values[[x]]) - 1,
        "integer" = which.min(abs(seq(min[[x]], max[[x]],
          by = (max[[x]] - min[[x]]) / (nlevels[[x]])
        ) - values[[x]])) - 1,
        "float" = which.min(abs(seq(min[[x]], max[[x]],
          by = (max[[x]] - min[[x]]) / (nlevels[[x]])
        ) - values[[x]])) - 1
      )
    },
    1:length(values)
  )

  bitOrders <- mapply(function(x) {
    nchar(toBin(x))
  }, nlevels)
  binary <- unlist(c(sapply(1:length(values), FUN = function(x) {
    binary2gray(decimal2binary(values[[x]], bitOrders[[x]]))
  })))

  return(binary)
}

#' Genetic algorithm core monitor
#'
#' Callback function called during the genetic algorithm
#' execution in order to monitor optimization evolution
#'
#' @param object <ga object> GA optimization object

gaMonitor2 <- function(object, digits = getOption("digits"), ...) {
  fitness <- na.exclude(object@fitness)
  cat(paste(
    "GA | Iter =", object@iter,
    " | Mean =", format(mean(fitness, na.rm = T), digits = digits),
    " | Best =", format(max(fitness, na.rm = T), digits = digits), "\n"
  ))
  flush.console()
}


#' Optimization function on binary representations of decision variables
#'
#' Optimization function on binary representations of decision variables
#' Maximization of a fitness function using genetic algorithms (GAs).
#' Using default binary representation to encode float and integer values
#  interval.
#'
#' @param opt_criteria <string> Fitness function criteria (minimise | maximise)
#' @param opt_function <function> Fitness function
#' @param features <list> List of features as decision variables
#' Feature description
#' feature = list(
#'    datatype = <type of data> (discrete | integer | float)
#'    min = <min value>,
#'    max = <max value>,
#'    nlevels = <number of levels>,
#'    levels = <levels> (optional: discrete type specific)
#' )
#' @param suggestions <list> A matrix of solutions strings to be
#' included in the initial population. If provided the number of
#' columns must match the number of decision variables
#' @param selection <function> An R function performing selection,
#' i.e. a function which generates a new population of individuals from
#' the current population probabilistically according to individual
#' fitness
#' @param keepBest <boolean> A logical argument specifying if best
#' solutions at each iteration should be saved in a slot called bestSol
#' @param popSize <integer> The population size
#' @param maxiter <integer> The maximum number of iterations to run
#' before the GA search is halted.
#' @param monitor <function> A logical or an R function which takes
#' as input the current state of the ga-class object and show the
#' evolution of the search
#' @param parallel <integer> An optional argument which allows to
#' specify if the Genetic Algorithm should be run sequentially or in
#' parallel
#' @param elitism <integer> The number of best fitness individuals
#' to survive at each generation
#' @param pmutation <float> The probability of mutation in a parent
#' chromosome
#' @param ... Additional arguments to be passed to the fitness function.
#' This allows to write fitness functions that keep some variables fixed
#' during the search. Supported GA parameters
#'     popSize (default 50). The population size
#'     pcrossover (default 0.8). The probability of crossover between
#'         pairs of chromosomes
#'     pmutation (default 0.1). The probability of mutation in a parent
#'         chromosome. Usually mutation occurs
#'     elitism. The number of best fitness individuals to survive at each
#'         generation. By default the top 5% individuals will survive at
#'         each iteration.
#'     maxiter (default 100). The maximum number of iterations to run
#'         before the DE search is halted
#'     stepsize.T he stepsize or weighting factor
#' @return <list> of optim solution for each decision variable

optimize <- function(opt_criteria, opt_function, features, suggestions = NULL,
                     keepBest = TRUE, parallel = FALSE, monitor = TRUE, ...) {

  minimise <- function(X, features, ...) {
    return(-opt_function(decodeValueFromBin(X, features), ...))
  }
  maximise <- function(X, features, ...) {
    return(opt_function(decodeValueFromBin(X, features), ...))
  }

  fitness <- minimise
  if (opt_criteria == "maximise") fitness <- maximise

  if (!(is.null(suggestions))) {
      suggestions <- decodeBinFromValue(
        values = suggestions,
        features = features
      )
  }

  if (is.null(parallel)) parallel <- detectCores() - 2
  if (monitor == FALSE) monitor <- interactive()

  opt_results <- suppressMessages(
    ga(
      type = "binary",
      fitness = fitness,
      nBits = sum(mapply(
        function(x) {
          nchar(toBin(x))
        },
        unlist(getAttribute(features, "nlevels"))
      )),
      features = features,
      opt_function = opt_function,
      suggestions = suggestions,
      monitor = if (monitor == TRUE) gaMonitor2 else FALSE,
      selection = gabin_tourSelection,
      keepBest = keepBest,
      parallel = parallel,
      ...
    )
  )
  return(decodeValueFromBin(opt_results@solution[1, ], features))
}
#' Hyperparameters tuning 
#'
#' Hyperparameter tuning via model fitting optimization. See optimize
#' documentation for more details
hyperparameters_tuning <- optimize


###
### Reformulation of Caret package functions ----
###

train.formula <- function (form, data, weights, subset, na.action = na.fail, 
                           contrasts = NULL,...) 
{
  m <- match.call(expand.dots = FALSE)
  
  # Add intercept if needed
  if (!("intercept" %in% colnames(data))){
    data$intercept <- 1
  }
  
  # Add features that are not directly specified in data, but are defined during
  # the transformation process
  transformationSentences <- eval(m$...$transformationSentences,envir = parent.frame())
    #eval(m$...$transformationSentences)
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
  if("weatherDependenceByCluster" %in% names(m$...)){
    weatherDependenceByCluster <- eval(m$...$weatherDependenceByCluster, envir = parent.frame())
  } else {
    weatherDependenceByCluster <- NULL
  }
  
  if("clusteringResults" %in% names(m$...)){
    clusteringResults <- eval(m$...$clusteringResults, envir = parent.frame())
  } else {
    clusteringResults <- NULL
  }
  
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
  res <- train(x, y, weights = w, formulaTerms=Terms,...)
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
  # if ("outputName" %in% names(object$finalModel$meta))
  #   newdata_ini <- newdata_ini[,!(colnames(newdata_ini) %in% c(object$finalModel$meta$outputName))]
  if(sum(!(colnames(newdata) %in% colnames(newdata_ini))) > 0){
    newdata <- cbind(
      if(sum(!(colnames(newdata) %in% colnames(newdata_ini)))==1){
        setNames(
          data.frame(newdata[,!(colnames(newdata) %in% colnames(newdata_ini))]),
          nm = colnames(newdata)[!(colnames(newdata) %in% colnames(newdata_ini))]
        )
      } else {
        newdata[,!(colnames(newdata) %in% colnames(newdata_ini))]
      },
      newdata_ini
    )
  }
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
