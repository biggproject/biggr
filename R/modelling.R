###
### Model candidates auxiliar functions ----
###

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

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
  shifted_importance <- c(0,dplyr::lag(importance,1)[is.finite(dplyr::lag(importance,1))])
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


#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

regression_metrics <- function(data, lev=NULL, model=NULL){
  c(
    CVRMSE = -(RMSE(data$obs,data$pred,na.rm = T)/mean(data$obs,na.rm=T)),
    RMSE = -RMSE(data$obs,data$pred,na.rm = T),
    R2 = cor(data$obs,data$pred,use = "na.or.complete")^2
  ) 
}


#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

AR_term <- function(features,orders,suffix=NULL){
  paste(
    mapply(features,FUN=function(feat){
      if(grepl(":",feat)){
        #feat=c("h_sin_1:s1","h_sin_2:s1")
        #orders=2
        elements <- mapply(function(l){
          if(l==0){
            paste(paste0("`",strsplit(feat,":")[[1]],"`"),collapse=":")
          } else {
            paste(paste0("`",strsplit(feat,":")[[1]],"_l",l,"`"),collapse=":")
          }
        },orders)
        if(!is.null(suffix)){
          elements <- paste0(elements,suffix)
        }
        paste(elements, collapse=" + ")
      } else {
        elements <- mapply(function(l)
          if(l==0){
            paste0("`",feat,"`")
          } else {
            paste0("`",feat,"_l",l,"`")
          }, orders)
        if(!is.null(suffix)){
          elements <- paste0(elements,suffix)
        }
        paste(elements, collapse=" + ")
      }
    }),
    collapse=" + "
  )
}


#' Check the ratio of the periodogram out of the boundaries
#'
#' @param ts <vector> A univariate time series of values, usually the training residuals of a model
#' @param taper <numeric> A proportion tapered in forming the periodogram.
#' @return The proportion of the periodogram out of boundaries.

relative_out_of_boundaries_in_periodogram <- function(ts, taper = 0.1){
  
  if (NCOL(ts) > 1) 
    stop("only implemented for univariate time series")
  x <- as.vector(ts)
  x <- x[!is.na(x)]
  x <- spec.taper(scale(x, TRUE, FALSE), p = taper)
  y <- Mod(fft(x))^2/length(x)
  y[1L] <- 0
  n <- length(x)
  x <- (0:(n/2)) * frequency(ts)/n
  if (length(x)%%2 == 0) {
    n <- length(x) - 1
    y <- y[1L:n]
    x <- x[1L:n]
  } else { 
    y <- y[seq_along(x)]
  }
  xm <- frequency(ts)/2
  mp <- length(x) - 1
  crit <- 1.358/(sqrt(mp) + 0.12 + 0.11/sqrt(mp))
  oldpty <- par(pty = "s")
  on.exit(par(oldpty))
  
  return(
    sum(((cumsum(y)/sum(y))-x*1/max(x)) > crit | ((cumsum(y)/sum(y))-x*1/max(x)) < -crit)/length(x)
  )
  # ggplot() +
  #   geom_point(aes(x,(cumsum(y)/sum(y))-x*1/max(x))) +
  #   geom_hline(aes(yintercept=crit),col="blue") +
  #   geom_hline(aes(yintercept=-crit),col="blue")
}

###
### Model candidates definition ----
###

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

PenalisedLM <- function(input_parameters=NULL){
  input_parameters_ini <- input_parameters
  if(is.null(input_parameters)){
    input_parameters <- data.frame(
      parameter = "parameter",
      class = "character")
  }
  input_parameters$label <- input_parameters$parameter
  modelFramework <- list(
    label = "penalisedRegression",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = if(is.null(input_parameters_ini)){
      function(x, y, len = NULL, search = "grid") {
        data.frame(parameter = "none")
      }
    } else {
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
      }
    },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, forcePositiveTerms=NULL, logOutput=F,
                   trainMask=NULL, numericStatusVariable=NULL, characterStatusVariable=NULL,
                   maxPredictionValue=NULL, minPredictionValue=NULL,
                   weatherDependenceByCluster=NULL, clusteringResults=NULL, 
                   ...) {

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
      if(!is.null(trainMask)){
        data <- data[trainMask,]
      }
      
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

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

ARX <- function(input_parameters=NULL){
  input_parameters_ini <- input_parameters
  if(is.null(input_parameters)){
    input_parameters <- data.frame(
      parameter = "parameter",
      class = "character")
  }
  input_parameters$label <- input_parameters$parameter
  modelFramework <- list(
    label = "ARX",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = if(is.null(input_parameters_ini)){
      function(x, y, len = NULL, search = "grid") {
        data.frame(parameter = "none")
      }
    } else {
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
      }
    },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, logOutput=T, trainMask=NULL,
                   numericStatusVariable=NULL, characterStatusVariable=NULL,
                   maxPredictionValue=NULL, minPredictionValue=NULL,
                   weatherDependenceByCluster=NULL, clusteringResults=NULL, 
                   ...) {

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
      if(logOutput) {
        boxCoxLambda <- opt_boxcox_lambda(y+0.001)
        data[,outputName] <- boxcox_transformation(y+0.001, boxCoxLambda)
      } else {data[,outputName] <- y}
      
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
                paste(if (!is.null(numericStatusVariable)){
                        paste0("0 + as.factor(as.character(",numericStatusVariable,"))")
                      } else if (!is.null(characterStatusVariable)) {
                        "0"#paste0("0 + ",factorStatusVariable)
                      } else {
                        "0"
                      },
                      paste0(do.call(
                        c,
                        lapply(
                          features,
                          function(f){
                            f_ <- f
                            if(f %in% names(transformationItems)){
                              f <- transformationItems[[f]]$formula
                            }
                            AR_term(features = f, 
                                    if(!(paste0("AR_",f_) %in% colnames(param))){
                                      0
                                    } else if(f[1]==outputName){
                                      1:param[,paste0("AR_",f_)]
                                    } else {
                                      0:param[,paste0("AR_",f_)]
                                    },
                                    suffix = if(!is.null(numericStatusVariable)){
                                        paste0(":",numericStatusVariable)
                                      } else if (!is.null(characterStatusVariable)) {
                                        paste0(":",characterStatusVariable)
                                      } else {
                                        NULL
                                      })
                          }
                        )
                      ),
                      collapse="+"),
                      sep=" + ")
        )
      )
      
      # Train the model
      if(!is.null(trainMask)){
        data <- data[trainMask,]
      }
      mod <- lm(
        formula=ARX_form, data=data 
      )
      
      # Store the meta variables
      mod$meta <- list(
        formula = ARX_form,
        features = features,
        outputName = outputName,
        logOutput = logOutput,
        maxPredictionValue = maxPredictionValue,
        minPredictionValue = minPredictionValue,
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
        clusteringResults = clusteringResults,
        boxCoxLambda = if(exists("boxCoxLambda")){boxCoxLambda}else{NULL} 
      )
      mod
      
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL,
                       forceInitOutputFeatures=NULL, forceOneStepPrediction=F, predictionIntervals=F) {

      newdata <- as.data.frame(newdata)
      features <- modelFit$meta$features[
        !(modelFit$meta$features %in% modelFit$meta$outputName)]
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      logOutput <- modelFit$meta$logOutput
      outputName <- modelFit$meta$outputName
      maxPredictionValue <- modelFit$meta$maxPredictionValue
      minPredictionValue <- modelFit$meta$minPredictionValue
      weatherDependenceByCluster <- modelFit$meta$weatherDependenceByCluster
      clusteringResults <- modelFit$meta$clusteringResults
      boxCoxLambda <- modelFit$meta$boxCoxLambda
      
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
      
      newdata <- if(any(!(names(forceInitOutputFeatures) %in% colnames(newdata)))){
        cbind(newdata,do.call(cbind,lapply(FUN = function(i){
          rep(NA,nrow(newdata))
        }, names(forceInitOutputFeatures)[
          names(forceInitOutputFeatures) %in% colnames(newdata)])))
      } else {newdata}
      
      if(maxLag>0){
        newdata <- 
          rbind(
            setNames(data.frame(lapply(FUN = function(f){
                initItem <- if(f %in% names(forceInitInputFeatures)) {
                  forceInitInputFeatures[[f]]
                } else if(f %in% names(forceInitOutputFeatures)) {
                  forceInitOutputFeatures[[f]]
                } else {
                  rep(newdata[1,f],maxLag)
                }
                c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
              },unique(c(colnames(newdata),names(forceInitOutputFeatures))))),
              nm=unique(c(colnames(newdata),names(forceInitOutputFeatures))))[,colnames(newdata)],
            newdata)
      }
      # if(!is.null(forceInitInputFeatures)){
      #   factor_char_features <- names(forceInitInputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitInputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitInputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitInputFeatures <- c(forceInitInputFeatures, as.list(aux))
      #   }
      # }
      # if(!is.null(forceInitOutputFeatures)){
      #   factor_char_features <- names(forceInitOutputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitOutputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitOutputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitOutputFeatures <- c(forceInitOutputFeatures, as.list(aux))
      #   }
      # }
      
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
        for (f in names(forceGlobalInputFeatures)[
            names(forceGlobalInputFeatures) %in% names(modelFit$meta$transformationSentences)]
            ){
          if(!(length(forceGlobalInputFeatures[[f]])==1 ||
               length(forceGlobalInputFeatures[[f]])==(nrow(newdata)+maxLag))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          if(length(forceGlobalInputFeatures[[f]])==1){
            newdata[,f] <- c(rep(NA,maxLag),rep(forceGlobalInputFeatures[[f]],
                                                nrow(newdata)-maxLag))
          } else {
            newdata[,f] <- c(rep(NA,maxLag),forceGlobalInputFeatures[[f]])
          }
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = c(outputName,featuresAll)#modelFit$meta$features, 
                                # forceGlobalInputFeatures = forceGlobalInputFeatures,
                                # forceInitInputFeatures = forceInitInputFeatures,
                                # forceInitOutputFeatures = forceInitOutputFeatures
                                )
      newdata <- newdata[(maxLag+1):nrow(newdata),]
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(forceOneStepPrediction==F && paste("AR",outputName,sep="_") %in% colnames(param)){
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = outputName,
                                    predictionStep = i-1#,
                                    # forceInitInputFeatures = forceInitInputFeatures,
                                    # forceInitOutputFeatures = forceInitOutputFeatures
                                    )
          if(predictionIntervals){
            prediction_results <- as.data.frame(
              predict(object = modelFit, newdata = newdata[i,], interval = "prediction",
                      level = 0.93-0.07)) %>%
              rename("average"="fit")
            prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
              (qnorm(0.93)-qnorm(0.07))
            newdata[i,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
          } else {
            newdata[i,outputName] <- predict(modelFit,newdata[i,])
          }
        }
      } else {
        if(predictionIntervals){
          prediction_results <- as.data.frame(
            predict(object = modelFit, newdata = newdata, interval = "prediction",
                    level = 0.93-0.07)) %>%
            rename("average"="fit")
          prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
            (qnorm(0.93)-qnorm(0.07))
          newdata[,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
        } else {
          newdata[,outputName] <- predict(modelFit,newdata)
        }
      }
      result <- 
        if(logOutput) { 
          if(predictionIntervals){
            inverse_boxcox_transformation(newdata[,paste0(outputName,"_",colnames(prediction_results))],
                                          boxCoxLambda)-0.001
          } else {
            inverse_boxcox_transformation(newdata[,outputName],boxCoxLambda)-0.001
          }
        } else {
          if(predictionIntervals){
            newdata[,paste0(outputName,"_",colnames(prediction_results))]
          } else {
            newdata[,outputName]
          }
        }
      if (!is.null(maxPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
              ifelse(result[,r] > maxPredictionValue, maxPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result > maxPredictionValue, maxPredictionValue, result)
        }
      }
      if (!is.null(minPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
              ifelse(result[,r] < minPredictionValue, minPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result < minPredictionValue, minPredictionValue, result)
        }
      }
      result
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

RandomForest <- function(input_parameters=NULL){
  input_parameters_ini <- input_parameters
  if(is.null(input_parameters)){
    input_parameters <- data.frame(
      parameter = "parameter",
      class = "character")
  }
  input_parameters$label <- input_parameters$parameter
  modelFramework <- list(
    label = "RandomForest",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = if(is.null(input_parameters_ini)){
      function(x, y, len = NULL, search = "grid") {
        data.frame(parameter = "none")
      }
    } else {
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
      }
    },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, trainMask=NULL,
                   maxPredictionValue=NULL, minPredictionValue=NULL,
                   clusteringResults=NULL, 
                   ...) {
      
      # x <<- x
      # y <<- y
      # params <<- param
      # param <- params
      # formulaTerms <<- formulaTerms
      # transformationSentences <<- transformationSentences
      # maxPredictionValue <<- maxPredictionValue
      # minPredictionValue <<- minPredictionValue
      # clusteringResults <<- clusteringResults
      # trainMask <<- trainMask
      
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
                paste(
                  "0",
                  paste0(do.call(
                    c,
                    lapply(
                      features,
                      function(f){
                        f_ <- f
                        if(f %in% names(transformationItems)){
                          f <- transformationItems[[f]]$formula
                        }
                        AR_term(features = f, 
                                if(!(paste0("AR_",f_) %in% colnames(param))){
                                  0
                                } else if(f[1]==outputName){
                                  1:param[,paste0("AR_",f_)]
                                } else {
                                  0:param[,paste0("AR_",f_)]
                                },
                                suffix = NULL)
                      }
                    )
                  ),
                  collapse="+"),
                sep=" + ")
        )
      )
      
      # Train the model
      if(!is.null(trainMask)){
        data <- data[trainMask,]
      }
      
      data_matrix <- model.matrix(ARX_form,data[is.finite(data[,outputName]),])
      colnames(data_matrix) <- gsub(":","_",colnames(data_matrix))
      
      mod <- ranger(x = data_matrix, y=y[is.finite(y)], data = data, num.trees = 500,num.threads = 1,min.node.size = 6,
                    mtry = sqrt(ncol(data_matrix)), num.random.splits = 6,splitrule = 'extratrees')
      
      # Store the meta variables
      mod$meta <- list(
        formula = ARX_form,
        features = features,
        outputName = outputName,
        maxPredictionValue = maxPredictionValue,
        minPredictionValue = minPredictionValue,
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
        clusteringResults = clusteringResults
      )
      mod
      
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL,
                       forceInitOutputFeatures=NULL, forceOneStepPrediction=F, predictionIntervals=F) {
      
      # modelFit <<- modelFit
      # newdata <<- newdata
      
      newdata <- as.data.frame(newdata)
      features <- modelFit$meta$features[
        !(modelFit$meta$features %in% modelFit$meta$outputName)]
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      outputName <- modelFit$meta$outputName
      maxPredictionValue <- modelFit$meta$maxPredictionValue
      minPredictionValue <- modelFit$meta$minPredictionValue
      clusteringResults <- modelFit$meta$clusteringResults
      
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
      
      newdata <- if(any(!(names(forceInitOutputFeatures) %in% colnames(newdata)))){
        cbind(newdata,do.call(cbind,setNames(
          lapply(FUN = function(i){
            rep(NA,nrow(newdata))
            }, names(forceInitOutputFeatures)[!(names(forceInitOutputFeatures) %in% colnames(newdata))]),
          nm= names(forceInitOutputFeatures)[!(names(forceInitOutputFeatures) %in% colnames(newdata))]
          )))
      } else {newdata}
      
      if(maxLag>0){
        newdata <- 
          rbind(
            setNames(data.frame(lapply(FUN = function(f){
              initItem <- if(f %in% names(forceInitInputFeatures)) {
                forceInitInputFeatures[[f]]
              } else if(f %in% names(forceInitOutputFeatures)) {
                forceInitOutputFeatures[[f]]
              } else {
                rep(newdata[1,f],maxLag)
              }
              c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
            },unique(c(colnames(newdata),names(forceInitOutputFeatures))))),
            nm=unique(c(colnames(newdata),names(forceInitOutputFeatures))))[,colnames(newdata)],
            newdata)
      }
      # if(!is.null(forceInitInputFeatures)){
      #   factor_char_features <- names(forceInitInputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitInputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitInputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitInputFeatures <- c(forceInitInputFeatures, as.list(aux))
      #   }
      # }
      # if(!is.null(forceInitOutputFeatures)){
      #   factor_char_features <- names(forceInitOutputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitOutputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitOutputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitOutputFeatures <- c(forceInitOutputFeatures, as.list(aux))
      #   }
      # }
      
      # Transform input data if it is needed
      transformation <- data_transformation_wrapper(
        data=newdata, features=features, transformationSentences = transformationSentences, 
        transformationResults = transformationResults, param = param, 
        clusteringResults = clusteringResults)
      newdata <- transformation$data
      features <- transformation$features
      featuresAll <- transformation$featuresAll
      
      # Change the transformed inputs if are specified in forceGlobalInputFeatures
      if (!is.null(forceGlobalInputFeatures)){
        for (f in names(forceGlobalInputFeatures)[
          names(forceGlobalInputFeatures) %in% names(modelFit$meta$transformationSentences)]
        ){
          if(!(length(forceGlobalInputFeatures[[f]])==1 ||
               length(forceGlobalInputFeatures[[f]])==(nrow(newdata)+maxLag))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          if(length(forceGlobalInputFeatures[[f]])==1){
            newdata[,f] <- c(rep(NA,maxLag),rep(forceGlobalInputFeatures[[f]],
                                                nrow(newdata)-maxLag))
          } else {
            newdata[,f] <- c(rep(NA,maxLag),forceGlobalInputFeatures[[f]])
          }
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = c(outputName,featuresAll)#modelFit$meta$features, 
                                # forceGlobalInputFeatures = forceGlobalInputFeatures,
                                # forceInitInputFeatures = forceInitInputFeatures,
                                # forceInitOutputFeatures = forceInitOutputFeatures
      )
      newdata <- newdata[(maxLag+1):nrow(newdata),]
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(forceOneStepPrediction==F && paste("AR",outputName,sep="_") %in% colnames(param)){
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = outputName,
                                    predictionStep = i-1#,
                                    # forceInitInputFeatures = forceInitInputFeatures,
                                    # forceInitOutputFeatures = forceInitOutputFeatures
          )
          if(predictionIntervals){
            # prediction_results <- as.data.frame(
            #   predict(object = modelFit, newdata = newdata[i,], interval = "prediction",
            #           level = 0.93-0.07)) %>%
            #   rename("average"="fit")
            # prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
            #   (qnorm(0.93)-qnorm(0.07))
            # newdata[i,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
          } else {
            newdata[i,outputName] <- predict(modelFit,newdata[i,])$predictions
          }
        }
      } else {
        if(predictionIntervals){
          # prediction_results <- as.data.frame(
          #   predict(object = modelFit, newdata = newdata, interval = "prediction",
          #           level = 0.93-0.07)) %>%
          #   rename("average"="fit")
          # prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
          #   (qnorm(0.93)-qnorm(0.07))
          # newdata[,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
        } else {
          newdata[,outputName] <- predict(modelFit,newdata)$predictions
        }
      }
      result <- 
          if(predictionIntervals){
            newdata[,paste0(outputName,"_",colnames(prediction_results))]
          } else {
            newdata[,outputName]
          }
      if (!is.null(maxPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
            ifelse(result[,r] > maxPredictionValue, maxPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result > maxPredictionValue, maxPredictionValue, result)
        }
      }
      if (!is.null(minPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
            ifelse(result[,r] < minPredictionValue, minPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result < minPredictionValue, minPredictionValue, result)
        }
      }
      result
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

#' Generalised Linear Model
#' 
#' This function is a custom model wrapper for caret R-package to train 
#' and predict Generalised Linear Models.
#'
#' @param formula -arg for train()- <formula> providing the model output feature
#' and the model input features. Inputs can be columns defined in data argument
#' and/or features described in transformationSentences argument.
#' @param data -arg for train()- <data.frame> containing the output feature and 
#' all the raw input features used to train the model.
#' @param trainMask -agr for train()- <array> providing the mask 
#' (TRUE if yes, FALSE if no) for the dataset used for model training.
#' This mask will be considered after all transformation procedures.
#' The array must be the same length as the number of rows in the data argument. 
#' @param numericStatusVariable -arg for train()- <string> defining the name of 
#' the column in data that contains the numerical status information 
#' (normally 0 and 1) of the output variable.
#' If it is not NULL (default), all model coefficients are fitted considering 
#' as model input the transformed input values multiplied by this numeric 
#' status variable.
#' @param characterStatusVariable -arg for train()- <string> defining the name of 
#' the column in data that contains the discrete status information of the 
#' output variable.
#' If it is not NULL (default), all model coefficients are fitted considering 
#' each one of the possible status defined in the character status variable.
#' @param transformationSentences -arg for train()- <list>. Run 
#' ?data_transformation_wrapper() for details.
#' @param familyGLM -arg for train()- <function> indicating the link function 
#' to be used in the model. For GLM this can be a character string naming a 
#' family function, a family function or the result of a call to a family 
#' function. Execute ?stats::family for details.
#' @param continuousTime -arg for train()- <boolean> indicating if the 
#' fitting process of the model coefficients should account for the 
#' data gaps. Set to 
#' @param maxPredictionValue -arg for train()- <float> defining 
#' the maximum value of predictions.
#' @param minPredictionValue -arg for train()- <float> defining 
#' the minimum value of predictions.
#' @param weatherDependenceByCluster -arg for train()- <data.frame>
#' containing the columns 's', 'heating', 'cooling', 'tbalh', 'tbalc';
#' corresponding to the daily load curve cluster, the heating dependence 
#' (TRUE or FALSE), the cooling dependance (TRUE or FALSE), the balance 
#' heating temperature, and the balance cooling temperature, respectively.
#' @param clusteringResults -arg for train()- <list> from the 
#' output produced by clustering_dlc().
#' @param newdata -arg for biggr::predict.train()- <data.frame> containing
#' the input data to consider in a model prediction.
#' @param forceGlobalInputFeatures -arg for biggr::predict.train()- <list> 
#' containing the input model features to overwrite in newdata. 
#' Each input feature must have length 1, or equal to the newdata's 
#' number of rows.
#' @param forceInitInputFeatures -arg for biggr::predict.train()- <list>
#' containing the last timesteps of the input features.
#' @param forceInitOutputFeatures -arg for biggr::predict.train()- <list>
#' containing the last timesteps of the output feature.
#' @param forceOneStepPrediction -arg for biggr::predict.train()- 
#' <boolean> indicating if the prediction mode should be done in one step 
#' prediction mode. 
#' @param predictionIntervals -arg for biggr::predict.train()- 
#' <boolean> describing if the prediction should be of the average value 
#' or the prediction interval.
#' 
#' @examples 
#' # It should be launched using the train() function for training, and 
#' # biggr::predict.train() function for predicting. 
#' # An example for model training is:
#' train(
#'  formula = Qe ~ daily_seasonality,
#'  data = df, # data.frame with three columns: 
#'             #  'time','Qe', and 'hour'; 
#'             # corresponding to time, electricity consumption, 
#'             # and hour of the day. 
#'             # 200 rows of data are needed considering 
#'             # the training control strategy that was selected 
#'             # in argument trControl. 
#'  method = GLM(
#'    data.frame(parameter = "nhar",
#'               class = "discrete")
#'  ),
#'  tuneGrid = data.frame("nhar"=4:6),
#'  trControl = trainControl(method="timeslice", initialWindow = 100,
#'                           horizon = 10, skip = 10, fixedWindow = T),
#'  minPredictionValue = 0,
#'  maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
#'  familyGLM = quasipoisson(),
#'  transformationSentences = list(
#'     "daily_seasonality" = c(
#'         "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
#'         "weekday")
#'    )
#'  )
#'  # An example for model prediction is:
#'  predictor <- crate(function(x, forceGlobalInputFeatures = NULL, predictionIntervals=F){
#'    biggr::predict.train(
#'      object = !!mod,
#'      newdata = x,
#'      forceGlobalInputFeatures = forceGlobalInputFeatures,
#'      predictionIntervals = predictionIntervals
#'    )
#'  })
#'  # An example call of the predictor function to predict Qe at certain time is:
#'  predictor(
#'      data.frame(
#'          time=as.POSIXct("2020-01-01 14:00:00",tz="UTC"),
#'          hour=15
#'      )
#'  )
#'  # An additional nice feature of predictors is that this object 
#'  # can be directly stored to MLFlow infrastructure using:
#'  mlflow_log_model(predictor,"example_name")
#'  # Last instance can only be executed if an MLFlow run was started, see:
#'  ?mlflow::mlflow_start_run()
#'  
#' @return When training: <list> containing the model, when predicting: <array> of the predicted results.

GLM <- function(input_parameters=NULL){
  input_parameters_ini <- input_parameters
  if(is.null(input_parameters)){
    input_parameters <- data.frame(
      parameter = "parameter",
      class = "character")
  }
  input_parameters$label <- input_parameters$parameter
  modelFramework <- list(
    label = "GLM",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = if(is.null(input_parameters_ini)){
      function(x, y, len = NULL, search = "grid") {
        data.frame(parameter = "none")
      }
    } else {
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
      }
    },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, familyGLM,
                   transformationSentences=NULL, trainMask=NULL,
                   numericStatusVariable=NULL, characterStatusVariable=NULL,
                   maxPredictionValue=NULL, minPredictionValue=NULL,
                   weatherDependenceByCluster=NULL, clusteringResults=NULL,
                   ...) {
      
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
                paste(if (!is.null(numericStatusVariable)){
                  paste0("0 + as.factor(as.character(",numericStatusVariable,"))")
                } else if (!is.null(characterStatusVariable)) {
                  "0"#paste0("0 + ",factorStatusVariable)
                } else {
                  "0"
                },
                paste0(do.call(
                  c,
                  lapply(
                    features,
                    function(f){
                      f_ <- f
                      if(f %in% names(transformationItems)){
                        f <- transformationItems[[f]]$formula
                      }
                      AR_term(features = f, 
                              if(!(paste0("AR_",f_) %in% colnames(param))){
                                0
                              } else if(f[1]==outputName){
                                1:param[,paste0("AR_",f_)]
                              } else {
                                0:param[,paste0("AR_",f_)]
                              },
                              suffix = if(!is.null(numericStatusVariable)){
                                paste0(":",numericStatusVariable)
                              } else if (!is.null(characterStatusVariable)) {
                                paste0(":",characterStatusVariable)
                              } else {
                                NULL
                              })
                    }
                  )
                ),
                collapse="+"),
                sep=" + ")
        )
      )
      
      # Train the model
      if(!is.null(trainMask)){
        data <- data[trainMask,]
      }
      mod <- glm(
        formula=ARX_form, data=data, family=familyGLM
      )
      
      # Store the meta variables
      mod$meta <- list(
        formula = ARX_form,
        features = features,
        outputName = outputName,
        maxPredictionValue = maxPredictionValue,
        minPredictionValue = minPredictionValue,
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
      
    },
    predict = function(modelFit, newdata, submodels, forceGlobalInputFeatures=NULL, forceInitInputFeatures=NULL,
                       forceInitOutputFeatures=NULL, forceOneStepPrediction=F, predictionIntervals=F) {
      
      newdata <- as.data.frame(newdata)
      features <- modelFit$meta$features[
        !(modelFit$meta$features %in% modelFit$meta$outputName)]
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      outputName <- modelFit$meta$outputName
      maxPredictionValue <- modelFit$meta$maxPredictionValue
      minPredictionValue <- modelFit$meta$minPredictionValue
      weatherDependenceByCluster <- modelFit$meta$weatherDependenceByCluster
      clusteringResults <- modelFit$meta$clusteringResults
      boxCoxLambda <- modelFit$meta$boxCoxLambda
      
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
      
      newdata <- if(any(!(names(forceInitOutputFeatures) %in% colnames(newdata)))){
        cbind(newdata,do.call(cbind,lapply(FUN = function(i){
          rep(NA,nrow(newdata))
        }, names(forceInitOutputFeatures)[
          names(forceInitOutputFeatures) %in% colnames(newdata)])))
      } else {newdata}
      
      if(maxLag>0){
        newdata <- 
          rbind(
            setNames(data.frame(lapply(FUN = function(f){
              initItem <- if(f %in% names(forceInitInputFeatures)) {
                forceInitInputFeatures[[f]]
              } else if(f %in% names(forceInitOutputFeatures)) {
                forceInitOutputFeatures[[f]]
              } else {
                rep(newdata[1,f],maxLag)
              }
              c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
            },unique(c(colnames(newdata),names(forceInitOutputFeatures))))),
            nm=unique(c(colnames(newdata),names(forceInitOutputFeatures))))[,colnames(newdata)],
            newdata)
      }
      # if(!is.null(forceInitInputFeatures)){
      #   factor_char_features <- names(forceInitInputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitInputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitInputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitInputFeatures <- c(forceInitInputFeatures, as.list(aux))
      #   }
      # }
      # if(!is.null(forceInitOutputFeatures)){
      #   factor_char_features <- names(forceInitOutputFeatures)[
      #     mapply(FUN=function(i)class(i),forceInitOutputFeatures) %in% c("factor","character")]
      #   for(fcf in factor_char_features){
      #     aux <- fastDummies::dummy_cols(as.factor(forceInitOutputFeatures[[fcf]]),remove_selected_columns = T)
      #     colnames(aux) <- gsub(".data",fcf,colnames(aux))
      #     forceInitOutputFeatures <- c(forceInitOutputFeatures, as.list(aux))
      #   }
      # }
      
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
        for (f in names(forceGlobalInputFeatures)[
          names(forceGlobalInputFeatures) %in% names(modelFit$meta$transformationSentences)]
        ){
          if(!(length(forceGlobalInputFeatures[[f]])==1 ||
               length(forceGlobalInputFeatures[[f]])==(nrow(newdata)+maxLag))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          if(length(forceGlobalInputFeatures[[f]])==1){
            newdata[,f] <- c(rep(NA,maxLag),rep(forceGlobalInputFeatures[[f]],
                                                nrow(newdata)-maxLag))
          } else {
            newdata[,f] <- c(rep(NA,maxLag),forceGlobalInputFeatures[[f]])
          }
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = c(outputName,featuresAll)#modelFit$meta$features, 
                                # forceGlobalInputFeatures = forceGlobalInputFeatures,
                                # forceInitInputFeatures = forceInitInputFeatures,
                                # forceInitOutputFeatures = forceInitOutputFeatures
      )
      newdata <- newdata[(maxLag+1):nrow(newdata),]
      
      # Predict at multi-step ahead or one-step ahead prediction, 
      # depending if some AR input is considered using the output variable
      if(forceOneStepPrediction==F && paste("AR",outputName,sep="_") %in% colnames(param)){
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = outputName,
                                    predictionStep = i-1#,
                                    # forceInitInputFeatures = forceInitInputFeatures,
                                    # forceInitOutputFeatures = forceInitOutputFeatures
          )
          if(predictionIntervals){
            prediction_results <- as.data.frame(
              predict(object = modelFit, newdata = newdata[i,], interval = "prediction",
                      level = 0.93-0.07)) %>%
              rename("average"="fit")
            prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
              (qnorm(0.93)-qnorm(0.07))
            newdata[i,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
          } else {
            newdata[i,outputName] <- predict(modelFit,newdata[i,],type="response")
          }
        }
      } else {
        if(predictionIntervals){
          prediction_results <- as.data.frame(
            predict(object = modelFit, newdata = newdata, interval = "prediction",
                    level = 0.93-0.07)) %>%
            rename("average"="fit")
          prediction_results$sigma <- (prediction_results$upr - prediction_results$lwr)/
            (qnorm(0.93)-qnorm(0.07))
          newdata[,paste0(outputName,"_",colnames(prediction_results))] <- prediction_results
        } else {
          newdata[,outputName] <- predict(modelFit,newdata,type="response")
        }
      }
      result <- if(predictionIntervals){
            newdata[,paste0(outputName,"_",colnames(prediction_results))]
          } else {
            newdata[,outputName]
          }
      if (!is.null(maxPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
            ifelse(result[,r] > maxPredictionValue, maxPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result > maxPredictionValue, maxPredictionValue, result)
        }
      }
      if (!is.null(minPredictionValue)){
        if(predictionIntervals){
          mapply(function(r){
            ifelse(result[,r] < minPredictionValue, minPredictionValue, result[,r])},
            colnames(result))
        } else {
          ifelse(result < minPredictionValue, minPredictionValue, result)
        }
      }
      result
    },
    prob = NULL,
    varImp = NULL,
    predictors = function(x, ...) predictors(x$terms),
    levels = NULL,
    sort = function(x) x)
  
  return(modelFramework)
}

#' Recursive Least Square model
#' 
#' This function is a custom model wrapper for caret R-package to train 
#' and predict linear models fitted using the Recursive Least Square 
#' method. The model coefficients of this kind of model are 
#' time-varying; thus, the relation between inputs and output changes 
#' over time to better fit the data. 
#'
#' @param formula -arg for train()- <formula> providing the model output feature
#' and the model input features. Inputs can be columns defined in data argument
#' and/or features described in transformationSentences argument.
#' @param data -arg for train()- <data.frame> containing the output feature and 
#' all the raw input features used to train the model.
#' @param transformationSentences -arg for train()- <list>. Run 
#' ?data_transformation_wrapper() for details.
#' @param logOutput -arg for train()- <boolean> indicating if a
#' Box-Jenkins transformation is considered in the output feature
#' during the training of the model. When predicting, 
#' it computes, automatically, the inverse transformation.
#' @param minMonthsTraining -arg for train()- <integer> indicating 
#' the minimum number of months for training.
#' @param continuousTime -arg for train()- <boolean> indicating if the 
#' fitting process of the model coefficients should account for the 
#' data gaps. Set to 
#' @param maxPredictionValue -arg for train()- <float> defining 
#' the maximum value of predictions.
#' @param minPredictionValue -arg for train()- <float> defining 
#' the minimum value of predictions.
#' @param weatherDependenceByCluster -arg for train()- <data.frame>
#' containing the columns 's', 'heating', 'cooling', 'tbalh', 'tbalc';
#' corresponding to the daily load curve cluster, the heating dependence 
#' (TRUE or FALSE), the cooling dependance (TRUE or FALSE), the balance 
#' heating temperature, and the balance cooling temperature, respectively.
#' @param clusteringResults -arg for train()- <list> from the 
#' output produced by clustering_dlc().
#' @param newdata -arg for biggr::predict.train()- <data.frame> containing
#' the input data to consider in a model prediction.
#' @param forceGlobalInputFeatures -arg for biggr::predict.train()- <list> 
#' containing the input model features to overwrite in newdata. 
#' Each input feature must have length 1, or equal to the newdata's 
#' number of rows.
#' @param forceInitInputFeatures -arg for biggr::predict.train()- <list>
#' containing the last timesteps of the input features.
#' @param forceInitOutputFeatures -arg for biggr::predict.train()- <list>
#' containing the last timesteps of the output feature.
#' @param forceOneStepPrediction -arg for biggr::predict.train()- 
#' <boolean> indicating if the prediction mode should be done in one step 
#' prediction mode. 
#' @param predictionHorizonInHours -arg for biggr::predict.train()- 
#' <array> considering the minimum and maximum horizon in hours for each 
#' prediction timestep. When forceOneStepPrediction is TRUE, this argument
#' is not used.
#' @param modelWindow -arg for biggr::predict.train()- <string> containing the 
#' window size considered in best model selection  (e.g. '%M-%Y', '%d-%M-%Y').
#' When forceOneStepPrediction is TRUE, this argument is not used.
#' @param modelSelection -arg for biggr::predict.train()- <string> defining 
#' the model selection mode for selecting the best model at every timeframe. 
#' Default: 'rmse' or 'random'. When forceOneStepPrediction is TRUE, 
#' this argument is not used.
#' 
#' @examples 
#' # It should be launched using the train() function for training, and 
#' # biggr::predict.train() function for predicting. 
#' # An example for model training is:
#' train(
#'  formula = Qe ~ daily_seasonality,
#'  data = df, # data.frame with three columns: 
#'             #  'time','Qe', and 'hour'; 
#'             # corresponding to time, electricity consumption, 
#'             # and hour of the day. 
#'             # 200 rows of data are needed considering 
#'             # the training control strategy that was selected 
#'             # in argument trControl. 
#'  method = RLS(
#'    data.frame(parameter = "nhar",
#'               class = "discrete")
#'  ),
#'  tuneGrid = data.frame("nhar"=4:6),
#'  trControl = trainControl(method="timeslice", initialWindow = 100,
#'                           horizon = 10, skip = 10, fixedWindow = T),
#'  minPredictionValue = 0,
#'  maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
#'  transformationSentences = list(
#'     "daily_seasonality" = c(
#'         "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
#'         "weekday")
#'    )
#'  )
#'  # An example for model prediction is:
#'  predictor <- crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours=1,
#'  modelWindow="%Y-%m-%d", modelSelection="rmse"){
#'    biggr::predict.train(
#'      object = !!mod,
#'      newdata = x,
#'      forceGlobalInputFeatures = forceGlobalInputFeatures,
#'      predictionHorizonInHours = predictionHorizonInHours,
#'      modelWindow = modelWindow,
#'      modelSelection = modelSelection
#'    )
#'  })
#'  # An example call of the predictor function to predict Qe at certain time is:
#'  predictor(
#'      data.frame(
#'          time=as.POSIXct("2020-01-01 14:00:00",tz="UTC"),
#'          hour=15
#'      )
#'  )
#'  # An additional nice feature of predictors is that this object 
#'  # can be directly stored to MLFlow infrastructure using:
#'  mlflow_log_model(predictor,"example_name")
#'  # Last instance can only be executed if an MLFlow run was started, see:
#'  ?mlflow::mlflow_start_run()
#'  
#' @return When training: <list> containing the model, when predicting: <array> of the predicted results.

RLS <- function(input_parameters=NULL){
  input_parameters_ini <- input_parameters
  if(is.null(input_parameters)){
    input_parameters <- data.frame(
      parameter = "parameter",
      class = "character")
  }
  input_parameters$label <- input_parameters$parameter
  modelFramework <- list(
    label = "RLS",
    library = NULL,
    type = "Regression",
    ## Define the ARX parameters
    parameters = input_parameters,
    grid = if(is.null(input_parameters_ini)){
      function(x, y, len = NULL, search = "grid") {
        data.frame(parameter = "none")
      }
    } else {
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
      }
    },
    loop = NULL,
    fit = function(x, y, wts, param, lev, last, classProbs, formulaTerms, 
                   transformationSentences=NULL, logOutput=F, 
                   minMonthsTraining=0, continuousTime=T,minPredictionValue=NULL,
                   maxPredictionValue=NULL, weatherDependenceByCluster=NULL,
                   clusteringResults=NULL,...
                   ) {
      # x <<- x
      # y <<- y
      # formulaTerms <<- formulaTerms
      # params <<- param
      # param <- params
      
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
      model <- onlineforecast::forecastmodel$new()
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
                                   min(data$localtime) + lubridate::hours(length(data$localtime)), 
                                   by=iso8601_period_to_text(detect_time_step(data$localtime),only_first = T))
        data_for_rls[["t"]] <- data_for_rls[["t"]][1:length(data_for_rls[[outputName]])] 
      } else {
        data_for_rls[["t"]] <- data$localtime
      }
      data_for_rls$scoreperiod <- sample(c(F,T),length(data_for_rls$t),replace = T,prob = c(0.9,0.1))
      
      # Fit the RLS model and obtain the time-varying coefficients
      mod_rls <- onlineforecast::rls_fit(c("lambda"=param$lambda),model, data_for_rls, scorefun = rmse, printout = F)
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
      mod_rls$data
      mod$yhat <- if(logOutput){exp(mod_rls$Yhat$k0)}else{mod_rls$Yhat$k0}
      mod$meta <- list(
        features = features,
        outputName = outputName,
        formula = ARX_form,
        logOutput = logOutput,
        minMonthsTraining = minMonthsTraining,
        maxPredictionValue = maxPredictionValue,
        minPredictionValue = minPredictionValue,
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
                       predictionHorizonInHours=0, modelMinMaxHorizonInHours=NULL, 
                       modelWindow="%Y-%m-%d", forceOneStepPrediction=F, modelSelection="rmse") {
      
      # Deprecated args
      if(!is.null(modelMinMaxHorizonInHours)){
        warning("modelMinMaxHorizonInHours is deprecated! Please, use predictionHorizonInHours argument instead.\n")
        if(predictionHorizonInHours==0){
          predictionHorizonInHours <- modelMinMaxHorizonInHours
        }
      }
      
      newdata <- as.data.frame(newdata)
      newdata$localtime <- lubridate::with_tz(newdata$time,
                                              lubridate::tz(modelFit$localtime))
      newdata <- newdata[order(newdata$localtime),]
      features <- modelFit$meta$features[
        !(modelFit$meta$features %in% modelFit$meta$outputName)]
      param <- modelFit$meta$param
      maxLag <- modelFit$meta$maxLag
      logOutput <- modelFit$meta$logOutput
      outputName <- modelFit$meta$outputName
      maxPredictionValue <- modelFit$meta$maxPredictionValue
      minPredictionValue <- modelFit$meta$minPredictionValue
      
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
      
      newdata <- if(any(!(names(forceInitOutputFeatures) %in% colnames(newdata)))){
        cbind(newdata,do.call(cbind,lapply(FUN = function(i){
          rep(NA,nrow(newdata))
        }, names(forceInitOutputFeatures)[
          names(forceInitOutputFeatures) %in% colnames(newdata)])))
      } else {newdata}
      
      if(maxLag>0){
        newdata <- 
          rbind(
            setNames(data.frame(lapply(FUN = function(f){
              initItem <- if(f %in% names(forceInitInputFeatures)) {
                forceInitInputFeatures[[f]]
              } else if(f %in% names(forceInitOutputFeatures)) {
                forceInitOutputFeatures[[f]]
              } else {
                rep(newdata[1,f],maxLag)
              }
              c(rep(initItem[1],max(0,maxLag-length(initItem))),tail(initItem,maxLag))
            },unique(c(colnames(newdata),names(forceInitOutputFeatures))))),
            nm=unique(c(colnames(newdata),names(forceInitOutputFeatures))))[,colnames(newdata)],
            newdata)
      }
      
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
        for (f in names(forceGlobalInputFeatures)[
          names(forceGlobalInputFeatures) %in% names(modelFit$meta$transformationSentences)]
        ){
          if(!(length(forceGlobalInputFeatures[[f]])==1 || 
               length(forceGlobalInputFeatures[[f]])==(nrow(newdata)+maxLag))){
            stop(sprintf("forceGlobalInputFeatures[[%s]] needs to have a length of 1 
                     or equal to the number of rows of newdata argument (%s).",f, nrow(newdata)))
          }
          if(length(forceGlobalInputFeatures[[f]])==1){
            newdata[,f] <- c(rep(NA,maxLag),rep(forceGlobalInputFeatures[[f]],
                                                nrow(newdata)-maxLag))
          } else {
            newdata[,f] <- c(rep(NA,maxLag),forceGlobalInputFeatures[[f]])
          }
        }
      }
      
      # Lag the components that has been initialised
      newdata <- lag_components(data = newdata, 
                                maxLag = maxLag, 
                                featuresNames = c(outputName,featuresAll)
                                )
      newdata <- newdata[(maxLag+1):nrow(newdata),]
      
      # Data transformation for RLS framework
      model_formula <- modelFit$meta$formula
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
      #mod_coef <- suppressMessages(pad(mod_coef, by="localtime"))
      mod_coef <- mod_coef[order(mod_coef$localtime),]
      mod_coef <- zoo::na.locf(mod_coef,fromLast = T,na.rm = F)
      
      ## Predict at multi-step ahead or one-step ahead prediction, 
      ## depending if some AR input is considered using the output variable
      
      predictionHorizonInHours_default = ifelse(length(predictionHorizonInHours)>1, 0, predictionHorizonInHours)
      mod_coef_aux <- mod_coef
      mod_coef_aux$localtime <- as.POSIXct(lubridate::with_tz(as.POSIXct(
        lubridate::with_tz(mod_coef_aux$localtime,"UTC") +
          lubridate::seconds(predictionHorizonInHours_default*3600)), tz=lubridate::tz(modelFit$localtime)))
      # Select the existent model closer to the horizon required
      if(!all(mod_coef_aux$localtime %in% mod_coef$localtime)){
        mod_coef_aux$localtime[!(mod_coef_aux$localtime %in% mod_coef$localtime)] <- as.POSIXct(
          mod_coef$localtime[
            mapply(mod_coef_aux$localtime[!(mod_coef_aux$localtime %in% mod_coef$localtime)], 
                   FUN= function(elem){ which.min(abs(as.numeric(elem - mod_coef$localtime)))}
            )]
        )
      }
      # Fill the gaps and get a data.frame with newdata dimensions
      mod_coef_aux <- mod_coef_aux[!duplicated(mod_coef_aux$localtime),]
      mod_coef_aux <- data.frame("localtime" = newdata$localtime) %>% left_join(mod_coef_aux, by="localtime")
      mod_coef_aux <- zoo::na.locf(mod_coef_aux,fromLast = T,na.rm = F)

      if(forceOneStepPrediction==F & (paste("AR",outputName,sep="_") %in% colnames(param))){
        # MULTIPLE STEPS (slower)
        for (i in 1:nrow(newdata)){
          newdata <- lag_components(data = newdata,
                                    maxLag = maxLag,
                                    featuresNames = featuresAll,
                                    predictionStep = i-1
                                    )
          newdata[i,outputName] <- sum(
            newdata_matrix[i,colnames(modelFit$coefficients)] * 
              mod_coef[mod_coef$localtime==newdata$localtime[i],colnames(modelFit$coefficients)]
          )
        }
      } else {
        # ONE STEP
        newdata[newdata$localtime %in% mod_coef_aux$localtime,outputName] <- 
          rowSums(
            newdata_matrix[newdata$localtime %in% mod_coef_aux$localtime,
                           colnames(modelFit$coefficients)] * 
              as.matrix(mod_coef_aux[mod_coef_aux$localtime %in% newdata$localtime,
                                 colnames(modelFit$coefficients)]),
            na.rm=T
          )
      }
      
      # return the output
      if(length(predictionHorizonInHours)==1){
        result <- 
          if (logOutput) {
            exp(newdata[,outputName])
          } else { 
            newdata[,outputName] 
          }
        result <- if (!is.null(maxPredictionValue)){
          ifelse(result > maxPredictionValue, maxPredictionValue, result)
        } else {result}
        result <- if (!is.null(minPredictionValue)){
          ifelse(result < minPredictionValue, minPredictionValue, result)
        } else {result}
        result
      } else {
        # When predicting a fixed horizon with each model coefficients set
        if(paste("AR",outputName,sep="_") %in% colnames(param)){
          stop("Horizons per step higher than 1 are not allowed when predicting multiple step ahead of ARX models")
        } else {
          mod_coef <- mod_coef[
            mod_coef$localtime >= min(newdata$localtime)-lubridate::hours(max(predictionHorizonInHours)) &
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
                   allowed_times <- newdata$localtime >= (mod_coef_aux[1,"localtime"]  + lubridate::hours(min(predictionHorizonInHours))) & 
                       newdata$localtime <= (mod_coef_aux[1,"localtime"] + lubridate::hours(max(predictionHorizonInHours)))
                   result <- setNames(
                     data.frame(
                      newdata$localtime[allowed_times],
                      if (logOutput) {
                        exp(
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
                   if(!is.null(minPredictionValue)){
                     result[,2] <- ifelse(result[,2] < minPredictionValue, 
                                          minPredictionValue, 
                                          result[,2])
                   }
                   result
                 })
          all_preds <- Reduce(
            function(x, y, ...) merge(x, y, all = TRUE, ...),
            multiple_preds
          )
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
          if(!is.null(minPredictionValue)){
            timePred[,"pred"] <- ifelse(timePred[,"pred"] < minPredictionValue, 
                                        minPredictionValue, 
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
        )[if(nlevels[[x]]>1){orders[[x]] + 1} else {1}]),
        "float" = seq(min[[x]], max[[x]],
          by = if (nlevels[[x]] > 0) {
            (max[[x]] - min[[x]]) / (nlevels[[x]])
          } else {
            1
          }
        )[if(nlevels[[x]]>1){orders[[x]] + 1} else {1}]
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

bee_uCrossover <- function(object, parents, nlevels_per_feature)
{
  parents <- object@population[parents,,drop = FALSE]
  u <- unlist(lapply(nlevels_per_feature,function(i)rep(runif(1),nchar(toBin(i)))))
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
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
      mutation = gabin_raMutation,
      crossover = purrr::partial(bee_uCrossover,
                          nlevels_per_feature = unlist(getAttribute(features, "nlevels"))),
      keepBest = keepBest,
      parallel = parallel,
      ...
    )
  )
  
  results <- decodeValueFromBin(opt_results@solution[1,], features)
  if(!is.finite(opt_results@fitnessValue)){
    results <- lapply(results,function(i)NA)
  }
  return(results)
}

#' Hyperparameters tuning 
#'
#' Hyperparameter tuning via model fitting optimization. See optimize
#' documentation for more details
hyperparameters_tuning <- optimize

###
### Misc functions ----
###

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

weather_dependence_disaggregator <- function(predictor, df, forceNoCooling, forceNoHeating, 
                                             forceNoCoolingAndHeating=NULL,...){
  
  # Forcing only heating and only cooling dependency
  baseload_and_cooling <- predictor( df, forceGlobalInputFeatures = forceNoHeating, ...)
  baseload_and_heating <- predictor( df, forceGlobalInputFeatures = forceNoCooling, ...)
  
  # Estimate the baseload consumption along the period
  if(is.null(forceNoCoolingAndHeating)) forceNoCoolingAndHeating <- c(forceNoCooling,forceNoHeating)
  baseload <- predictor( df, forceGlobalInputFeatures = forceNoCoolingAndHeating, ...)
  
  # Disaggregated predicted components and actual consumption
  disaggregated_df <- data.frame(
    "time"=df$time,
    "temperature"=df$temperature,
    "real"=df$Qe,
    "baseload"=baseload,
    "heating"=baseload_and_heating-baseload, 
    "cooling"=baseload_and_cooling-baseload
  )
  disaggregated_df$baseload <- disaggregated_df$baseload + 
    ifelse(disaggregated_df$heating>0,0,disaggregated_df$heating)
  disaggregated_df$baseload <- disaggregated_df$baseload + 
    ifelse(disaggregated_df$cooling>0,0,disaggregated_df$cooling)
  disaggregated_df$baseload <- ifelse(disaggregated_df$baseload<0,0,disaggregated_df$baseload)
  disaggregated_df$heating <- ifelse(disaggregated_df$heating<0,0,disaggregated_df$heating)
  disaggregated_df$cooling <- ifelse(disaggregated_df$cooling<0,0,disaggregated_df$cooling)
  disaggregated_df$predicted <- disaggregated_df$cooling + disaggregated_df$heating + disaggregated_df$baseload
  
  disaggregated_df$heatingSmooth <- #ifelse(disaggregated_df$heating>0,
    #       disaggregated_df$heating,0)
    rollmean(ifelse(disaggregated_df$heating>0,
                    disaggregated_df$heating,0), k = 3,
             align = "center", partial = T, fill = c(0,0,0))
  disaggregated_df$coolingSmooth <- #ifelse(disaggregated_df$cooling>0,
    #       disaggregated_df$cooling,0)
    rollmean(ifelse(disaggregated_df$cooling>0,
                    disaggregated_df$cooling,0), k = 3,
             align = "center", partial = T, fill = c(0,0,0))
  disaggregated_df$real_ <- ifelse(is.finite(disaggregated_df$real),disaggregated_df$real,
                                   disaggregated_df$predicted)
  disaggregated_df$baseloadR <- ifelse(disaggregated_df$baseload > disaggregated_df$real_,
                                       disaggregated_df$real_, disaggregated_df$baseload)
  disaggregated_df$heatingR <- ifelse(
    disaggregated_df$heatingSmooth > 0.1,
    ifelse(
      disaggregated_df$heatingSmooth > 0.1 & disaggregated_df$coolingSmooth > 0.1,
      # Heating and cooling at the same time
      (disaggregated_df$real_ - disaggregated_df$baseloadR) *
        (disaggregated_df$heatingSmooth/
           (disaggregated_df$heatingSmooth+disaggregated_df$coolingSmooth)),
      # Only heating
      disaggregated_df$real_ - disaggregated_df$baseloadR),
    0
  )
  disaggregated_df$coolingR <- ifelse(
    disaggregated_df$coolingSmooth > 0.1,
    disaggregated_df$real_ - (disaggregated_df$baseloadR + disaggregated_df$heatingR),
    0)
  disaggregated_df$baseloadR <- disaggregated_df$real_ - (disaggregated_df$coolingR + disaggregated_df$heatingR)
  
  disaggregated_df$real_ <- NULL
  
  return(disaggregated_df)
}


# Calculate the balance temperature
compute_tbal_using_model_predictions <- function(df, predictor, dep_vars, simplify=T,...){
  
  expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
  
  dep_vars_ini <- dep_vars
  if(simplify==T){
    for(dep_var in dep_vars_ini){
      if(length(unique(df[,dep_var]))>24){
        df[,paste0(dep_var,"_simplified")] <- 
          ave(df[,dep_var], cut(df[,dep_var], 12),
              FUN=function(x)floor(mean(x,na.rm=T)))
      } else { df[,paste0(dep_var,"_simplified")] <- df[,dep_var] }
    }
    dep_vars <- paste0(dep_vars_ini,"_simplified")
  }
  
  dep_vars_df <- df[,dep_vars]
  colnames(dep_vars_df)[colnames(dep_vars_df) %in% dep_vars] <-
    dep_vars_ini
  dep_vars_df <- dep_vars_df[!duplicated(dep_vars_df),]
  dep_vars_df <- data.frame(dep_vars_df, case=1:nrow(dep_vars_df))
  all_vars_df <- expand.grid.df(
    dep_vars_df,
    data.frame("temperature"=seq(ceiling(quantile(df$temperature,0.05,na.rm=T)),
                                 floor(quantile(df$temperature,0.95,na.rm=T)),by=1))
  )
  
  aux <- predictor(all_vars_df,
                   forceGlobalInputFeatures = list(temperature=all_vars_df$temperature,
                                                   temperatureLpf=all_vars_df$temperature), 
                   ...)
  all_vars_df$value <- aux
  
  for (i in unique(dep_vars_df$case)){
    df_case <- all_vars_df[all_vars_df$case==i,]
    # if(nrow(df_case)>14){
    #   df_case <- df_case[sample(1:nrow(df_case),size = 14,replace=F),]
    # }
    loess_mod <- as.data.frame(loess.smooth(df_case$temperature,df_case$value,degree=2))
    colnames(loess_mod) <- c("temperature","smoothed_value")
    loess_mod$smoothed_slope <- c(diff(loess_mod$smoothed_value),NA)
    if(sum(abs(loess_mod$smoothed_slope) < 0,na.rm=T) <= nrow(loess_mod)*0.8){
      loess_mod <- loess_mod[abs(loess_mod$smoothed_slope) <= quantile(abs(loess_mod$smoothed_slope),0.1,na.rm=T),]
      tbal <- loess_mod[which.min(loess_mod$smoothed_value),"temperature"]
      wdep <- T 
    } else {
      tbal <- NA
      wdep <- F
    }
    dep_vars_df$tbal[dep_vars_df$case==i] <- tbal
  }
  
  unique_cases <- dep_vars_df
  colnames(dep_vars_df)[colnames(dep_vars_df) %in% dep_vars_ini] <-
    dep_vars
  
  return(list(
    "df"=merge(df,dep_vars_df[,!(colnames(dep_vars_df) %in% c("case","value"))]),
    "uniqueCases"=unique_cases,
    "casesPredicted"=all_vars_df
  ))
}


# Simulate the prediction intervals based on the average estimated
#  value and standard deviation for each predicted timestep 
#  and aggregate them to a certain output frequency

#' Title
#' 
#' Description
#'
#' @param arg <> 
#' @return 

simulate_prediction_intervals <- function(predictionResults, outputFrequency, 
                                          timeColumn="time", funAggregation="SUM", 
                                          minPredictionValue=NULL, nSim=1000, estimate=F){
  
  results <- data.frame(
    "time"=rep(predictionResults[,timeColumn],each=nSim),
    "sim"=1:nSim,
    "value"=unlist(lapply(FUN=function(i){
      suppressWarnings(rnorm(nSim,predictionResults[i,"mean"],predictionResults[i,"sigma"]))},
      1:nrow(predictionResults))))
  if(!is.null(minPredictionValue)){
    results$value <- ifelse(results$value > minPredictionValue, results$value, minPredictionValue)
  }
  results <- results %>% {
    if(is.na(outputFrequency)){
      group_by(.,
               time = first(time), sim
      )
    } else {
      group_by(.,
               time = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                            week_start = getOption("lubridate.week.start", 1)), sim
      )
    }} %>% 
    summarise(
      value = if(all(is.na(value))){NA}else{
        if(funAggregation=="SUM"){
          if(estimate){
            mean(value,na.rm=T)*length(value)
          } else {
            sum(value,na.rm=T)
          }
        } else if(funAggregation=="MEAN"){
          mean(value,na.rm=T)
        }
      }
    ) %>% 
    ungroup() %>% {
      if(is.na(outputFrequency)){
        group_by(.,
                 time = first(time)
        )
      } else {
        group_by(.,
                 time = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                              week_start = getOption("lubridate.week.start", 1))
        )
      }} %>%
    summarise(
      "q2.5" = quantile(value,0.025,na.rm=T),
      "q5" = quantile(value,0.05,na.rm=T),
      "q10" = quantile(value,0.10,na.rm=T),
      "q25" = quantile(value,0.10,na.rm=T),
      "q50" = quantile(value,0.5,na.rm=T),
      "q90" = quantile(value,0.9,na.rm=T),
      "q95" = quantile(value,0.95,na.rm=T),
      "q97.5" = quantile(value,0.975,na.rm=T),
      "mean" = mean(value,na.rm=T),
      "sd" = sd(value,na.rm=T)
    ) %>% 
    ungroup()
  return(as.data.frame(results))
}


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
    m$na.action <- quote(na.pass)
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
  y <- model.response(m,type="any")
  
  # Force the inclusion of all data columns, they might be needed by some transformation procedure
  if(sum(!(colnames(data) %in% colnames(x))) > 0){
    x <- cbind(
      as.data.frame(if(sum(!(colnames(data) %in% colnames(x)))==1){
        setNames(
          data.frame(data[,!(colnames(data) %in% colnames(x))]),
          nm = colnames(data)[!(colnames(data) %in% colnames(x))]
        )
      } else {
        data[,!(colnames(data) %in% colnames(x))]
      }),
      as.data.frame(x)
    )
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

predict.train <- function (object, newdata = NULL, type = "raw", na.action = na.pass, ...){
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
  if(sum(!(colnames(newdata_ini) %in% colnames(newdata))) > 0){
    newdata <- cbind(
      as.data.frame(if(sum(!(colnames(newdata_ini) %in% colnames(newdata)))==1){
        setNames(
          data.frame(newdata_ini[,!(colnames(newdata_ini) %in% colnames(newdata))]),
          nm = colnames(newdata_ini)[!(colnames(newdata_ini) %in% colnames(newdata))]
        )
      } else {
        newdata_ini[,!(colnames(newdata_ini) %in% colnames(newdata))]
      }),
      as.data.frame(newdata)
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
