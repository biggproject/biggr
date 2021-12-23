#' @importFrom utils assignInNamespace

.onAttach <- function(...) {
  
  assignInNamespace("train.formula", train.formula, ns="caret")

  unlockBinding("predictionFunction", as.environment("package:caret"))
  assignInNamespace("predictionFunction", predictionFunction, ns="caret", envir=as.environment("package:caret"))
  assign("predictionFunction", predictionFunction, pos="package:caret")
  lockBinding("predictionFunction", as.environment("package:caret"))

  if (!interactive()) return()
  
  tip <- c("Warning: The following Caret functions will be overwritten\n",
           "because some modelling functionalities of biggr needs a specific behaviour:\n",
           "train.formula and predictionFunction")
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    
}