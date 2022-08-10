library(GA)

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

  binary <- split(
    binary,
    rep.int(seq.int(bitOrders), times = bitOrders)
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
#' Local search using general-purpose optimisation algorithms can be
#' applied stochastically to exploit interesting regions.
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
#' during the search.
#' @return <list> of optim solution for each decision variable

optimize <- function(opt_criteria, opt_function, features, suggestions,
                     selection = gabin_tourSelection, keepBest = TRUE, popSize = 64,
                     maxiter = 20, parallel = 16, elitism = 0.08, pmutation = 0.05, ...) {
  minimise <- function(X, features, ...) {
    return(-opt_function(decodeValueFromBin(X, features), ...))
  }
  maximise <- function(X, features, ...) {
    return(opt_function(decodeValueFromBin(X, features), ...))
  }

  fitness <- minimise
  if (opt_criteria == "maximise") fitness <- maximise

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
      monitor = gaMonitor2,
      selection = selection,
      keepBest = keepBest,
      popSize = popSize,
      maxiter = maxiter,
      parallel = parallel,
      elitism = elitism,
      pmutation = pmutation,
      ...
    )
  )
  return(decodeValueFromBin(opt_results@solution[1, ], features))
}
