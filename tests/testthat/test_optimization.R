library(lubridate)
Sys.setlocale("LC_ALL", "en_US.UTF-8")

features__ <- list(
  "a" = list(
    levels = c("mean", "max", "min"),
    datatype = "discrete"
  ),
  "b" = list(
    min = 3,
    max = 20,
    nlevels = 20 - 3,
    datatype = "integer"
  ),
  "c" = list(
    min = 30,
    max = 60,
    nlevels = 36,
    datatype = "float"
  )
)

# Initial suggestion to help optimizer
suggestions__ <- decodeBinFromValue(
  values = list("min", 10, 39.9),
  features = features__
)

# Cost function to evaluate
opt_function__ <- function(X, df, test_arg1, ...) {
  score <- if (X$a == "mean") {
    mean(as.matrix(df) * X$b - X$c, na.rm = T) + test_arg1
  } else if (X$a == "max") {
    max(as.matrix(df) * X$b - X$c, na.rm = T) + test_arg1
  } else if (X$a == "min") {
    min(as.matrix(df) * X$b - X$c, na.rm = T) + test_arg1
  }
  if (!is.finite(score)) {
    score <- 100
  }
  return(
    score
  )
}

test_that("Test GA optimization - minimize", {
  df__ <- data.frame(rnorm(100, 10, 4), rnorm(100, 10, 50))
  test_arg1__ <- 45

  value <- optimize(
    opt_criteria = "minimise",
    opt_function = opt_function__,
    features = features__,
    suggestions = suggestions__,
    df = df__,
    test_arg1 = test_arg1__
  )

  expected <- list(a = "min", b = 20, c = 60)
  expect(
    all.equal(value, expected),
    "Expected and obtained are different"
  )
})

test_that("Test GA optimization - maximise", {
  df__ <- data.frame(rnorm(100, 10, 4), rnorm(100, 10, 50))
  test_arg1__ <- 45
  value <- optimize(
    opt_criteria = "maximise",
    opt_function = opt_function__,
    features = features__,
    suggestions = suggestions__,
    popSize = 64,
    pmutation = 0.05,
    elitism = 0.08,
    maxiter = 20,
    df = df__,
    test_arg1 = test_arg1__
  )

  expected <- list(a = "max", b = 20, c = 30)
  expect(
    all.equal(value, expected),
    "Expected and obtained are different"
  )
})
