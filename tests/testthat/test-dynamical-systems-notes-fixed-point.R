## ---- test-dynamical-systems-notes-fixed-point
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    func <- LinearConstructor(alpha = 8.0, beta = 0.8)
    linear <- FixedPoint(y0 = 0.01, func = func)
    Cobweb(y0 = 0.01, func = func)
    y_linear <- UnivSeries(y0 = 0.01, func = func, nt = 1000000L)
    func <- LogisticConstructor(r = 1.5, K = 10)
    logistic <- FixedPoint(y0 = 0.01, func = func)
    Cobweb(y0 = 0.01, func = func)
    y_logistic <- UnivSeries(y0 = 0.01, func = func, nt = 1000000L)
    testthat::test_that(
      paste(text, "linear"),
      {
        testthat::expect_true(
          abs(
            40 - linear[length(linear)]
          ) <= tol
        )
        testthat::expect_true(
          abs(
            40 - y_linear[length(y_linear)]
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "logistic"),
      {
        testthat::expect_true(
          abs(
            3.33333333 - logistic[length(logistic)]
          ) <= tol
        )
        testthat::expect_true(
          abs(
            3.33333333 - y_logistic[length(y_logistic)]
          ) <= tol
        )
      }
    )
  },
  text = "test-dynamical-systems-notes-fixed-point",
  tol = 1e-8
)
