#' Fixed Point
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param y0 Numeric.
#'   Initial condition.
#' @param func Function.
#'   The input is \eqn{y_{i \left( t - 1\right)}}
#'   and the output is \eqn{y_{i t}}.
#' @param tol Small numeric value.
#'   Convergence criteria.
#' @param max_iter Large positive integer.
#'   Maximum number of iterations.
#'
#' @examples
#' # linear
#' func <- LinearConstructor(alpha = 8.0, beta = 0.8)
#' FixedPoint(y0 = 0.01, func = func)
#'
#' # logistic
#' func <- LogisticConstructor(r = 1.5, K = 10)
#' FixedPoint(y0 = 0.01, func = func)
#'
#' @export
FixedPoint <- function(y0,
                       func,
                       tol = sqrt(.Machine$double.eps),
                       max_iter = 1000L) {
  y <- rep(x = NA_real_, times = max_iter)
  y[1] <- y0
  y_new <- func(y0)
  y_old <- y0
  iter <- 1
  while (abs(y_new - y_old) > tol) {
    y_old <- y_new
    y_new <- func(y_new)
    iter <- iter + 1
    y[iter] <- y_new
    if (iter == max_iter) {
      warning(
        paste(
          max_iter,
          "iterations reached without meeting the convergence criteria."
        )
      )
      break()
    }
  }
  return(
    y[stats::complete.cases(y)]
  )
}
