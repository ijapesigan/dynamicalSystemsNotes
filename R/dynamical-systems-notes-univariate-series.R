#' Univariate Time Series
#'
#' @details The univariate time series
#'   is generated using the following equation
#'   \deqn{
#'     y_{i t} = f \left( y_{i \left( t - 1 \right)} \right) .
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param y0 Numeric.
#'   Initial condition.
#' @param func Function.
#'   The input is \eqn{y_{i \left( t - 1\right)}}
#'   and the output is \eqn{y_{i t}}.
#' @param nt Positive integer.
#'   Maximum discrete time points.
#'
#' @examples
#' # linear
#' func <- LinearConstructor(alpha = 8.0, beta = 0.8)
#' y <- UnivSeries(y0 = 0.01, func = func, nt = 100)
#' plot(y)
#'
#' # logistic
#' func <- LogisticConstructor(r = 1.5, K = 10)
#' y <- UnivSeries(y0 = 0.01, func = func, nt = 100)
#' plot(y)
#'
#' @export
UnivSeries <- function(y0,
                       func,
                       nt) {
  y <- double(nt)
  y[1] <- y0
  for (i in 2:nt) {
    y[i] <- func(y[i - 1])
  }
  return(y)
}
