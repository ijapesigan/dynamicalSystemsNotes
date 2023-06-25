#' Cobweb plot
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
#' Cobweb(y0 = 0.01, func = func)
#'
#' # logistic
#' func <- LogisticConstructor(r = 1.5, K = 10)
#' Cobweb(y0 = 0.01, func = func)
#'
#' @export
Cobweb <- function(y0,
                   func,
                   tol = 1e-11,
                   max_iter = 1000000L) {
  y <- FixedPoint(
    y0 = y0,
    func = func,
    tol = tol,
    max_iter = max_iter
  )
  nt <- length(y)
  x <- seq(
    from = min(y),
    to = max(y),
    length.out = nt
  )
  plot(
    x = x,
    y = func(x),
    type = "l",
    col = "red",
    lwd = 2,
    xlab = expression(y[t - 1]),
    ylab = expression(y[t]),
    main = "Cobweb Plot",
    xaxs = "i",
    yaxs = "i"
  )
  graphics::abline(0, 1, lwd = 2)
  y_new <- func(y0)
  y_old <- y0
  graphics::lines(
    x = c(y_old, y_old),
    y = c(0, y_new)
  )
  for (i in 1:nt) {
    graphics::lines(
      x = c(y_old, y_new),
      y = c(y_new, y_new)
    )
    y_old <- y_new
    y_new <- func(y_new)
    graphics::lines(
      x = c(y_old, y_old),
      y = c(y_old, y_new)
    )
  }
}
