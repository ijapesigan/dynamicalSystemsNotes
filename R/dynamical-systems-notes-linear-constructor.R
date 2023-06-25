#' Simple Linear System Function Constructor
#'
#' @details The simple linear system is given by
#'   \deqn{
#'     y_{i t} = \alpha + \beta y_{i \left( t - 1 \right)}
#'   }
#'   where
#'   \eqn{\alpha} is the intercept and \eqn{\beta} is the slope.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param alpha Numeric.
#'   Intercept \eqn{\alpha}.
#' @param beta Numeric.
#'   Slope \eqn{\beta}.
#' @param sigmasq Positive numeric value.
#'   Variance of the random error term with expected value of zero.
#'   If `sigmasq = NULL`, the system is deterministic.
#'
#' @examples
#' func <- LinearConstructor(alpha = 8.0, beta = 0.8)
#' func(0.1)
#'
#' @export
LinearConstructor <- function(alpha,
                              beta,
                              sigmasq = NULL) {
  if (is.null(sigmasq)) {
    return(
      function(y) {
        return(
          alpha + beta * y
        )
      }
    )
  } else {
    return(
      function(y) {
        return(
          (
            alpha + beta * y
          ) + stats::rnorm(
            n = 1,
            mean = 0,
            sd = sqrt(sigmasq)
          )
        )
      }
    )
  }
}
