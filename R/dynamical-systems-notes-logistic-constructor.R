#' Simple Logistic System Function Constructor
#'
#' @details The logistic system is given by
#'   \deqn{
#'     y_{i t} = r y_{i \left( t - 1 \right)}
#'     \left( 1 - \frac{y_{i \left( t - 1 \right)}}{K} \right)
#'   }
#'   where
#'   \eqn{r} is the growth rate and \eqn{K} is the carrying capacity.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param r Numeric.
#'   Growth rate.
#' @param K Numeric
#'   Carrying capacity.
#'
#' @examples
#' func <- LogisticConstructor(r = 1.5, K = 10)
#' func(0.1)
#'
#' @export
LogisticConstructor <- function(r,
                                K) {
  return(
    function(y) {
      return(
        (
          r * y
        ) * (
          1 - (y / K)
        )
      )
    }
  )
}
