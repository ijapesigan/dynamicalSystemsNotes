% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynamical-systems-notes-cobweb.R
\name{Cobweb}
\alias{Cobweb}
\title{Cobweb Plot}
\usage{
Cobweb(y0, func, tol = sqrt(.Machine$double.eps), max_iter = 1000L)
}
\arguments{
\item{y0}{Numeric.
Initial condition.}

\item{func}{Function.
The input is \eqn{y_{i \left( t - 1\right)}}
and the output is \eqn{y_{i t}}.}

\item{tol}{Small numeric value.
Convergence criteria.}

\item{max_iter}{Large positive integer.
Maximum number of iterations.}
}
\description{
Cobweb Plot
}
\examples{
# linear
func <- LinearConstructor(alpha = 8.0, beta = 0.8)
Cobweb(y0 = 0.01, func = func)

# logistic
func <- LogisticConstructor(r = 1.5, K = 10)
Cobweb(y0 = 0.01, func = func)

}
\author{
Ivan Jacob Agaloos Pesigan
}
