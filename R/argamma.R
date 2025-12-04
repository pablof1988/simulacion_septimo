#' @title Acceptance and rejection method for the GAMMA distribution
#'
#' @description
#' Implementation of the Acceptance–Rejection method to generate random
#' numbers from a Gamma(k, θ) distribution.
#' The algorithm uses a Uniform(0, b) proposal distribution, where
#' \code{b} corresponds to the 0.999 quantile of the Gamma distribution, and
#' constructs a constant majorizing function based on the maximum value of
#' the Gamma density at its mode \code{xmax = (k - 1) * theta}.
#'
#' Each candidate \code{x} generated from the Uniform proposal is accepted with
#' probability \code{f(x) / m(x)}, where \code{f(x)} is the target Gamma
#' density and \code{m(x)} is the majorizing function.
#' This procedure ensures that all accepted values follow the specified
#' Gamma distribution.
#'
#' @usage
#' argamma(n, k, theta)
#'
#' @param n Number of random values to generate.
#' @param k Shape parameter of the Gamma distribution.
#' @param theta Scale parameter of the Gamma distribution.
#'
#' @return
#' A numeric vector of length \code{n} containing random values generated
#' from a Gamma(k, θ) distribution.
#'
#' @examples
#' # Generate 100 values from a Gamma(3, 2) distribution
#' argamma(100, k = 3, theta = 2)
#'
#' @export
argamma <- function(n, k, theta) {
  x_max <- (k - 1) * theta
  f_max <- dgamma(x_max, shape = k, scale = theta)
  dominio_max <- qgamma(0.999, shape = k, scale = theta)
  f_objetivo <- function(x) {
    dgamma(x, shape = k, scale = theta)
  }
  muestras <- numeric(n)
  i <- 0
  while (i < n) {
    u1 <- glewis(1)
    u2 <- glewis(1)
    x <- u1 * dominio_max
    P_aceptar <- f_objetivo(x) / f_max
    if (u2 <= P_aceptar) {
      i <- i + 1
      muestras[i] <- x
    }
  }
  return(muestras)
}
