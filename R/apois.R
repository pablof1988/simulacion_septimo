#' @title Poisson Random Number Generator
#'
#' @description
#' Generates a sample of `m` random numbers from a Poisson distribution with
#' parameter `lambda` using the **inverse transform method** on the cumulative
#' distribution function (CDF). This is an **exact method** — no approximation.
#'
#' @usage apois(lambda, m)
#'
#' @param lambda Mean of the Poisson distribution. Must be a positive number
#'   (\\(\\lambda > 0\\)). Larger values imply more events on average.
#' @param m Number of random numbers to generate. Must be a positive integer.
#'
#' @return An integer vector of length `m` with values \\(\\geq 0\\), following
#'   a Poisson(\\( \\lambda \\)) distribution.
#'
#' @references
#' Romero, T. (2019).
#' *Simulation of Random Variables*.
#' In: *Computational Statistics*.
#' Available at:
#' \url{https://tereom.github.io/est-computacional-2019/simulacion-de-variables-aleatorias.html}
#'
#' @examples
#' x <- apois(lambda = 7, m = 10)
#' x
#' mean(x)  # approximately 7
#' var(x)   # approximately 7
#'
#' @importFrom stats dpois qpois runif
#' @export
apois <- function(lambda, m) {
  stopifnot(lambda > 0, m >= 1, m == as.integer(m))

  u  <- runif(m)                          # Step 1: U ~ Unif(0,1)
  k  <- qpois(0.999, lambda = lambda)     # Upper bound: covers 99.9% of mass
  Fx <- cumsum(dpois(0:k, lambda = lambda))  # CDF: F(x) = P(X <= x)
  findInterval(u, Fx)                     # Inversion: X = min { x : F(x) >= u }
}
