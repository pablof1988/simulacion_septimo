#' @title Acceptance and rejection method for the GAMMA distribution
#'
#' @description
#' Implementation of the **Acceptance–Rejection method** to generate random
#' numbers from a **Gamma(k, θ)** distribution.
#' The algorithm uses a **Uniform(0, b)** proposal distribution, where
#' \code{b} corresponds to the 0.999 quantile of the Gamma distribution, and
#' constructs a **constant majorizing function** based on the maximum value of
#' the Gamma density at its mode \code{xmax = (k - 1) * theta}.
#'
#' Each candidate \code{x} generated from the Uniform proposal is accepted with
#' probability \code{f(x) / m(x)}, where \code{f(x)} is the target Gamma
#' density and \code{m(x)} is the majorizing function.
#' This procedure ensures that all accepted values follow the specified
#' Gamma distribution.
#'
#' @usage
#' argamma(m, k, theta)
#'
#' @param m Number of random values to generate.
#' @param k Shape parameter of the Gamma distribution.
#' @param theta Scale parameter of the Gamma distribution.
#'
#' @return
#' A numeric vector of length \code{m} containing random values generated
#' from a Gamma(k, θ) distribution.
#'
#' @examples
#' # Generate 5 values from a Gamma(3, 2) distribution
#' argamma(5, k = 3, theta = 2)
#'
#' # Generate 10,000 values and compare with the theoretical density
#' x <- argamma(10000, k = 4, theta = 1)
#' hist(x, freq = FALSE, breaks = 40)
#' curve(dgamma(x, 4, 1), add = TRUE, col = "red", lwd = 2)
#'
#' @export
argamma <- function(m, k, theta){
  b <- qgamma(0.999, shape = k, scale = theta)
  xmax <- (k - 1) * theta
  mconst <- dgamma(xmax, shape = k, scale = theta)
  ma <- mconst + 0.01
  res <- numeric(m)
  i <- 1
  while(i <= m){
    u <- glewis(2)
    x <- u[1] * b
    acept <- dgamma(x, k, theta) / mconst
    if(u[2] <= acept){
      res[i] <- x
      i <- i + 1
    }
  }
  res
}
