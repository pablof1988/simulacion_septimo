#' @title Binomial Random Number Generator
#'
#' @description
#' Generates a sample of m random numbers from a binomial distribution with
#' parameters n (number of trials) and p (success probability) using
#' the method on the cumulative distribution function (CDF).
#' This is an exact method — no approximation.
#'
#' @usage abinom(m, n, p)
#'
#' @param m Number of random numbers to generate. Must be a positive integer.
#' @param n Number of trials in the binomial experiment. Must be a non-negative
#'   integer (n >= 0).
#' @param p Probability of success on each trial. Must be between 0 and 1.
#'
#' @return An integer vector of length m with values between 0 and n,
#'   following a Binomial(n, p) distribution.
#'
#' @examples
#' set.seed(123)
#' x <- abinom(m = 10, n = 5, p = 0.3)
#' x
#' mean(x)  # approximately 1.5
#' var(x)   # approximately 1.05
#'
#' @importFrom stats dbinom
#' @export
abinom <- function(m, n, p) {
  stopifnot(
    m >= 1, m == as.integer(m),
    n >= 0, n == as.integer(n),
    p >= 0, p <= 1
  )

  u  <- glewis(m)                         # Uniform(0,1) from glewis
  Fx <- cumsum(dbinom(0:n, n, p))         # CDF: F(x) = P(X <= x)
  findInterval(u, Fx)                     # Inversion
}
