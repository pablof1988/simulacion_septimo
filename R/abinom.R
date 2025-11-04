#' @title Binomial Random Number Generator
#'
#' @description
#' Generates a sample of `m` random numbers from a **binomial distribution**
#' with parameters `n` (number of trials) and `p` (success probability) using
#' the **inverse transform method** on the cumulative distribution function (CDF).
#' This is an **exact method** — no approximation.
#'
#' @usage abinom(m, n, p)
#'
#' @param m Number of random numbers to generate. Must be a positive integer.
#' @param n Number of trials (\eqn{n \ge 0}).
#' @param p Probability of success (\eqn{0 \le p \le 1}).
#'
#' @return Integer vector of length `m` following a Binomial\eqn{(n, p)} distribution.
#'
#' @examples
#' set.seed(123)
#' abinom(m = 10, n = 5, p = 0.3)
#'
#' @importFrom stats dbinom
#' @export
abinom <- function(m, n, p) {
  stopifnot(
    m >= 1, m == as.integer(m),
    n >= 0, n == as.integer(n),
    p >= 0, p <= 1
  )
  u  <- glewis(m)
  Fx <- cumsum(dbinom(0:n, n, p))
  findInterval(u, Fx)
}

