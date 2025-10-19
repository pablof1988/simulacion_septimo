#' @title Pseudorandom Number Generator using the Lewis Method
#'
#' @description
#' Implements the *Multiplicative Congruential Generator*
#' for capacity of the IBM System/360 computers create a sequence of
#' pseudo-random numbers.
#'
#'
#' @usage glewis(n,seed)
#'
#' @param n Integer. Number of pseudorandom values to generate.
#' @param seed Integer initial value (the seed), between 1 and \eqn{m - 1}.
#'
#'
#' @details
#'The generator produces a sequence of pseudorandom numbers.
#' \deqn{X_{i+1} = (a \times X_i) \mod m}
#' where \eqn{a = 7^5} and \eqn{m = 2^{31} - 1 = 2147483647}.
#'
#' Here, \eqn{m} is a *prime number*, and \eqn{a = 16807}
#' is a *positive primitive root* modulo \eqn{m}, which guarantees a *maximal period*
#' of \eqn{m - 1}.
#'

#' @return
#' A numeric vector of length \code{n} containing pseudorandom numbers
#' uniformly distributed.
#'
#' @references
#' Lewis, P. A. W., Goodman, A. S., & Miller, J. M. (1969).
#' A pseudo-random number generator for the System/360.
#' IBM Systems Journal, 8(2), 136–146.
#'
#' @examples
#' # Generate 5 pseudorandom numbers using the Lewis method
#' glewis(5,12345 )
#'
#' # Try with a different seed
#' glewis(5,987654)
#'
#' @export
glewis <- function(n, seed) {
  a <- 7^5
  m <- (2^31) - 1
  x <- seed
  result <- numeric(n)
  for (i in 1:n) {
    x <- (a * x) %% m
    result[i] <- x / m
  }
  return(result)
}
