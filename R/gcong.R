#'
#' @title Linear Congruential Generator
#'
#' @description
#' Implements a linear congruential generator to create a sequence of pseudo-random numbers.
#'
#' @param n Integer. Number of numbers to generate.
#' @param seed Integer. Initial value that determines the starting point of the sequence.
#' @param a Integer. The multiplier used in the generator.
#' @param c Integer, optional. The additive constant. Default is 0.
#' @param m Integer. The modulus used in the generator.
#'
#' @details
#' The linear congruential generator is one of the oldest and best-known algorithms for generating pseudo-random numbers.
#' The quality of the generated sequence depends on the appropriate choice of parameters \code{a}, \code{c}, and \code{m}.
#'
#' The formula used is: \deqn{x[i+1] = (a * x[i] + c) \mod m}
#'
#' @return
#' A numeric vector of length \code{n} containing the sequence of normalized pseudo-random numbers.
#'
#' @examples
#' # Generate a sequence with arbitrary parameters
#' sq1 <- gcong(n = 100, seed = 12, a = 64, c = 11, m = 7)
#'
#' # Generate another sequence without an additive constant
#' seq2 <- gcong(n = 100, seed = 2003, a = 120, m = 120)
#'
#' # Evaluate the quality of a sequence
#' seq3 <- gcong(n = 100, seed = 1610, a = 21, c = 25, m = 2^10)
#' uniftest(seq3)
#'
#' @export
gcong <- function(n, seed, a, c = 0, m) {
  x <- seed
  for (i in 1:n) {
    x[i+1] <- ((a * x[i]) + c) %% m
  }
  x[-1] / m
}
