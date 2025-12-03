#' @title Standard Normal Random Number Generator
#'
#' @description
#' Generates a sample of size \code{n} from the standard normal distribution
#' using the Box–Muller transform. This is an exact method that converts
#' pairs of independent uniform random variables into independent standard
#' normal values.
#'
#'
#' @usage normbox(n)
#'
#' @param n Integer. Number of random standard normal values to generate.
#'
#' @return  A numeric vector of length \code{n} containing independent draws from
#' the standard normal distribution \eqn{N(0,1)}.
#'
#'
#' @references
#' Box, G. E. P., & Muller, M. E. (1958). A note on the generation of random normal deviates.
#' The Annals of Mathematical Statistics, 29(2), 610–611. Available at JSTOR 2237388.
#'
#' @examples
#' set.seed(123)
#' x <- normbox(10)
#' mean(x)   # approximately 0
#' var(x)    # approximately 1
#'
#' @export
normbox <- function(n) {
  m <- ceiling(n / 2)
  pares <- replicate(m, {
    u12 <- glewis(2)
    z1 <- sqrt(-2 * log(1 - u12[1])) * sin(2 * pi * u12[2])
    z2 <- sqrt(-2 * log(1 - u12[1])) * cos(2 * pi * u12[2])
    c(z1, z2)
  })
  as.vector(pares)[1:n]
}

