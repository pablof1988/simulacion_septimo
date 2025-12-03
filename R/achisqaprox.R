#'
#' @title Monte Carlo Approximation of the Chi-Square Distribution
#'
#'@description
#  Generates a Monte Carlo approximation of a Chi-square random variable
#' using standard normal random variables. Each simulated value is obtained
#' as the sum of squared standard normal variables generated with
#' \code{normbox}.
#'
#' @usage achisqaprox(m, v)
#'
#' @param m Number of Monte Carlo replications.
#' @param v Degrees of freedom of the Chi-square distribution.
#'
#' @return A numeric vector of length \code{m} containing simulated values
#' from a Chi-square distribution with \code{v} degrees of freedom.
#'
#' @details
#' If Z₁, Z₂, ..., Zᵥ are independent standard normal random variables,
#' then the random variable ∑ Zᵢ² follows a Chi-square distribution with
#' \code{v} degrees of freedom.
#'
#' This function uses the \code{normbox} function from the
#' \code{erandom} package to generate the standard normal samples.
#'
#' @seealso \code{\link[stats]{dchisq}}, \code{normbox}
#'
#' @examples
#' library(erandom)
#' x <- achisqaprox(m = 5000, v = 5)
#' hist(x, probability = TRUE, main = "Chi-square approximation")
#' curve(dchisq(x, df = 5), col = "red", add = TRUE)
#'
#' @export
achisqaprox <- function(m, v){
  replicate(m, {
    normalestand <- normbox(v)
    sum(normalestand^2)
  })
}
