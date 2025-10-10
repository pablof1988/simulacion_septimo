#' @title Test of Independence for Random Sequences
#'
#' @description
#' This function performs a chi-square test of independence to evaluate
#' whether a given sequence of random numbers is independent.
#' The test compares the observed joint frequencies of successive pairs
#' in the sequence with the expected frequencies under the hypothesis
#' of independence between consecutive values.
#'
#' The test is commonly used in the analysis of random number generators
#' to detect potential autocorrelation or dependence patterns in simulated data.
#'
#' @usage indeptest(x, lags)
#'
#' @param x A random sequence
#' @param lags The number of lags (autocorrelation terms) to include in the test statistic.
#'
#' @return The function returns the test statistic (Q), the degrees of freedom,
#' and the p-value of a chi-square test for serial independence based on
#' sample autocorrelations up to the specified number of lags.
#'
#'
#' @references Burns, P. (2002). Robustness of the Ljung-Box test and its rank equivalent. Available at SSRN 443560.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' independ(x, lags = 10)
#'
#' @export
independ <- function (x, lags) {
  n <- length(x)
  rho <- stats::acf(x, lag.max = lags, type = "correlation", plot = FALSE)$acf[-1]
  Q <- n * (n + 2) * sum((rho^2) / (n - 1:lags))
  gl <- lags
  p_valor <- stats::pchisq(Q, df = gl, lower.tail = FALSE)
  c(estadist = Q, gl = gl, p_valor = p_valor)
}
