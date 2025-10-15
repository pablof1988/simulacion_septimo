#' @title Test of Independence for Random Sequences
#'
#' @description
#'This function performs the Ljung-Box test to verify whether a sequence of random
#'numbers is truly independent across its lags. It takes the series of generated
#'values and calculates the autocorrelations between the numbers at different lags
#'up to a certain limit. Using these autocorrelations, it calculates the Q statistic,
#'which it then compares to a chi-square distribution to determine whether there is
#'time dependence.
#'
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
