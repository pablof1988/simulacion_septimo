#' @title Test for Uniformity Unif(0, 1)
#'
#' @description
#' This function implements the chi-square goodness of fit test to prove
#' uniformity U(0, 1) from a sequence
#'
#' @usage uniftest(u)
#'
#' @param u a random sequence
#'
#' @return The function returns the test statistic, the p-value and the degree
#' of freedom of a chi-square goodness of fit test to prove
#' uniformity U(0, 1) from a sequence.
#'
#' @references Andreis, F. (2014). R and Introduction to Simulation.
#'
#' @examples
#' u <- runif(n = 100, min = 0, max = 1)
#' uniftest(u)
#'
#' @export

uniftest <- function(u){
  # población
  n <- length(u)
  probs <- rep(0.1, 10)
  pi <- n * probs

  # muestra
  tf <- fdth::fdt(u, start = 0, end = 1, h = 0.1)
  pi0 <- tf$table$f

  # test
  est <- sum(((pi0 - pi)^2) / pi)
  pval <- pchisq(est, 9, lower.tail = F)

  c("est" = est, "g.l" = 9, "p-val" = pval)
}
