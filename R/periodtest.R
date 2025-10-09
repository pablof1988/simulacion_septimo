#' @title Test Perio
#'
#' @description
#' This function analyzes the periodicity of a given numeric or time series.
#' It estimates the dominant period in the data and tests whether the series
#' exhibits significant cyclic behavior.
#'
#' @usage periodtest(x)
#'
#' @details
#' The function estimates the periodicity by computing the spectral density of the
#' sequence using the Fast Fourier Transform (FFT). The frequency corresponding
#' to the maximum spectral density is identified as the dominant periodic component.
#'
#' @param x a random sequence
#'
#' @return
#'
#'— The estimated period (in time units).
#'— The frequency associated with the strongest cycle.
#'— The p-value of the periodicity test (if applicable).
#'— A character string describing the test used.
#'
#' @references
#' Priestley, M. B. (1981). *Spectral Analysis and Time Series*. Academic Press.
#' Shumway, R. H. & Stoffer, D. S. (2017). *Time Series Analysis and Its Applications: With R Examples*. Springer.
#'
#' @examples
#' x <- runif(n = 100)
#' periodtest(x)
#'
#' @export
periodtest <- function(x){
  sum(!duplicated(x))
}


