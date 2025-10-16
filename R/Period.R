#' @title Period
#'
#' @description
#' This function counts the number of unique, non-repeated elements in a sequence or vector.
#' In other words, it identifies and counts the values that appear exactly once within
#' the sequence.
#'
#' @usage Period(x)
#'
#' @param x a random sequence
#'
#' @return Returns a numeric value representing the number of unique, non-repeated
#' elements in the input sequence or vector.In other words, the count of values
#' that appear exactly once in the sequence.
#'
#' @references
#' R Core Team (2024). R: A Language and Environment for Statistical Computing.
#' R Foundation for Statistical Computing, Vienna, Austria.
#'
#' @examples
#' x <- runif(n = 100, min = 0, max = 1)
#' Period(x)
#'
#' @export
Period <- function(x){
  sum(!duplicated(x))
}
