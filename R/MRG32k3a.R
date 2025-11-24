#' @title MRG32k3a Multiple Recursive Generator
#'
#' @description
#' Implementation of the combined multiple-recursive generator MRG32k3a
#' (L'Ecuyer, 1999), producing uniform pseudorandom numbers in \eqn{(0,1)}.
#'
#' The generator uses two linear recurrences modulo \eqn{m_1} and \eqn{m_2},
#' achieving a maximal period approximately equal to:
#'
#' \deqn{(m_1^3 - 1)(m_2^3 - 1)/2.}
#'
#' This implementation provides excellent statistical quality and long period.
#'
#' @usage
#' MRG32k3a(n,
#'   s10 = 1, s11 = 1, s12 = 1,
#'   s20 = 1, s21 = 1, s22 = 1
#' )
#'
#' @param n Positive integer. Number of uniform pseudorandom numbers to generate.
#'
#' @param s10,s11,s12,s20,s21,s22
#' Integers defining the initial seeds of the generator.
#' These six values correspond to the initial internal states of the two
#' recurrences. They must satisfy \eqn{0 < s_{ij} < m_k}.
#'
#' @details
#' The MRG32k3a generator is defined by the recurrences:
#'
#' **Component 1**
#' \deqn{
#'   p_1 = (a_{12} s_{11} + a_{13} s_{12}) \bmod m_1
#' }
#'
#' **Component 2**
#' \deqn{
#'   p_2 = (a_{21} s_{20} + a_{23} s_{22}) \bmod m_2
#' }
#'
#' with parameters:
#' \itemize{
#'   \item \eqn{m_1 = 4294967087}
#'   \item \eqn{m_2 = 4294944443}
#'   \item \eqn{a_{12} = 1403580,\ a_{13} = -810728}
#'   \item \eqn{a_{21} = 527612,\ a_{23} = -1370589}
#' }
#'
#' The combined value is:
#'
#' \deqn{
#'   z_n = (p_1 - p_2) \bmod m_1
#' }
#'
#' and the uniform output is obtained through:
#'
#' \deqn{
#'   u_n = \frac{z_n}{m_1 + 1}.
#' }
#'
#' @return
#' A numeric vector of length \code{n} containing uniform \eqn{(0,1)} values
#' produced by the MRG32k3a generator.
#'
#' @examples
#' # Generate 5 values using the default seeds
#' MRG32k3a(5)
#'
#' # Use a custom initial state
#' MRG32k3a(5, s10 = 7, s11 = 11, s12 = 19)
#'
#' # Quick histogram
#' x <- MRG32k3a(10000)
#' hist(x, breaks = 20)
#'
#' @references
#' L'Ecuyer, P. (1999). *Good Parameters and Implementations for Combined
#' Multiple Recursive Random Number Generators*. Operations Research.
#'
#' L'Ecuyer, P. (2012). *Random Number Generation*. Handbook of Computational
#' Statistics.
#'
#' @export
MRG32k3a <- function(n,
                     s10 = 1, s11 = 1, s12 = 1,
                     s20 = 1, s21 = 1, s22 = 1) {

  m1 <- 4294967087
  m2 <- 4294944443

  a12 <- 1403580
  a13 <- -810728
  a21 <- 527612
  a23 <- -1370589

  norm <- 1 / (m1 + 1)


  st <- list(s10=s10, s11=s11, s12=s12,
             s20=s20, s21=s21, s22=s22)

  next_cmrgrand <- function() {
    p1 <- (a12 * st$s11 + a13 * st$s12) %% m1
    st$s12 <<- st$s11
    st$s11 <<- st$s10
    st$s10 <<- p1

    p2 <- (a21 * st$s20 + a23 * st$s22) %% m2
    st$s22 <<- st$s21
    st$s21 <<- st$s20
    st$s20 <<- p2

    z <- (p1 - p2) %% m1
    if (z <= 0) z <- z + m1

    z * norm
  }

  next_unique <- function() {
    u1 <- next_cmrgrand()
    u2 <- next_cmrgrand()
    v <- u1 + u2 / (2^26)
    if (v >= 1) v <- v - 1
    v
  }

  out <- numeric(n)
  for (i in seq_len(n)) out[i] <- next_unique()
  out
}

