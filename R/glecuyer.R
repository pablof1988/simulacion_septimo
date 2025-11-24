#' @title L’Ecuyer 1988 Pseudo-Random Number Generator
#'
#' @description
#' Implementation of L'Ecuyer's combined generator to
#' produce pseudo-random numbers uniformly distributed
#' in (0,1). It uses two multiplicative congruential generators
#' that are combined to improve the period and statistical properties.
#'
#' @usage
#' glecuyer(n = 1, s1 = 12345, s2 = 67890,
#'          m1 = 2147483563, a1 = 40014,
#'          m2 = 2147483399, a2 = 40692)
#'
#' @param n Number of random numbers to generate.
#' @param s1 Initial seed of the first generator.
#' @param s2 Initial seed of the second generator.
#' @param m1 Modulus of the first generator.
#' @param a1 Multiplier of the first generator.
#' @param m2 Modulus of the second generator.
#' @param a2 Multiplier of the second generator.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{numeros}{Vector of uniformly distributed random numbers in (0,1).}
#'   \item{semillas_finales}{Vector containing the updated seeds \code{s1} and
#'   \code{s2}.}
#' }
#'
#' @examples
#'
#' res2 <- glecuyer(n = 2, s1 = 12345, s2 = 67890)
#' sprintf("%.12f", res2$numeros)
#' res10000 <- glecuyer(n = 10000, s1 = 987654, s2 = 123456)
#' sprintf("%.12f", res10000$numeros)
#'
#' @export
glecuyer <- function(n = 1, s1 = 12345, s2 = 67890, m1 = 2147483563, a1 = 40014, m2 = 2147483399, a2 = 40692) {
  q1 <- m1 %/% a1; r1 <- m1 %% a1
  q2 <- m2 %/% a2; r2 <- m2 %% a2
  uniforms <- numeric(n)
  for (i in 1:n) {
    k1 <- s1 %/% q1
    s1 <- a1 * (s1 - k1 * q1) - k1 * r1
    if (s1 < 0) s1 <- s1 + m1
    k2 <- s2 %/% q2
    s2 <- a2 * (s2 - k2 * q2) - k2 * r2
    if (s2 < 0) s2 <- s2 + m2
    z <- s1 - s2
    if (z < 1) z <- z + (m1 - 1)
    uniforms[i] <- z / m1
  }
  list(numeros = uniforms, semillas_finales = c(s1 = s1, s2 = s2))
}




