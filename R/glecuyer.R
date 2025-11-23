#' @title Combined Random Number Generators
#'
#' @description
#' Implementación del generador combinado de L'Ecuyer para producir números
#' pseudoaleatorios uniformes en (0,1). Utiliza dos generadores congruenciales
#' multiplicativos que se combinan para mejorar el periodo y las propiedades estadísticas.
#'
#' @usage
#' glecuyer(n = 1, s1 = 12345, s2 = 67890,
#'          m1 = 2147483563, a1 = 40014,
#'          m2 = 2147483399, a2 = 40692)
#'
#' @param n Cantidad de números aleatorios a generar.
#' @param s1 Semilla inicial del primer generador.
#' @param s2 Semilla inicial del segundo generador.
#' @param m1 Módulo del primer generador.
#' @param a1 Multiplicador del primer generador.
#' @param m2 Módulo del segundo generador.
#' @param a2 Multiplicador del segundo generador.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{numeros}{Vector de números aleatorios uniformes en (0,1).}
#'   \item{semillas_finales}{Vector con las semillas actualizadas \code{s1} y \code{s2}.}
#' }
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

