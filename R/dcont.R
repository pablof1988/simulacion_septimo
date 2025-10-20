dcont <- function(x){
  ifelse(x >= 0, 0.5*exp(-0.5*x), 0)
}

curve(dcont(x), -0.01, 5)
integrate(dcont, 0.5, 2)

## Función de probabilidad acumulada.
# P(X < 1) = F(1)

pcont <- function(q){
  integrate(dcont, 0, q)$value
}

pcont(1)


#######
dcont2 <- function(x){
  ifelse(x >= 0, 0.5*x^2*exp(-0.5*x), 0)
}
integrate(dcont2, 0, Inf)
