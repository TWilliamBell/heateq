#' Numerical Solution to the Heat Equation with Dirichlet Boundary Conditions
#'
#' @param n 
#' @param soln 
#' @param alpha 
#' @param dt 
#' @param eps 
#'
#' @return
#'
#' @examples

one.d.heat.eq.dirichlet <- function(n, soln, alpha, dt, eps) {
  du <- rep(0, length(soln[ , 1]))
  lambda <- dt*alpha/2
  for (i in seq_len(n)[-1]) {
    init <- soln[ , i-1]
    du[1] <- 2*init[1]-init[2]
    for (j in seq_along(du)[-c(1, length(du))]) {
      du[j] <- 2*init[j]-init[j-1]-init[j+1]
    }
    du[length(du)] <- 2*init[length(du)]-init[length(du)-1]
    soln[ , i] <- init - lambda*du
    soln[soln[ , i] < eps , i] <- 0
  }
  soln
}
