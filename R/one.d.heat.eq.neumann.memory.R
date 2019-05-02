#' Numerical Solution to the Heat Equation with Neumann Boundary Conditions in R
#' 
#' This function only returns the final result and the initial data, rather than any of the intermediates.
#'
#' @param n the Number of Time steps you wish to solve for.
#' @param init.dat the initial layout of points (must be a vector).
#' @param alpha the heat equation is parameterized here by dU/dt = alpha * d^2U/(dx)^2, if you set alpha large you may need to set the time step shorter.
#' @param dt the length of each time step.
#' @param eps sometimes I've observed unexpected behaviour when the numbers get very small, in order to combat this I include an epsilon such that if a value is within that neighbourhood of zero they will be set to zero.  (Epsilon can be set to zero if you do not wish for this confound.)
#'
#' @return
#'
#' @examples

one.d.heat.eq.neumann.memory <- function(n, init.dat, alpha, dt, eps) {
  du <- rep(0, length(init.dat))
  lambda <- dt*alpha/2
  init <- init.dat
  for (i in seq_len(n)[-1]) {
    du[1] <- init[1]-init[2]
    for (j in seq_along(du)[-c(1, length(du))]) {
      du[j] <- 2*init[j]-init[j-1]-init[j+1]
    }
    du[length(du)] <- init[length(init)]-init[length(init)-1]
    init <- init - lambda*du
    init[init < eps] <- 0
  }
  init
}
