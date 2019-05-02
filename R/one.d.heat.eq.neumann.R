
#' Numerical Solution to the Heat Equation with Neumann boundary conditions, written in R
#' 
#' This function is not very memory efficient since it saves all intermediate steps.  However it gives you all of the information.  If you don't know what you want you should use the C++ version instead.
#'
#' @param n the Number of Time steps you wish to solve for.
#' @param soln the initial layout of points (must be a vector).
#' @param alpha the heat equation is parameterized here by dU/dt = alpha * d^2U/(dx)^2, if you set alpha large you may need to set the time step shorter.
#' @param dt the length of each time step.
#' @param eps sometimes I've observed unexpected behaviour when the numbers get very small, in order to combat this I include an epsilon such that if a value is within that neighbourhood of zero they will be set to zero.  (Epsilon can be set to zero if you do not wish for this confound.)
#'
#' @return
#' @export
#'
#' @examples

one.d.heat.eq.neumann <- function(n, soln, alpha, dt, eps) {
  du <- rep(0, length(soln[ , 1]))
  lambda <- dt*alpha/2
  for (i in seq_len(n)[-1]) {
    init <- soln[ , i-1]
    du[1] <- init[1]-init[2]
    for (j in seq_along(du)[-c(1, length(du))]) {
      du[j] <- 2*init[j]-init[j-1]-init[j+1]
    }
    du[length(du)] <- init[length(init)]-init[length(init)-1]
    soln[ , i] <- init - lambda*du
    soln[soln[ , i] < eps , i] <- 0
  }
  soln
}