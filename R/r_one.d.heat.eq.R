#' A One-Dimensional Heat Equation solver written in R
#' 
#' If you don't know what you want, you should use the C++ version instead (the default for one.d.heat.eq()).
#'
#' @param n the Number of Time steps you wish to solve for.
#' @param init.dat the initial layout of points (must be a vector).
#' @param boundary currently supports two boundary conditions, Neumann and Dirichlet.
#' @param alpha the heat equation is parameterized here by dU/dt = alpha * d^2U/(dx)^2, if you set alpha large you may need to set the time step shorter.
#' @param dt the length of each time step.
#' @param eps sometimes I've observed unexpected behaviour when the numbers get very small, in order to combat this I include an epsilon such that if a value is within that neighbourhood of zero they will be set to zero.  (Epsilon can be set to zero if you do not wish for this confound.)
#' @param save.intermediates A parameter stating whether you're interested in the final result only or also all of the intermediate time steps (beware, for long times this produces extremely large matrices).
#'
#' @return
#'
#' @examples
#' 

r_one.d.heat.eq <- function(n = 1000, init.dat = dgamma(seq(0, 5, 0.01), 
                                                        shape = 3), 
                            boundary = c("neumann", "dirichlet"), alpha = 1, 
                            dt = 0.1, eps = 1e-5, save.intermediates = F) {
  boundary <- stringr::str_to_lower(boundary)
  if (isTRUE(save.intermediates)) {
    soln <- matrix(nrow = length(init.dat), ncol = n)
    cat(dim(soln))
    soln[ , 1] <- init.dat
    if (boundary == "neumann") {
      return(one.d.heat.eq.neumann(n, init.dat, alpha, dt, eps))
    } else if (boundary == "dirichlet") {
      return(one.d.heat.eq.dirichlet(n, soln, init.dat, dt, eps))
    } else {
      stop("Sorry, I don't recognize that boundary condition yet.")
    }
  } else {
    if (boundary == "neumann") {
      return(one.d.heat.eq.neumann.memory(n, init.dat, alpha, dt, eps))
    } else if (boundary == "dirichlet") {
      return(one.d.heat.eq.dirichlet.memory(n, init.dat, init.dat, dt, eps))
    } else {
      stop("Sorry, I don't recognize that boundary condition yet.")
    }
  }
}
