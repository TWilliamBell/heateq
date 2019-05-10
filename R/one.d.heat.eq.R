#' The Heat Equation Solver
#' This function takes in all the options possible in order to choose how to solve the heat equation, and the boundary conditions.
#' @param init.dat A vector representing the evenly spaced points describing the initial state of the system.
#' @param n The number of time steps to run the simulation for.
#' @param alpha The rate constant for the heat equation, 'a' in du/dt = a d^2u/(dx^2).
#' @param boundary A string, either "Neumann" or "Dirichlet" (any capitalization allowed).
#' @param dt The length of each time step.
#' @param eps If abs(eps) > 0, then if the value of the data at some point is less than eps, it will be truncated to zero.
#' @param save.intermediates Recording the data at each time step is quite memory-intensive, but if one is interested in the evolution
#' of the system rather than just the outcome, it may be desirable to record them.  For large values MALLOC in C++ might complain and
#' shut down RStudio, in order to address this I hope to develop a rule of thumb for an average operating system (= my operating system)
#' for how large of an array should generate an error.
#' 
#' @return finalresults The state of the data on the final time step.
#' @return full If save.intermediates = T, this will be a matrix containing the data at every time step.  Time along the rows, spatial
#' dimension along the columns.
#' @return initial.data The data at the start.
#' @return n.steps The number of time steps chosen.
#' @return time.passed The amount of time passed, i.e. n*dt.
#' @return intermediates.saved Whether values other than the initial and final data were saved, necessary Boolean for the plot.heat method.
#'
#' @export

one.d.heat.eq <- function(init.dat, n = 50000,
                          boundary = c("Neumann", "Dirichlet"),
                          alpha = 1, dt = 0.1, dx = 0.1, eps = 0,
                          save.intermediates = F) {
  if (!is.scalar(alpha) | !is.scalar(dt) | !is.scalar(eps) | !is.scalar(n)) {
    stop("Some input that should be a scalar is not.")
  }
  soln <- c_one.d.heat.eq(n, init.dat, boundary,
                            alpha, dt, eps, save.intermediates, dx)
  results <- list(initial.data = init.dat,
                  n.steps = n, time.passed = n*dt,
                  intermediates.saved = save.intermediates)
  if (isFALSE(save.intermediates)) {
    results$finalresults <- soln
  }
  if (isTRUE(save.intermediates)) {
    results$full <- t(soln)
    t <- c(seq_len(n)*dt)
    rownames(results$full) <- t
    colnames(results$full) <- seq_len(nrow(soln))*dx
  }
  class(results) <- "heat"
  results
}
