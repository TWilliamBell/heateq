#' The Heat Equation Solver
#' This function takes in all the options possible in order to choose how to solve the heat equation, the boundary conditions, and what language to use.
#' @export

one.d.heat.eq <- function(init.dat, n = 50000,
                          boundary = c("Neumann", "Dirichlet"),
                          alpha = 1, dt = 0.1, dx = 0.1, eps = 1e-5,
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
