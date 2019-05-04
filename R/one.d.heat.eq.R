#' The Heat Equation Solver
#' This function takes in all the options possible in order to choose how to solve the heat equation, the boundary conditions, and what language to use.
#' @export

one.d.heat.eq <- function(init.dat, n = 50000,
                          boundary = c("Neumann", "Dirichlet"),
                          alpha = 1, dt = 0.1, eps = 1e-5, cpp = T,
                          save.intermediates = F, dx = NULL) {
  if (!is.scalar(alpha) | !is.scalar(dt) | !is.scalar(eps) | !is.scalar(n)) {
    stop("Some input that should be a scalar is not.")
  }
  if (isTRUE(cpp)) {
    soln <- c_one.d.heat.eq(n, init.dat, boundary,
                            alpha, dt, eps, save.intermediates)
  } else {
    cat("We're this far.")
    soln <- r_one.d.heat.eq(n, init.dat, boundary,
                            alpha, dt, eps, save.intermediates)
  }
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
    if (!is.null(dx)) {
      colnames(results$full) <- seq_len(nrow(soln))*dx
    }
  }
  class(results) <- "heat"
  results
}
