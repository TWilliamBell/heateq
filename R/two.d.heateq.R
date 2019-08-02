#' Solution of the 2D Heat Equation
#'
#' Given the formula dH/dt = c*(d^2H/dx^2+d^2H/dy^2), we can assign a meaning to all of the parameters.
#'
#' @param n the number of time steps.
#' @param init a matrix of initial values in two dimensions.
#' @param c a conductivity constant.
#' @param dt the length of the time step.
#' @param dx the length of the spatial steps in both directions.
#' @param bound boundary condition, either "neumann" or "dirichlet".
#'
#' @return The return value is a list of type "heat2" that includes the initial condition, final result (a matrix), and the boundary condition.
#'
#' @export
#'
#' @examples
#' heateq2d(matrix(runif(10000), nrow = 100))

two.d.heateq <- function(n = 1000, init, c = 1, dt = 0.01, dx = 0.1,
                     bound = "neumann") {
  if (bound == "neumann") {
    final <- heateq_neumann(n = n, y = init, c = c, dt = dt, dx = dx)
  } else if (bound == "dirichlet") {
    final <- heateq_dirichlet(n = n, y = init, c = c, dt = dt, dx = dx)
  } else {
    stop("Boundary condition not recognized.")
  }
  results <- list(initial.data = init.dat,
                  n.steps = n, time.passed = n*dt)
  results$final <- final
  class(results) <- "heat2"
  results
}
