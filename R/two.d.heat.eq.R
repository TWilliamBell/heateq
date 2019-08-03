#' Solution of the 2D Heat Equation
#'
#' Given the formula dH/dt = c*(d^2H/dx^2+d^2H/dy^2), we can assign a meaning to all of the parameters.
#'
#' @param n the number of time steps.
#' @param init.dat a matrix of initial values in two dimensions.
#' @param c a conductivity constant.
#' @param dt the length of the time step.
#' @param dx the length of the spatial steps in both directions.
#' @param boundary boundary condition, either "neumann" or "dirichlet".
#'
#' @return The return value is a list of type "heat2" that includes the initial condition, final result (a matrix), and the boundary condition.
#'
#' @export
#'
#' @examples
#' two.d.heat.eq(init.dat = matrix(runif(10000), nrow = 100))
#' a <- matrix(dnorm((-49:50)/25), nrow = 100, ncol = 100)
#' mound <- a+t(a)
#' smaller.mound <- two.d.heat.eq(n = 3000, init.dat = mound)
#' plot(smaller.mound)

two.d.heat.eq <- function(n = 1000, init.dat, c = 1, dt = 0.01, dx = 0.1,
                     boundary = "neumann") {
  if (!is.scalar(alpha) | !is.scalar(dt) | !is.scalar(eps) | !is.scalar(n)) {
    stop("Some input that should be a scalar is not.")
  }
  if (boundary == "neumann") {
    final <- heateq_neumann(n = n, y = init.dat, c = c, dt = dt, dx = dx)
  } else if (boundary == "dirichlet") {
    final <- heateq_dirichlet(n = n, y = init.dat, c = c, dt = dt, dx = dx)
  } else {
    stop("Boundary condition not recognized.")
  }
  results <- list(initial.data = init.dat,
                  n.steps = n, time.passed = n*dt, boundary = boundary, dim = 2L)
  results$final.results <- final
  class(results) <- "heat"
  results
}
