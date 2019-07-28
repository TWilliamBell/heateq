#' Plotting for Solutions of the 2D Heat Equation
#'
#' @param heat the object of class "heat2".
#' @param ... any arguments to pass to both calls of image.
#'
#' @export
#'
#' @examples
#' plot(heateq2d(matrix(runif(10000), nrow = 100)))

plot.heat2 <- function(heat, ...) {
  par(mfrow = c(1, 2), mar = c(5, 2, 4, 0) + 0.1)
  range.val <- seq(from = if (heat$bound == "neumann") {range(heat$init.con)[1]} else if (heat$bound == "dirichlet") {min(range(heat$init.con)[1], 0)},
                   to = if (heat$bound == "neumann") {range(heat$init.con)[2]} else if (heat$bound == "dirichlet") {max(range(heat$init.con)[2], 0)},
                   length.out = 11)
  cols <- heat.colors(10)
  image(heat$init.con, breaks = range.val, col = cols, ...)
  image(heat$final, breaks = range.val, col = cols, yaxt = "n", ...)
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}
