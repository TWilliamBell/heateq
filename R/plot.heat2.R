#' Plotting for Solutions of the 2D Heat Equation
#'
#' @param heat the object of class "heat2".
#' @param ... any arguments to pass to both calls of image.
#'
#' @export
#'
#' @examples
#' plot(two.d.heat.eq(matrix(runif(10000), nrow = 100)))

plot.heat2 <- function(heat, ...) {
  par(mfrow = c(1, 2), mar = c(5, 2, 4, 0) + 0.1)
  range.val <- seq(from = if (heat$boundary == "neumann") {range(heat$initial.data)[1]} else if (heat$boundary == "dirichlet") {min(range(heat$initial.data)[1], 0)},
                   to = if (heat$boundary == "neumann") {range(heat$initial.data)[2]} else if (heat$boundary == "dirichlet") {max(range(heat$initial.data)[2], 0)},
                   length.out = 11)
  cols <- heat.colors(10)
  image(heat$initial.data, breaks = range.val, col = cols, ...)
  image(heat$final, breaks = range.val, col = cols, yaxt = "n", ...)
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
}
