#' @export

plot.heat <- function(x, cex = 0.01, xaxt = "n", 
                      yaxt = "n", mar = c(0, 0, 0, 0), ...) {
  if (x$dim == 1L) {
    if (isTRUE(x$intermediates.saved)) {
      n <- ncol(x$full)
      par(mfrow = c(2, 2), mar = mar)
      range.val <- range(x$full[1 , ])
      range.val[1] <- min(range.val[1], -0.01)
      plot(x$full[1 , ], cex = 0.01,
           yaxt = "n", xaxt = "n", ylim = range.val, ...)
      plot(x$full[n %/% 4 , ], cex = 0.01,
           yaxt = "n", xaxt = "n", ylim = range.val, ...)
      plot(x$full[n %/% 2 , ], cex = 0.01,
           yaxt = "n", xaxt = "n", ylim = range.val, ...)
      plot(x$full[n , ], cex = 0.01,
           yaxt = "n", xaxt = "n", ylim = range.val, ...)
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
    } else {
      par(mfrow = c(1, 2), mar = mar)
      range.val <- range(c(x$initial.data, x$final.results))
      range.val[1] <- min(range.val[1], -0.01)
      plot(x$initial.data, cex = 0.01, yaxt = "n", xaxt = "n", 
           ylim = range.val, ...)
      plot(x$final.results, cex = 0.01, yaxt = "n", xaxt = "n", 
           ylim = range.val, ...)
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
    }
  } else if (x$dim == 2L) {
      par(mfrow = c(1, 2), mar = c(5, 2, 4, 0) + 0.1)
      range.val <- seq(from = if (x$boundary == "neumann") {range(x$initial.data)[1]} else if (x$boundary == "dirichlet") {min(range(x$initial.data)[1], 0)},
                   to = if (x$boundary == "neumann") {range(x$initial.data)[2]} else if (x$boundary == "dirichlet") {max(range(x$initial.data)[2], 0)},
                   length.out = 11)
      cols <- heat.colors(10)
      image(x$initial.data, breaks = range.val, col = cols, ...)
      image(x$final.results, breaks = range.val, col = cols, yaxt = "n", ...)
      par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  } else {
      stop("No plot method for given dimension.")
  }
}
