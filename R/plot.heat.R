#' @export

plot.heat <- function(x, cex = 0.01, xaxt = "n", 
                      yaxt = "n", mar = c(0, 0, 0, 0), ...) {
  if (isTRUE(x$intermediates.saved)) {
    n <- nrow(x$full)
    par(mfrow = c(2, 2), mar = mar)
    range.val <- range(x$full[1 , ])
    range.val[1] <- min(range.val[1], -0.01)
    plot(x$full[1 , ], cex = cex,
         yaxt = yaxt, xaxt = xaxt, ylim = range.val, ...)
    plot(x$full[n %/% 4 , ], cex = cex,
         yaxt = yaxt, xaxt = xaxt, ylim = range.val, ...)
    plot(x$full[n %/% 2 , ], cex = cex,
         yaxt = yaxt, xaxt = xaxt, ylim = range.val, ...)
    plot(x$full[n , ], cex = cex,
         yaxt = yaxt, xaxt = xaxt, ylim = range.val, ...)
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  } else {
    par(mfrow = c(1, 2), mar = mar)
    range.val <- range(c(x$initial.data, x$finalresults))
    range.val[1] <- min(range.val[1], -0.01)
    plot(x$initial.data, cex = cex, yaxt = yaxt, xaxt = xaxt, 
         ylim = range.val, ...)
    plot(x$finalresults, cex = cex, yaxt = yaxt, xaxt = xaxt, 
         ylim = range.val, ...)
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  }
}
