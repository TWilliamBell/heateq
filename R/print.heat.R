#' @export

print.heat <- function(x) {
  cat("Final Results:\n", head(x$final.results, n = if (x$dim == 1L) {6L} else {1L}), "   ... \n", 
      tail(x$final.results, n = if (x$dim == 1L) {6L} else {1L}))
}
