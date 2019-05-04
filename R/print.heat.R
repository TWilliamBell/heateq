#' @export

print.heat <- function(x) {
  cat("Final Results:\n", head(x$finalresults), "   ... \n", tail(x$finalresults))
}
