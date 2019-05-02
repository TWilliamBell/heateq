#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector c_heat_memory(int n, Rcpp::NumericVector init, 
                                  Rcpp::String boundary,
                                  double alpha, double dt = 0.1,
                                  double eps = 1e-5) {
  int x = init.length();
  if (boundary == "neumann") {
    return c_heat_neumann_memory(n, init, alpha, eps, dt, x);
  } else if (boundary == "dirichlet") {
    return c_heat_dirichlet_memory(n, init, alpha, eps, dt, x);
  } else {
    Rcpp::stop("Boundary condition not recognized.");
  }
}
