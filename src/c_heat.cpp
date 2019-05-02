#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat(int n, Rcpp::NumericVector init, 
                           Rcpp::String boundary,
                           double alpha, double dt = 0.1,
                           double eps = 1e-5) {
  int x = init.length();
  Rcpp::NumericMatrix soln(x, n);
  soln( Rcpp::_ , 0) = init;
  if (boundary == "neumann") {
    soln = c_heat_neumann(n, soln, alpha, eps, dt, x);
  } else if (boundary == "dirichlet") {
    soln = c_heat_dirichlet(n, soln, alpha, eps, dt, x);
  } else {
    Rcpp::stop("Boundary condition not recognized.");
  }
  return soln;
}