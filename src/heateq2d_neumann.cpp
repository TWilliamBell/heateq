#include <Rcpp.h>

double von_neumann_nbhd_neumann_bound(Rcpp::NumericMatrix x,
                                                   int i, int j) {
  Rcpp::NumericVector v(4);
  for (int k = 0; k < 4; k++) {
    if (i == 0) {
      v(0) = x(i, j);
    } else {
      v(0) = x(i-1, j);
    }
    if (j == 0) {
      v(1) = x(i, j);
    } else {
      v(1) = x(i, j-1);
    }
    if (i == x.nrow()-1L) {
      v(2) = x(i, j);
    } else {
      v(2) = x(i+1, j);
    }
    if (j == x.ncol()-1L) {
      v(3) = x(i, j);
    } else {
      v(3) = x(i, j+1);
    }
  }
  return Rcpp::sum(v);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix heateq_neumann(int n, Rcpp::NumericMatrix y,
                             double c, double dt, double dx) {
  double constant = c*dt/dx;
  Rcpp::NumericMatrix x(y.nrow(), y.ncol());
  for (int i = 0; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      double neighbours =
        von_neumann_nbhd_neumann_bound(y, i, j);
      x(i, j) = y(i, j) - constant*(4*y(i, j)-neighbours);
    }
  }
  for (int k = 0; k < n-1L; k++) {
    for (int i = 0; i < x.nrow(); i++) {
      for (int j = 0; j < x.ncol(); j++) {
        double neighbours =
          von_neumann_nbhd_neumann_bound(x, i, j);
        x(i, j) = x(i, j) - constant*(4*x(i, j)-neighbours);
      }
    }
  }
  return x;
}
