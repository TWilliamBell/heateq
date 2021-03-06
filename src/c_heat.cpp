#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat_neumann(int n, Rcpp::NumericMatrix soln,
                                   double alpha, double eps, double dt,
                                   int space, double dx) {
    Rcpp::NumericVector du (space);
    double lambda = (dt*alpha)/(2.0*dx);
    int i;
    int j;
    int k;
    for (i = 1; i < n; i = i+1) {
        Rcpp::NumericVector heat = soln( Rcpp::_ , i-1);
        du[0] = heat[0]-heat[1];
        for (j = 1; j < space-1; j = j+1) {
            du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
        }
        du[space-1] = heat[space-1]-heat[space-2];
        for (k = 0; k <= space-1; k = k+1) {
            soln(k, i) = heat[k] - lambda*du[k];
            if (soln(k, i) < eps) {
                soln(k, i) = 0.0;
            }
        }
    }
    return soln;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat_dirichlet(int n, Rcpp::NumericMatrix soln,
                                     double alpha, double eps, double dt,
                                     int space, double dx) {
    Rcpp::NumericVector du (space);
    double lambda = (dt*alpha)/(2.0*dx);
    int i;
    int j;
    int k;
    for (i = 1; i < n; i = i+1) {
        Rcpp::NumericVector heat = soln( Rcpp::_ , i-1);
        du[0] = 2.0*heat[0]-heat[1];
        for (j = 1; j < space-1; j = j+1) {
            du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
        }
        du[space-1] = 2.0*heat[space-1]-heat[space-2];
        for (k = 0; k < space-1; k = k+1) {
            soln(k, i) = heat[k] - lambda*du[k];
            if (soln(k, i) < eps) {
                soln(k, i) = 0.0;
            }
        }
    }
    return soln;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat(int n, Rcpp::NumericVector init,
                           Rcpp::String boundary,
                           double alpha, double dt = 0.1,
                           double eps = 1e-5, double dx = 0.1) {
  int x = init.length();
  Rcpp::NumericMatrix soln(x, n);
  soln( Rcpp::_ , 0) = init;
  if (boundary == "neumann") {
    soln = c_heat_neumann(n, soln, alpha, eps, dt, x, dx);
  } else if (boundary == "dirichlet") {
    soln = c_heat_dirichlet(n, soln, alpha, eps, dt, x, dx);
  } else {
    Rcpp::stop("Boundary condition not recognized.");
  }
  return soln;
}
