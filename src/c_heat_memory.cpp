#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector c_heat_dirichlet_memory(int n, Rcpp::NumericVector init,
                                            double alpha, double eps, double dt,
                                            int space, double dx) {
  Rcpp::NumericVector du (space);
  double lambda = (dt*alpha)/(2.0*dx);
  int i;
  int j;
  int k;
  Rcpp::NumericVector heat (space);
  du[0] = 2.0*init[0]-init[1];
  for (j = 1; j < space-1; j = j+1) {
    du[j] = 2.0*init[j]-init[j-1]-init[j+1];
  }
  du[space-1] = 2.0*init[space-1]-init[space-2];
  for (k = 0; k <= space-1; k = k+1) {
    heat[k] = init[k] - lambda*du[k];
    if (heat[k] < eps) {
        heat[k] = 0.0;
    }
  }
  for (i = 1; i < n; i = i+1) {
    du[0] = 2.0*heat[0]-heat[1];
    for (j = 1; j < space-1; j = j+1) {
      du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
    }
    du[space-1] = 2.0*heat[space-1]-heat[space-2];
    for (k = 0; k <= space-1; k = k+1) {
      heat[k] = heat[k] - lambda*du[k];
      if (heat[k] < eps) {
        heat[k] = 0.0;
      }
    }
  }
  return heat;
}

// [[Rcpp::export]]
Rcpp::NumericVector c_heat_neumann_memory(int n, Rcpp::NumericVector init,
                                          double alpha, double eps, double dt,
                                          int space, double dx) {
  Rcpp::NumericVector du (space);
  double lambda = (dt*alpha)/(2.0*dx);
  int i;
  int j;
  int k;
  Rcpp::NumericVector heat (space);
  du[0] = init[0]-init[1];
  for (j = 1; j < space-1; j = j+1) {
    du[j] = 2.0*init[j]-init[j-1]-init[j+1];
  }
  du[space-1] = init[space-1]-init[space-2];
  for (k = 0; k <= space-1; k = k+1) {
    heat[k] = init[k] - lambda*du[k];
    if (heat[k] < eps) {
        heat[k] = 0.0;
    }
  }
  for (i = 1; i < n; i = i+1) {
    du[0] = heat[0]-heat[1];
    for (j = 1; j < space-1; j = j+1) {
      du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
    }
    du[space-1] = heat[space-1]-heat[space-2];
    for (k = 0; k <= space-1; k = k+1) {
      heat[k] = heat[k] - lambda*du[k];
      if (heat[k] < eps) {
        heat[k] = 0.0;
      }
    }
  }
  return heat;
}

// [[Rcpp::export]]
Rcpp::NumericVector c_heat_memory(int n, Rcpp::NumericVector init,
                                  Rcpp::String boundary,
                                  double alpha, double dt = 0.1,
                                  double eps = 1e-5, double dx = 0.1) {
  int x = init.length();
  if (boundary == "neumann") {
    return c_heat_neumann_memory(n, init, alpha, eps, dt, x, dx);
  } else if (boundary == "dirichlet") {
    return c_heat_dirichlet_memory(n, init, alpha, eps, dt, x, dx);
  } else {
    Rcpp::stop("Boundary condition not recognized.");
  }
}
