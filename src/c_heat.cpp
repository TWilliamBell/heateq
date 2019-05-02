#include <Rcpp.h>

Rcpp::NumericVector c_heat_dirichlet_memory(int n, Rcpp::NumericVector init,
                                            double alpha, double eps, double dt,
                                            int space) {
  Rcpp::NumericVector du (space);
  double lambda = (dt*alpha)/2.0;
  int i;
  int j;
  int k;
  Rcpp::NumericVector heat(Rcpp::clone(init));
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

Rcpp::NumericVector c_heat_neumann_memory(int n, Rcpp::NumericVector init,
                                          double alpha, double eps, double dt,
                                          int space) {
  Rcpp::NumericVector du (space);
  double lambda = (dt*alpha)/2.0;
  int i;
  int j;
  int k;
  Rcpp::NumericVector heat(Rcpp::clone(init));
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

Rcpp::NumericMatrix c_heat_neumann(int n, Rcpp::NumericMatrix soln,
                                   double alpha, double eps, double dt,
                                   int space) {
    Rcpp::NumericVector du (space);
    double lambda = (dt*alpha)/2.0;
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

Rcpp::NumericMatrix c_heat_dirichlet(int n, Rcpp::NumericMatrix soln,
                                     double alpha, double eps, double dt,
                                     int space) {
    Rcpp::NumericVector du (space);
    double lambda = (dt*alpha)/2.0;
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
