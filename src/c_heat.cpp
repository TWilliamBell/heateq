#include <Rcpp.h>

double absf(double x) { // Sorry, avoiding including the entire cmath library for simple floating point absolute values.
  if (x < 0) { 
    return -x; 
  } else {
    return x;
  }
}

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat_neumann(int n, Rcpp::NumericMatrix soln, // Solution at each time step is stored in a matrix, this
                                   // grows fast for a large number of time steps or initial data.  Can cause memory allocation errors
                                   // (i.e. you run out of RAM).
                                   double alpha, double eps, double dt,
                                   int space, double dx) {
    Rcpp::NumericVector du (space); // This will be the vector containing the (spatial) second differences of the previous time step.
    double lambda = (dt*alpha)/(2.0*dx); // This is the rate constant for how much the second differences change the result.
    int i; int j; int k;
    for (i = 1; i < n; i = i+1) {
        Rcpp::NumericVector heat = soln( Rcpp::_ , i-1); // At each time step, a column of the solution matrix is made from
                                                         // the solution at the previous time step.
        du[0] = heat[0]-heat[1]; // This is the left second differences given a Neumann boundary condition (i.e. du/dt = 0).
        for (j = 1; j < space-1; j = j+1) { // This is the second differences in the interior of the region.
            du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
        }
        du[space-1] = heat[space-1]-heat[space-2]; //This is the right second differences given a Neumann boundary condition.
        for (k = 0; k <= space-1; k = k+1) {
            soln(k, i) = heat[k] - lambda*du[k]; // Updates the solution based on the second differences and our rate constant.
            if (absf(soln(k, i)) < eps) { // Given a value a given distance from zero, this can be rounded to zero,
              // could represent measurement error, eps = 0 is obviously an option.
                soln(k, i) = 0.0;
            }
        }
    }
    return soln;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix c_heat_dirichlet(int n, Rcpp::NumericMatrix soln, // Mostly the same as previous.
                                     double alpha, double eps, double dt,
                                     int space, double dx) {
    Rcpp::NumericVector du (space);
    double lambda = (dt*alpha)/(2.0*dx);
    int i; int j; int k;
    for (i = 1; i < n; i = i+1) {
        Rcpp::NumericVector heat = soln( Rcpp::_ , i-1);
        du[0] = 2.0*heat[0]-heat[1]; // Second differences at the left boundary given the Dirichlet boundary condition (u = 0)
        for (j = 1; j < space-1; j = j+1) {
            du[j] = 2.0*heat[j]-heat[j-1]-heat[j+1];
        }
        du[space-1] = 2.0*heat[space-1]-heat[space-2]; // Second differences at the right boundary given the Dirichlet boundary 
                                                      // condition (u = 0)
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
  if (boundary == "neumann") { // Did you choose a Neumann boundary?
    soln = c_heat_neumann(n, soln, alpha, eps, dt, x, dx);
  } else if (boundary == "dirichlet") { // Did you choose a Dirichlet boundary?
    soln = c_heat_dirichlet(n, soln, alpha, eps, dt, x, dx);
  } else {
    Rcpp::stop("Boundary condition not recognized.");
  }
  return soln;
}
