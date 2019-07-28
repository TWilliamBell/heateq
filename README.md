# heateq

A low-dimensional heat equation solver written in Rcpp for two boundary conditions.  Just a method of playing around with Rcpp.
	
	    if (!require(devtools)) {install.packages("devtools")}
	    devtools::install_github("TWilliamBell/heateq")
	
Should allow you to install it if you want to try it out, I have two master functions (`one.d.heat.equation`, and `heateq2d`) which gives you the opportunity to solve the heat equation using either C++ functions written with the help of Rcpp.  You also have the option to save intermediates or not in the one-dimensional case.  I've written a few basic S3 Methods for the function if you wish to have some decent defaults for plotting and printing (otherwise it just prints *everything*).
