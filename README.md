# heateq

A one-dimensional heat equation solver written in Rcpp for two boundary conditions.  Just a method of playing around with Rcpp.
	
	    if (!require(devtools)) {install.packages("devtools")}
	    devtools::install_github("heateq")
	
Should allow you to install it if you want to try it out, I have one master function (`one.d.heat.equation`) which gives you the opportunity to solve the heat equation using a C++ function wrapped for R written with the help of Rcpp.  You also have the option to save intermediates or not.  I've written a few basic S3 Methods for the function if you wish to have some decent defaults for plotting and printing (otherwise it just prints *everything*).

It appears to be working fine, so my next task is to improve the documentation.
