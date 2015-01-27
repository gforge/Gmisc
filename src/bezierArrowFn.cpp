#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
int timesTwo(int x) {
   return x * 2;
}


/*
for (i in 2:(length(bp$x)-1)){
    lr_width <- rotateWidthAccVector(x_origo=bp$x[i],
                                     y_origo=bp$y[i],
                                     x=bp$x[i+1],
                                     y=bp$y[i+1],
                                     width=width,
                                     perpendicular=TRUE,
                                     prev_angle=lr_width$angle,
                                     default.units=default.units)
    if (length(lines$right$x) > 3){
      if (is_point_in_poly(lr_width$right, lines)){
        # Copy last point
        lr_width$right <- unit.c(tail(lines$right$x, 1),
                                 tail(lines$right$y, 1))
      }
      if (is_point_in_poly(lr_width$left, lines)){
        # Copy last point
        lr_width$left <- unit.c(tail(lines$left$x, 1),
                                   tail(lines$left$y, 1))
      }
    }
    lines <- addLineOffset(bp$x[i], bp$y[i],
      lines=lines, offset=lr_width)
  }*/


// [[Rcpp::export]]

require(inline)
getLineOffset =
  cxxfunction(
    signature(length="integer",
              x="numeric",
              y="numeric",
              width = "numeric"),
    body = '
    NumericVector xx(x);
    NumericVector yy(x);
    int no_elmnts = as<int>(length);
    NumericVector ret_xx(no_elmnts), ret_yy(no_elmnts);
    ret_xx= xx * 2;
    return( ret_xx );
    ', plugin="Rcpp")

x <- c(0.2, 0.2, 0.4, 0.4)
y <- c(0.2, 0.4, 0.4, 0.2)
tmp <- bezierGrob(x, y)
tmp <- bezierPoints(tmp)
tmp$x <- convertX(tmp$x, unitTo = "mm", valueOnly = TRUE)
tmp$y <- convertX(tmp$y, unitTo = "mm", valueOnly = TRUE)
getLineOffset(length(tmp$x), tmp$x, tmp$y, 3)
