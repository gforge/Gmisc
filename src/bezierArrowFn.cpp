//[[Rcpp::depends(Gmisc)]]

#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

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
int test(){
  return(2);
}

//' Gets offsetted lines
//' 
//' The function calculates new points according to the offset
//' that lie to the left/right of the provided line.
//' 
//' @param x A numeric vector containing all the x-elements
//' @param y A numeric vector containing all the y-elements
//' @param offset The offset to add to the line
//' @param end_x The x end of the line where the arrow occurrs (if < 0 arrow is skipped)
//' @param end_y The y end of the line where the arrow occurrs (if < 0 arrow is skipped)
//' @param arrow_offset The offset to add to the arrow section if any (if <= 0 arrow is skipped)
//' @return \code{list(list(x = ..., y = ...))} Returns a list with the right/left 
//'  lines that in turn lists with \emph{x} and \emph{y} elements
//' @useDynLib Gmisc
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
Rcpp::List calculateLinesAndArrow(NumericVector x, 
    NumericVector y, 
    double offset,
    double end_x = -1,
    double end_y = -1,
    double arrow_offset = -1){
  
  if (x.size() != y.size())
    throw std::length_error("The two vectors must have the same length");

  if ((end_y < 0 || end_x < 0 || arrow_offset <= 0) &&
      (end_y >= 0 || end_x >= 0 || arrow_offset > 0))
    throw std::logic_error("Some of your arrow variables have invalid values");
  
  int vlen = x.size();
  if (arrow_offset > 0)
    vlen += 2;
    
  NumericVector left_x(vlen);
  NumericVector left_y(vlen);
  NumericVector right_x(vlen);
  NumericVector right_y(vlen);
  double angle = 0;
  
  double dy = 0;
  double dx = 0;
  double left_angle, right_angle;
  for( int i=0; i<x.size(); i++){
    if (i + 1 < x.size() ||
        (arrow_offset > 1 &&
        i + 1 == x.size())){
      // If last element then use the arrow end instead ot the y[i + 1] that would
      // index outside the elements
      if (i + 1 == x.size()){
        dy = (end_y - y[i]);
        dx = (end_x - x[i]);
      }else{
        dy = (y[i+1] - y[i]);
        dx = (x[i+1] - x[i]);
      }
      if (dx == 0){
        if (dy > 0)
          angle = PI;
        else if (dy < 0)
          angle = -PI;
        else
          angle = 0;
      }else{
        angle = atan(dy/dx);
      }
        
      if (dy > 0 && dx < 0){
        // Upper left quadrant
        angle -= PI;
      }else if (dy <= 0 && dx < 0){
        // Lower left quadrant
        angle += PI;
      }
    }

    left_angle = angle +  PI/2;
    left_x[i] = offset * cos(left_angle) + x[i];
    left_y[i] = offset * sin(left_angle) + y[i];

    right_angle = angle -  PI/2;
    right_x[i] = offset * cos(right_angle) + x[i];
    right_y[i] = offset * sin(right_angle) + y[i];
  }
  
  // Add the arrow if requested
  if (arrow_offset > 0){
    // Same angle as last angle
    left_x[x.size()] = arrow_offset * cos(left_angle) + x[x.size() - 1];
    left_y[x.size()] = arrow_offset * sin(left_angle) + y[x.size() - 1];

    right_x[x.size()] = arrow_offset * cos(right_angle) + x[x.size() - 1];
    right_y[x.size()] = arrow_offset * sin(right_angle) + y[x.size() - 1];
    
    right_x[x.size() + 1] = end_x;
    left_x[x.size() + 1] = end_x;
    right_y[x.size() + 1] = end_y;
    left_y[x.size() + 1] = end_y;
  }
  
  Rcpp::List right;
  Rcpp::List left;
  left["x"] = left_x;
  left["y"] = left_y;
  right["x"] = right_x;
  right["y"] = right_y;

  Rcpp::List lines;
  lines["right"] = right;
  lines["left"] = left;
  
  return lines;
}