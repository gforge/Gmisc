#include <Rcpp.h>
using namespace Rcpp;

//' Gets offsetted lines
//' 
//' The function calculates new points according to the offset
//' that lie to the left/right of the provided line.
//' 
//' @param x A numeric vector containing all the x-elements
//' @param y A numeric vector containing all the y-elements
//' @param offset The offset to add to the line, can be a vector if you
//'  want to use different offsets.
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
                                  NumericVector offset,
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
  double angle, dy, dx, offs = 0;
  for(int i=0; i < x.size(); i++){
    offs = offset[i % offset.size()];
    
    // If last element then use the arrow end 
    // or the previous element instead of the y[i + 1] 
    if (i + 1 == x.size()){
      if (arrow_offset > 1){
        dy = (end_y - y[i]);
        dx = (end_x - x[i]);
      }else{
        dy = (y[i] - y[i - 1]);
        dx = (x[i] - x[i - 1]);
      }
    }else{
      dy = (y[i+1] - y[i]);
      dx = (x[i+1] - x[i]);
    }

    // See if the dx == 0 then we can be more exact than the cos/sin
    // functions allow for
    // Note that FLT_EPSILON is used on purpose instead of DBL_EPSILON
    // as I want to catch values that are slighlty larger than the DBL_EPSILON
    // and graphics don't really need the double
    if (fabs(dx) < FLT_EPSILON){
      // If there is no change in x then the line is
      // 1. Vertical
      // 2. Non-existant
      left_y[i] = y[i];
      right_y[i] = y[i];
      if (fabs(dy) < FLT_EPSILON){
        dy = y[y.size()] - y[1];
        if (fabs(dy) < FLT_EPSILON)
          throw std::length_error("There must be either a change in y or x");
      }
      if (dy > 0){
        angle = -PI/2;
        
        left_x[i] = -offs + x[i];
        right_x[i] = offs + x[i];
        continue;
      }else if (dy < 0){
        angle = -PI/2;
        
        left_x[i] = offs + x[i];
        right_x[i] = -offs + x[i];
        continue;
      }
    }else if (fabs(dy) < FLT_EPSILON){
      if (fabs(dx) < FLT_EPSILON){
        dx = x[x.size()] - x[1];
        if (fabs(dx) < FLT_EPSILON)
          throw std::length_error("There must be either a change in y or x");
      }
      
      left_x[i] = x[i];
      right_x[i] = x[i];
      if (dx < 0){
        angle = PI;
        left_y[i] = -offs + y[i];
        right_y[i] = offs + y[i];
        continue;
      }else{
        angle = 0;
        left_y[i] = offs + y[i];
        right_y[i] = -offs + y[i];
        continue;
      }
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
    
    right_x[i] = offs * cos(angle -  PI/2) + x[i];
    right_y[i] = offs * sin(angle -  PI/2) + y[i];
    left_x[i] = offs * cos(angle +  PI/2) + x[i];
    left_y[i] = offs * sin(angle +  PI/2) + y[i];
  }
  
  // Add the arrow if requested
  if (arrow_offset > 0){
    // Same angle as last angle
    left_x[x.size()] = arrow_offset * cos(angle +  PI/2) + x[x.size() - 1];
    left_y[x.size()] = arrow_offset * sin(angle +  PI/2) + y[x.size() - 1];

    right_x[x.size()] = arrow_offset * cos(angle -  PI/2) + x[x.size() - 1];
    right_y[x.size()] = arrow_offset * sin(angle -  PI/2) + y[x.size() - 1];
    
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