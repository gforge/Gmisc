#include <Rcpp.h>
#include <Gmisc.h>
using namespace Rcpp;


Point generatePoint(double offs,
                    double offs_delta,
                    double perpendicular_angle,
                    double x,
                    double y,
                    Point last,
                    Point delta,
                    int i){
  double cos_val = cos(perpendicular_angle);
  double sin_val = sin(perpendicular_angle);

  Point p;
  p.x = offs * cos_val + x;
  p.y = offs * sin_val + y;
  p.problematic = false;

  // Strange things happen around 0
  Point offset_delta = { (p.x - offs_delta * cos_val - last.x),
                         (p.y - offs_delta * sin_val - last.y) };
  if (fabs(offset_delta.x) < SINGLE_EPS)
    offset_delta.x = 0;
  if (fabs(offset_delta.y) < SINGLE_EPS)
    offset_delta.y = 0;
  
  // Notify that element goes in wrong direction
  if (i > 1 &&
      (delta.x * offset_delta.x < -fabs(offs_delta) ||
      delta.y * offset_delta.y < -fabs(offs_delta))){
    p.problematic = true;
    /* Nightmare to debug ....
    Rcout << "i = " << i << " offset delta = " << offs_delta
          << std::setprecision(1)
          << " X = " << delta.x  << ":" << offset_delta.x << " -> " << delta.x * offset_delta.x
          << " Y = " << delta.y  << ":" << offset_delta.y << " -> " << delta.y * offset_delta.y
          << std::endl; */
  }
  return(p);
}

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
//' @param rm_intersect Set to 0 if you want to skip intersection removal, 1 only to remove left or
//'  2 to only remove right. See details for why.
//'
//'  @section Remove intersections:
//'
//'  When the line is wide and the arrow has a narrow curve there may appear an empty triangle due
//'  to polygon cancellation (two polygons within the same are cancel out). This behaviour may be
//'  ugly and the function therefor tries to remove these.
//'
//'  \emph{Note:} it is expensive to check if there are the lineas may intersect at one point,
//'  remove those unexpected, and then adjust the line to the new situation so that the
//'  top and bottom lines match. It can also cause some unexpected behaviour why you may want to
//'  remove this feature if the arrow behaves erratically.
//'
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
                                  double arrow_offset = -1,
                                  int rm_intersect = 3){

  if (x.size() != y.size())
    throw std::length_error("The two vectors must have the same length");

  if ((end_y < 0 || end_x < 0 || arrow_offset <= 0) &&
      (end_y >= 0 || end_x >= 0 || arrow_offset > 0))
    throw std::logic_error("Some of your arrow variables have invalid values");

  int vlen = x.size();
  if (arrow_offset > 0)
    vlen += 2;

  Line left(vlen);
  Line right(vlen);

  double angle, offs, offs_delta;
  angle = 0; // Only to avoid compiler warning
  Point delta = {0, 0, false}, last_right = {0, 0, false}, last_left = {0, 0, false};
  for(int i=0; i < x.size(); i++){
    offs = offset[i % offset.size()];
    if (i == 0){
      offs_delta = 0;
    }else{
      offs_delta = offset[i % offset.size()] - offset[(i-1) % offset.size()];
    }

    // If last element then use the arrow end
    // or the previous element instead of the y[i + 1]
    if (i + 1 == x.size()){
      if (arrow_offset > 1){
        delta.y = (end_y - y[i]);
        delta.x = (end_x - x[i]);
      }else{
        delta.y = (y[i] - y[i - 1]);
        delta.x = (x[i] - x[i - 1]);
      }
    }else{
      // If the same point appears twice it should skip to the
      // next point
      int ii = i;
      do{
        delta.y = (y[ii+1] - y[ii]);
        delta.x = (x[ii+1] - x[ii]);
        ii++;
      }while(ii < x.size() &&
           fabs(delta.x) < SINGLE_EPS &&
           fabs(delta.y) < SINGLE_EPS);
    }

    // Strange things happen around 0
    if (fabs(delta.x) < SINGLE_EPS)
      delta.x = 0;
    if (fabs(delta.y) < SINGLE_EPS)
      delta.y = 0;

    angle = atan2(delta.y, delta.x);

    Point p = generatePoint(offs,
                      offs_delta,
                      angle -  PI/2,
                      x[i],
                      y[i],
                      last_right,
                      delta,
                      i);
    right.addPoint(p);
    last_right = p;

    p = generatePoint(offs,
                      offs_delta,
                      angle +  PI/2,
                      x[i],
                      y[i],
                      last_left,
                      delta,
                      i);

    left.addPoint(p);
    last_left = p;
  }

  // Add the arrow if requested
  if (arrow_offset > 0){
    // Same angle as last angle
    left.addPoint(arrow_offset * cos(angle +  PI/2) + x[x.size() - 1],
                  arrow_offset * sin(angle +  PI/2) + y[x.size() - 1],
                  false);
    left.addPoint(end_x,
                  end_y,
                  false);

    right.addPoint(arrow_offset * cos(angle -  PI/2) + x[x.size() - 1],
                   arrow_offset * sin(angle -  PI/2) + y[x.size() - 1],
                   false);
    right.addPoint(end_x,
                   end_y,
                   false);
  }

  if (rm_intersect !=0){
    if (rm_intersect == 1 ||
        rm_intersect == 3)
      left.removeIntersections();
    if (rm_intersect == 2 ||
        rm_intersect == 3)
      right.removeIntersections();
  }

  Rcpp::List right_list;
  Rcpp::List left_list;
  left_list["x"] = as<NumericVector>(wrap(left.getX()));
  left_list["y"] = as<NumericVector>(wrap(left.getY()));
  left_list["prblm"] = as<NumericVector>(wrap(left.getProblematic()));

  right_list["x"] = as<NumericVector>(wrap(right.getX()));
  right_list["y"] = as<NumericVector>(wrap(right.getY()));

  Rcpp::List lines;
  lines["right"] = right_list;
  lines["left"] = left_list;

  return lines;
}

