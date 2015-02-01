#include <Rcpp.h>
using namespace Rcpp;

//' Generates a generalized Bezier line
//' 
//' This is a general form of bezier line that can be used for cubic, quadratic, 
//' and more advanced Bezier lines. 
//' 
//' @param x The x-values for the bezier control points. The first
//'  is the starting point and the last the stop point.
//' @param y The y-values for the bezier control points. The first
//'  is the starting point and the last the stop point.
//' @param length_out The length of the return points, i.e. how fine
//'  detailed the points should be.
//' 
//' @export
//' @examples
//' grid.newpage()
//' out_sizes <- 4:20
//' clrs <- colorRampPalette(c("orange", "darkblue"))(length(out_sizes))
//' for (i in out_sizes){
//'   l <- gnrlBezierPoints(x = c(.1, -.1, .7, 1, 1, 0.1), 
//'                         y = c(.9, 0, 1, .8, .4, .1), 
//'                         length_out = i)
//'   grid.lines(l$x, l$y, 
//'              gp=gpar(col=clrs[which(i == out_sizes)]))
//' }
// [[Rcpp::export]]
Rcpp::List gnrlBezierPoints(NumericVector x, NumericVector y, int length_out = 100) {
  if (length_out < 1)
    throw std::logic_error("The length.out cannot be negative");
  if (x.size() != y.size())
      throw std::range_error("The x and y elements need to be identical");
  if (x.size() < 3)
      throw std::range_error("The minimum length of the control points is 3");

  NumericVector ret_x(length_out, 0.0);
  NumericVector ret_y(length_out, 0.0);
  // We need to set the first element to the starting point
  ret_x[0] = x[0];
  ret_y[0] = y[0];
  length_out -= 1;
  
  double t = 0.0;
  for(int i = 0; i < length_out; i++){
    if (i + 1 == length_out)
      t = 1.0;
    else
      t += 1.0/double(length_out);
      
    // Now we need to sum up the elements according to the 
    // 1962 Pierre Bézier formula
    // Note that P0 -> Pn the n in the formula is actually n - 1
    int n = x.size() - 1;
    for (int ii = 0; ii <= n; ii++){
      ret_x[i + 1] += R::choose(n, ii)*
        pow(1-t, n - ii) *
        pow(t, ii) *
        x[ii];
        
      ret_y[i + 1] += R::choose(n, ii) * 
        pow(1-t, n - ii) *
        pow(t, ii) *
        y[ii];
    } 
  };

  Rcpp::List ret;
  ret["x"] = ret_x;
  ret["y"] = ret_y;
  
  return(ret);
}

