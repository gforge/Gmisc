#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix getNumericMatrix(SEXP& x);

//' Generates a generalized Bézier line
//'
//' This is a general form of bezier line that can be used for cubic, quadratic,
//' and more advanced Bézier lines.
//'
//' @param ctrl_points The ctrl_points for the bezier control points. This should
//'  either be a \code{matrix} or a \code{data.frame}.
//' @param length_out The length of the return points, i.e. how fine
//'  detailed the points should be.
//'
//' @export
//' @examples
//' library(grid)
//' grid.newpage()
//' l <- gnrlBezierPoints(data.frame(x = c(.1, -.1, .7, 1, 1, 0.1),
//'                                  y = c(.9, 0, 1, .8, .4, .1)),
//'                       length_out = 100)
//' grid.lines(l[,1], l[,2], gp=gpar(col="#550000", lwd = 4))
//'
//' out_sizes <- 4:20
//' clrs <- colorRampPalette(c("orange", "darkblue"))(length(out_sizes))
//' for (i in out_sizes){
//'    l <- gnrlBezierPoints(data.frame(x = c(.1, -.1, .7, 1, 1, 0.1),
//'                                     y = c(.9, 0, 1, .8, .4, .1)),
//'                          length_out = i)
//'    grid.lines(l[,1], l[,2],
//'    gp=gpar(col=clrs[which(i == out_sizes)]))
//' }
// [[Rcpp::export]]
NumericMatrix gnrlBezierPoints(SEXP& ctrl_points, int length_out = 100) {
  NumericMatrix ctrl_mtrx = getNumericMatrix(ctrl_points);

  if (length_out < 1)
    throw std::logic_error("The length.out cannot be negative");
  if (ctrl_mtrx.nrow() < 3)
      throw std::range_error("The minimum length of the control points is 3");

  NumericMatrix ret(length_out, ctrl_mtrx.ncol());

  // We need to set the first element to the starting point
  for (int col = 0; col < ctrl_mtrx.ncol(); col++){
    ret(0, col) = ctrl_mtrx(0, col);
  }
  length_out -= 1;

  double t = 0.0;
  for(int i = 0; i < length_out; i++){
    if (i + 1 == length_out) {
      t = 1.0;
    } else {
      t += 1.0/double(length_out); 
    }

    // Now we need to sum up the elements according to the
    // 1962 Pierre Bézier formula
    // Note that P0 -> Pn the n in the formula is actually n - 1
    int n = ctrl_mtrx.nrow() - 1;
    for (int ii = 0; ii <= n; ii++){
      double b_ii_n = R::choose(n, ii)*
        pow(1-t, n - ii) *
        pow(t, ii);
      for (int col = 0; col < ctrl_mtrx.ncol(); col++){
        ret(i + 1, col) +=  b_ii_n *
          ctrl_mtrx(ii, col);
      }
    }
  };


  List dmn = ctrl_mtrx.attr("dimnames");
  if (dmn.size() == 2){
    List dmn_new = Rcpp::List::create(Rcpp::CharacterVector::create(),
                                      dmn[1]);
    ret.attr("dimnames") = dmn_new;
  }

  return(ret);
}

NumericMatrix getNumericMatrix(SEXP& x){
  if( is<DataFrame>(x) ){
    NumericMatrix mx = internal::convert_using_rfunction(x, "as.matrix");
    return(mx);
  } else if (is<NumericMatrix>(x)){
    NumericMatrix mx(x);
    return(mx);
  } else {
    throw std::invalid_argument("You have provided something that is neither a data.frame or a numeric matrix");
  }
}


