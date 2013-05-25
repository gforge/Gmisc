#' SI formatting of an integer
#' 
#' Returns a formatted large number with spaces if
#' x > 10^4. Used for nicer latex/html output that
#' is easier to convert to .doc format than the \\num{}
#' that the siunit package in LaTeX provides. 
#' 
#' @param x The integer variable 
#' @return string
#' 
#' @examples
#' outputInt(123456)
#' 
#' @export
#' @author max
outputInt <- function(x){
  return(ifelse(x >= 10^4, format(x, big.mark=" "), format(x)))  
}
