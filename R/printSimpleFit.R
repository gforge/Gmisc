#' Prints a simple fit summary for a model fit.
#' 
#' Sometimes you just want a simple fit print without all the details
#' and with only the confidence intervals. 
#' 
#' @param fit Regression model object 
#' @param digits Number of digits you want to round to
#' @return The function only prints
#' 
#' @example examples/printSimpleFit_example.R
#' 
#' @author max
#' @export
printSimpleFit <- function(fit, digits=2){
  tmp_vals <- cbind(round(coef(fit), digits), round(confint(fit), digits))
  vars <- apply(tmp_vals, 1, FUN=function(x){
      tmp <- paste(x[1], "; 95% CI ", min(x[2:3]), "-", max(x[2:3]), sep="")
      return(tmp)
    })
  
  nchar(names(vars))
  length(as.character())
  for(i in 1:length(vars)){
    cat(names(vars)[i], ":", sep="")
    if (substr(vars[i], 1, 1) == "-"){
      add <- 0
    }else{
      add <- 1
    }
    cat(rep(" ", times=max(nchar(names(vars)))-nchar(names(vars)[i])+add), sep="")
    cat(vars[i])
    cat("\n")
  }
}
