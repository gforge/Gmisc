#' Tests if function is of a logistic regression type
#' 
#' @param fit Regression object 
#' @return boolean 
#' 
#' @example examples/isFitLogit_example.R
#' 
#' @export
#' @author max
isFitLogit <- function(fit){
  if ("lrm" %in% class(fit))
    return (TRUE)
  
  if ("glm" %in% class(fit) &&
    fit$family$link == "logit")
    return (TRUE)
  
  return (FALSE)  
}
