#' Tests if function is of a cox regression type.
#' 
#' Counts competing risk regression as a coxph
#' 
#' @param fit Regression object
#' @return boolean
#' 
#' @export 
#' @example examples/isFitCoxPH_example.R
isFitCoxPH <- function(fit){
  if ("coxph" %in% class(fit))
    return (TRUE)
  
  if ("crr" %in% class(fit))
    return (TRUE)
  
  return (FALSE)
}
