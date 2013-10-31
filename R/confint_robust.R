
#' The confint function adapted for vcovHC
#' 
#' The confint.lm uses the t-distribution as the default
#' confidence interval estimator. According to Cribari-Nieto's
#' study 2008 this should be the normal distribution. This
#' is also supported by my own resampling study with heteroscedastic
#' outcomes.
#'   
#' @param object The regression model object, either an ols or lm object 
#' @param parm The names or number of the coefficients of interest
#' @param level Level of confidence interval, defaults to 0.95
#' @param HC_type See options for \code{\link[sandwich]{vcovHC}}
#' @param t_distribution A boolean for if the t-distribution should be used
#'  or not. Defaults to FALSE.
#' @param ... Additional parameters that are passed on.
#' @return a matrix with the confidence intervals
#' 
#' @example examples/confint_robust_example.R
#' 
#' @author Max
#' @export
confint_robust <- function(object, parm, level = 0.95, 
    HC_type="HC3", t_distribution = FALSE,...){
  cf <- coef(object); pnames <- names(cf)
  if(missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  
  a <- (1-level)/2; a <- c(a, 1-a)
  pct <- stats:::format.perc(a, 3)
  if (t_distribution)
    fac <- qt(a, object$df.residual)
  else
    fac <- qnorm(a)
  ci <- array(NA, 
      dim = c(length(parm), 2L), 
      dimnames = list(parm, pct))
  ses <- sqrt(diag(vcovHC(object, type=HC_type)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}
