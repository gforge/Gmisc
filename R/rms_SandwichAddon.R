#' Robust covariance matrix based upon the sandwich-package
#' 
#' This is an alternative to the rms-package robust covariance
#' matrix that uses the sandwich package \code{\link[sandwich]{vcovHC}} function
#' instead of the rms-built-in estimator. The advantage being that 
#' many more estimation types are available.
#'  
#' @param fit The ols fit that
#' @param type a character string specifying the estimation type. See 
#'  \code{\link[sandwich]{vcovHC}} for options. 
#' @param ... You should specify type= followed by some of the alternative available
#'  for the \code{\link[sandwich]{vcovHC}} function.
#' @return model The fitted model with adjusted variance and df.residual set to NULL
#' 
#' @example examples/rms_SandwichAddon_example.R
#' @importFrom sandwich vcovHC
#' @author max
#' @export
robcov_alt <- function (fit, type="HC3", ...) 
{
  if (!is.null(list(...)$cluster))
    stop("This function has no working implementation of the cluster option")
  
  fit$orig.var <- vcov(fit, intercepts = "all")
  fit$var <- vcovHC(fit, type=type, ...)
  # The vcovHC can't always handle the names correctly
  rownames(fit$var) <- rownames(fit$orig.var)
  colnames(fit$var) <- colnames(fit$orig.var)
  # Remove fit$df.residuals as the function then
  # wrongly uses the t-distribution instead of the normal distribution
  attr(fit, "original_degrees_of_freedom") <- fit$df.residual 
  attr(fit, "robust") <- type
  fit$df.residual <- NULL
  fit
}

#' A confint function for the ols
#' 
#' This function checks that there is a df.residual
#' before running the qt(). If not found it then
#' defaults to the qnorm() function. Otherwise it is
#' a copy of the \code{\link[stats]{confint}} function.
#' 
#' TODO: Switch to the summaryrms 
#' 
#' @param object 	a fitted ols-model object.
#' @param parm a specification of which parameters 
#'  are to be given confidence intervals, either a vector 
#'  of numbers or a vector of names. If missing, all 
#'  parameters are considered.
#' @param level the confidence level required.
#' @param ... additional argument(s) for methods.
#' @return A matrix (or vector) with columns giving lower 
#'  and upper confidence limits for each parameter. These 
#'  will be labelled as (1-level)/2 and 1 - (1-level)/2 
#'  in % (by default 2.5% and 97.5%).
#' 
#' @example examples/rms_SandwichAddon_example.R
#' @method confint ols
#' @method confint ols
#' @importFrom stats confint
#' @author max
#' @export
confint.ols <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  else if (any(!parm %in% pnames))
    stop("Could not find the parameters that you requested, could not find: ", 
      paste(pnames[!parm %in% pnames], collapse=", "),
      "in the parameter name vector:", 
      paste(pnames, collapse=", "))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  if (is.null(object$df.residual))
    zcrit <- qnorm(a)
  else
    zcrit <- qt(a, object$df.residual)
  
  pct <- stats:::format.perc(a, 3)
  ci <- array(NA, dim = c(length(parm), 2L), 
    dimnames = list(parm, pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% zcrit
  ci
}

#' Get the hat matrix for the OLS
#' 
#' The hat matrix comes from the residual definition: 
#' \deqn{\hat{\epsilon}=y-X\hat{\beta}=\{I_n-X(X'X)X'\}y=(I_n-H)y}{epsilon = y - Xbeta_hat=(I_n - X(X'X)X')y = (I_n - H)y}
#' where the H is called the hat matrix since \deqn{Hy = \hat{y}}{Hy = y_hat}. The hat
#' values are actually the diagonal elements of the matrix that sum up
#' to p (the rank of X, i.e. the number of parameters + 1). See \code{\link[rms]{ols.influence}}.
#' 
#' @param model The ols model fit
#' @param ... arguments passed to methods.
#' @return vector
#' @example examples/rms_SandwichAddon_example.R
#' 
#' @importFrom rms ols
#' @importFrom stats hatvalues
#' @method hatvalues ols
#' @S3method hatvalues ols
#' @export
#' @author max
hatvalues.ols <- function(model, ...) {
  return(ols.influence(model, ...)$hat)
}

#' Getting the bread for the vcovHC
#' 
#' The original bread.lm uses the summary.lm function
#' it seems like a quick fix and I've therefore created
#' the original bread definition: $(X'X)^-1$
#' 
#' @param x The ols model fit
#' @return matrix The bread for the sandwich vcovHC function
#' @param ... arguments passed to methods.
#' @example examples/rms_SandwichAddon_example.R
#'  
#' @importFrom rms ols
#' @importFrom sandwich bread
#' @method bread ols
#' @S3method bread ols
#' @export 
#' @author max
bread.ols <- function(x, ...)
{
  if (!inherits(x, "ols"))
    stop("You have provided a non-ols object that is not defined",
         " for this function, the classes of the object: ", 
         paste(class(x), collapse=", "))
  
  X <- model.matrix(x)
  
  return(solve(crossprod(X))*(x$rank + x$df.residual))
}

#' A fix for the model.matrix
#' 
#' The model.matrix.lm that the ols falls back upon
#' "forgets" the intercept value and behaves unreliable in
#' the vcovHC() funcitons. I've therefore created this subfunction
#' to generate the actual model.matrix() by just accessing the formula.
#'  
#' @param object A Model 
#' @param ... Parameters passed on
#' @return matrix
#' 
#' @method model.matrix ols
#' @S3method model.matrix ols
#' @importFrom rms ols
#' @importFrom stats model.matrix
#' @author max
#' @export
model.matrix.ols <- function(object, ...){
  # If the ols already has a model.matrix saved
  # then use that one but add the intercept
  if (!is.null(object$x))
    return(cbind(Intercept=rep(1, times=nrow(object$x)),
        object$x))
           
  warning("You should set the ols(..., x=TRUE) as the fallback may be somewhat unreliable")
  
  data <- prGetModelData(object)
  mtrx <- model.matrix(formula(object), data=data)
  colnames(mtrx) <- names(coef(object))
  return(mtrx)
}

#' Fix for the Extract Empirical Estimating Functions
#' 
#' As missing data is handled a little different for the ols 
#' than for the lm we need to change the estfun to work with the ols()
#'
#' I have never worked with weights and this should probably be checked
#' as this just uses the original estfun.lm as a template
#' 
#' @param x	A fitted ols model object.
#' @param ... arguments passed to methods.
#' @return matrix A matrix containing the empirical estimating functions.
#' @example examples/rms_SandwichAddon_example.R
#' 
#' @importFrom rms ols
#' @importFrom sandwich estfun
#' @author Max
#' @method estfun ols
#' @S3method estfun ols
#' @export 
estfun.ols <- function(x, ...){
  if (!inherits(x, "ols"))
    stop("You have provided a non-ols object that is not defined for this function, the classes of the object:", paste(class(x), collapse=", "))
  
  xmat <- model.matrix(x)
  xmat <- naresid(x$na.action$omit, xmat) # Modification
  x$na.action
  if (any(alias <- is.na(coef(x))))
    xmat <- xmat[, !alias, drop = FALSE]
  wts <- weights(x)
  if (is.null(wts))
    wts <- 1
  res <- na.omit(residuals(x)) # Modification
  rval <- as.vector(res) * wts * xmat
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
#   if (zoo::is.zoo(res))
#     rval <- zoo(rval, index(res), attr(res, "frequency"))
  if (is.ts(res))
    rval <- ts(rval, start = start(res), frequency = frequency(res))
  return(rval)
}
