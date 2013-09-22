#' Robust covariance matrix based upon the sandwich-package
#' 
#' This is an alternative to the rms-package robust covariance
#' matrix that uses the sandwich package \code{\link{sandwich:::vcovHC}} function
#' instead of the rms-built-in estimator. The advantage being that 
#' many more estimation types are available.
#'  
#' @param fit 
#' @param ... You should specify type= followed by some of the alternative available
#'  for the \code{\link{sandwich:::vcovHC}} function.
#' @return model The fitted model with adjusted variance and df.residual set to NULL
#' 
#' @example examples/rms_SandwichAddon_example.R
#' @import sandwich
#' @author max
#' @export
robcov_alt <- function (fit, ...) 
{
  if (!is.null(list(...)$cluster))
    stop("This function has no working implementation of the cluster option")
  
  fit$orig.var <- vcov(fit, intercepts = "all")
  fit$var <- vcovHC(fit, ...)
  # The vcovHC can't always handle the names correctly
  rownames(fit$var) <- rownames(fit$orig.var)
  colnames(fit$var) <- colnames(fit$orig.var)
  # Remove fit$df.residuals as the function then
  # wrongly uses the t-distribution instead of the normal distribution
  fit$df.residual <- NULL
  fit
}

#' Get the hat matrix for the OLS
#' 
#' The hat matrix comes from the residual definition: 
#' \deqn{\hat{\epsilon}=y-X\hat{\beta}=\{I_n-X(X'X)X'\}y=(I_n-H)y}{epsilon = y - Xbeta_hat=(I_n - X(X'X)X')y = (I_n - H)y}
#' where the H is called the hat matrix since \deqn{Hy = \hat{y}}{Hy = y_hat}. The hat
#' values are actually the diagonal elements of the matrix that sum up
#' to p (the rank of X, i.e. the number of parameters + 1). See \code{\link{rms:::ols.influence}}.
#' 
#' @param x The ols model fit
#' @param ... arguments passed to methods.
#' @return vector
#' @example examples/rms_SandwichAddon_example.R
#' 
#' @import rms
#' @export
#' @author max
hatvalues.ols <- function(x, ...) {
  if (inherits(x, "ols"))
    return(ols.influence(x, ...)$hat)
  else
    stop("You have provided a non-ols object that is not defined for this function, the classes of the object:", paste(class(x), collapse=", "))
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
#' @import rms
#' @export 
#' @author max
bread.ols <- function(x, ...)
{
  if (!inherits(x, "ols"))
    stop("You have provided a non-ols object that is not defined for this function, the classes of the object:", paste(class(x), collapse=", "))
  
  X <- model.matrix(x)
  return(solve(crossprod(X))*(x$rank + x$df.residual))
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
#' @import rms
#' @author Max
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
  if (is.zoo(res))
    rval <- zoo(rval, index(res), attr(res, "frequency"))
  if (is.ts(res))
    rval <- ts(rval, start = start(res), frequency = frequency(res))
  return(rval)
}
