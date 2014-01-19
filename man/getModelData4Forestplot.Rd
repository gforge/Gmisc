\name{getModelData4Forestplot}
\alias{getModelData4Forestplot}
\title{Get model data}
\usage{
getModelData4Forestplot(regr.obj, exp, variablesOfInterest.regexp, ref_labels,
  add_first_as_ref)
}
\arguments{
  \item{regr.obj}{A list with all the fits that have
  variables that are to be identified through the regular
  expression}

  \item{variablesOfInterest.regexp}{A regular expression
  identifying the variables that are of interest of
  comparing. For instance it can be "(score|index|measure)"
  that finds scores in different models that should be
  compared.}

  \item{exp}{Report in exponential form. Default true since
  the function was built for use with survival models.}

  \item{add_first_as_ref}{If you want that the first
  variable should be reference for that group of variables.
  The ref is a variable with the estimate 1 or 0 depending
  if exp() and the confidence interval 0.}

  \item{ref_labels}{If add_first_as_ref is TRUE then this
  vector is used for the model fits.}
}
\description{
A helper function for
\code{\link{forestplotCombineRegrObj}}
}
\examples{
org.par <- par("ask" = TRUE)

# simulated data to test 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:1,200,replace=TRUE)
cov <- data.frame(
  x1 = runif(200),
  x2 = runif(200),
  x3 = runif(200))

library(rms)
ddist <- datadist(cov)
options(datadist = "ddist")

fit1 <- cph(Surv(ftime, fstatus) ~ x1 + x2, data=cov)
fit2 <- cph(Surv(ftime, fstatus) ~ x1 + x3, data=cov)

forestplotCombineRegrObj (
  regr.obj = list(fit1, fit2),
  variablesOfInterest.regexp = "(x2|x3)",
  reference.names = c("First model", "Second model"),
  new_page = TRUE)

modifyNameFunction <- function(x){
  if (x == "x1")
    return ("Covariate A")
  
  if (x == "x2")
    return (expression(paste("My ", beta[2])))
  
  return (x)
}

forestplotCombineRegrObj (
  regr.obj = list(fit1, fit2),
  variablesOfInterest.regexp = "(x2|x3)",
  reference.names = c("First model", "Second model"),
  rowname.fn = modifyNameFunction,
  new_page = TRUE)

par(org.par)
}
\author{
max
}

