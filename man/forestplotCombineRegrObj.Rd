\name{forestplotCombineRegrObj}
\alias{forestplotCombineRegrObj}
\title{Compares different scores in different regression objects.}
\usage{
forestplotCombineRegrObj(regr.obj, variablesOfInterest.regexp,
  plot_title = NULL, clip = c(-Inf, Inf), col.zero = "lightgray",
  reference.names = NULL, rowname.fn = NULL, box.default.size = 0.5,
  xlab = NULL, xlog = FALSE, estimate.txt = NULL, zero = NULL,
  exp = TRUE, add_first_as_ref = FALSE, ref_txt = "ref.",
  ref_labels = c(), digits = 1, ...)
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

  \item{plot_title}{The title of the plot.}

  \item{clip}{If the plot confidence bands should be
  clipped}

  \item{col.zero}{The color of the vertical zero-effect
  line}

  \item{reference.names}{Additional reference names to be
  added to each model}

  \item{rowname.fn}{A function that takes a rowname and
  sees if it needs beautifying. The function has only one
  parameter the coefficients name and should return a
  string or expression.}

  \item{box.default.size}{Override the default box size
  based on precision}

  \item{xlab}{The label of the x-axis}

  \item{xlog}{If TRUE, x-axis tick marks are exponentiated}

  \item{estimate.txt}{The text of the estimate, usually HR
  for hazard ratio, OR for odds ratio}

  \item{zero}{Indicates what is zero effect. For
  survival/logistic fits the zero is 1 while in most other
  cases it's 0.}

  \item{exp}{Report in exponential form. Default true since
  the function was built for use with survival models.}

  \item{add_first_as_ref}{If you want that the first
  variable should be reference for that group of variables.
  The ref is a variable with the estimate 1 or 0 depending
  if exp() and the confidence interval 0.}

  \item{ref_labels}{If add_first_as_ref is TRUE then this
  vector is used for the model fits.}

  \item{ref_txt}{Text instead of estimate number}

  \item{digits}{Number of digits to use for the estimate
  output}

  \item{...}{Passed to forestplot2}
}
\value{
void
}
\description{
Creates a composite from different regression objects into
one forestplot where you can choose the variables of
interest to get an overview and easier comparison.
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
\seealso{
\code{\link{forestplot2}}
}

