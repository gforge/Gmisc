\name{printSimpleFit}
\alias{printSimpleFit}
\title{Prints a simple fit summary for a model fit.}
\usage{
printSimpleFit(fit, digits = 2)
}
\arguments{
  \item{fit}{Regression model object}

  \item{digits}{Number of digits you want to round to}
}
\value{
The function only prints
}
\description{
Sometimes you just want a simple fit print without all the
details and with only the confidence intervals.
}
\examples{
# simulated data to use 
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x1 = runif(200),
  x2 = runif(200),
  x3 = runif(200))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

getCphRegrObjData(fit)
}
\author{
max
}

