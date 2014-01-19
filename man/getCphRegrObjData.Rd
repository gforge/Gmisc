\name{getCphRegrObjData}
\alias{getCphRegrObjData}
\title{Prints the cph output}
\usage{
getCphRegrObjData(fit, round = TRUE)
}
\arguments{
  \item{fit}{The cph return object}

  \item{round}{How many integers to display}
}
\value{
Returns a matrix with columns HR, 2.5%, 97.5 % and Pr(>|Z|)
}
\description{
This is a different way of outputting the cph outcome from
the rms package. I needed something a little simpler than
the general summary and therefore created this function.
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

printSimpleFit(fit)
}
\author{
max
}

