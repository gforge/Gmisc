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
  Returns a matrix with columns HR, 2.5%, 97.5 % and
  Pr(>|Z|)
}
\description{
  This is a different way of outputting the cph outcome
  from the rms package. I needed something a little simpler
  than the general summary and therefore created this
  function.
}
\examples{
# simulated data to use 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:1,200,replace=TRUE)
cov <- data.frame(
		x1 = runif(200),
		x2 = runif(200),
		x3 = runif(200))

library(rms)
fit <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data=cov)

printSimpleFit(fit)
}
\author{
  max
}

