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
  Sometimes you just want a simple fit print without all
  the details and with only the confidence intervals.
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

getCphRegrObjData(fit)
}
\author{
  max
}

