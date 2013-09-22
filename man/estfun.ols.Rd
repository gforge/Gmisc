\name{estfun.ols}
\alias{estfun.ols}
\title{Fix for the Extract Empirical Estimating Functions}
\usage{
  estfun.ols(x, ...)
}
\arguments{
  \item{x}{A fitted ols model object.}

  \item{...}{arguments passed to methods.}
}
\value{
  matrix A matrix containing the empirical estimating
  functions.
}
\description{
  As missing data is handled a little different for the ols
  than for the lm we need to change the estfun to work with
  the ols()
}
\details{
  I have never worked with weights and this should probably
  be checked as this just uses the original estfun.lm as a
  template
}
\examples{
x1 <- runif(200)
x2 <- runif(200)
y <- x1 + x2 + rnorm(200)

f    <- ols(y ~ rcs(x1, 4) + x2)
bread(f)
vcovHC(f)

robcov_alt(f, type="HC4m")
}
\author{
  Max
}

