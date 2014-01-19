\name{estfun.ols}
\alias{estfun.ols}
\title{Fix for the Extract Empirical Estimating Functions}
\usage{
\method{estfun}{ols}(x, ...)
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
# Generate some data
n <- 500
x1 <- runif(n)*2
x2 <- runif(n)
y <- x1^3 + x2 + rnorm(n)

dd <- datadist(x1, x2, y)
org.op <- options(datadist = "dd")

# Main function
f    <- ols(y ~ rcs(x1, 3) + x2)

# Check the bread
bread(f)
# Check the HC-matrix
vcovHC(f, type="HC4m")
# Adjust the model so that it uses the HC4m variance
f_rob <- robcov_alt(f, type="HC4m")
# Get the new HC4m-matrix
# - this function just returns the f_rob$var matrix
vcov(f_rob)
# Now check the confidence interval for the function
confint(f_rob)

options(org.op)
}
\author{
Max
}

