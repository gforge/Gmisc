\name{confint.ols}
\alias{confint.ols}
\title{A confint function for the ols}
\usage{
\method{confint}{ols}(object, parm, level = 0.95, ...)
}
\arguments{
  \item{object}{a fitted ols-model object.}

  \item{parm}{a specification of which parameters are to be
  given confidence intervals, either a vector of numbers or
  a vector of names. If missing, all parameters are
  considered.}

  \item{level}{the confidence level required.}

  \item{...}{additional argument(s) for methods.}
}
\value{
A matrix (or vector) with columns giving lower and upper
confidence limits for each parameter. These will be
labelled as (1-level)/2 and 1 - (1-level)/2 in % (by
default 2.5% and 97.5%).
}
\description{
This function checks that there is a df.residual before
running the qt(). If not found it then defaults to the
qnorm() function. Otherwise it is a copy of the
\code{\link[stats]{confint}} function.
}
\details{
TODO: Switch to the summaryrms
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
max
}

