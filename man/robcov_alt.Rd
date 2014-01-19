\name{robcov_alt}
\alias{robcov_alt}
\title{Robust covariance matrix based upon the sandwich-package}
\usage{
robcov_alt(fit, type = "HC3", ...)
}
\arguments{
  \item{fit}{The ols fit that}

  \item{type}{a character string specifying the estimation
  type. See \code{\link[sandwich]{vcovHC}} for options.}

  \item{...}{You should specify type= followed by some of
  the alternative available for the
  \code{\link[sandwich]{vcovHC}} function.}
}
\value{
model The fitted model with adjusted variance and
df.residual set to NULL
}
\description{
This is an alternative to the rms-package robust covariance
matrix that uses the sandwich package
\code{\link[sandwich]{vcovHC}} function instead of the
rms-built-in estimator. The advantage being that many more
estimation types are available.
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

