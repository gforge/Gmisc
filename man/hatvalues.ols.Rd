\name{hatvalues.ols}
\alias{hatvalues.ols}
\title{Get the hat matrix for the OLS}
\usage{
\method{hatvalues}{ols}(model, ...)
}
\arguments{
  \item{model}{The ols model fit}

  \item{...}{arguments passed to methods.}
}
\value{
vector
}
\description{
The hat matrix comes from the residual definition:
\deqn{\hat{\epsilon}=y-X\hat{\beta}=\{I_n-X(X'X)X'\}y=(I_n-H)y}{epsilon
= y - Xbeta_hat=(I_n - X(X'X)X')y = (I_n - H)y} where the H
is called the hat matrix since \deqn{Hy = \hat{y}}{Hy =
y_hat}. The hat values are actually the diagonal elements
of the matrix that sum up to p (the rank of X, i.e. the
number of parameters + 1). See
\code{\link[rms]{ols.influence}}.
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

