\name{hatvalues.ols}
\alias{hatvalues.ols}
\title{Get the hat matrix for the OLS}
\usage{
  hatvalues.ols(x, ...)
}
\arguments{
  \item{x}{The ols model fit}

  \item{...}{arguments passed to methods.}
}
\value{
  vector
}
\description{
  The hat matrix comes from the residual definition:
  \deqn{\hat{\epsilon}=y-X\hat{\beta}=\{I_n-X(X'X)X'\}y=(I_n-H)y}{epsilon
  = y - Xbeta_hat=(I_n - X(X'X)X')y = (I_n - H)y} where the
  H is called the hat matrix since \deqn{Hy = \hat{y}}{Hy =
  y_hat}. The hat values are actually the diagonal elements
  of the matrix that sum up to p (the rank of X, i.e. the
  number of parameters + 1). See
  \code{\link{rms:::ols.influence}}.
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
  max
}

