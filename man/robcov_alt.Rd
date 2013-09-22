\name{robcov_alt}
\alias{robcov_alt}
\title{Robust covariance matrix based upon the sandwich-package}
\usage{
  robcov_alt(fit, ...)
}
\arguments{
  \item{fit}{}

  \item{...}{You should specify type= followed by some of
  the alternative available for the
  \code{\link{sandwich:::vcovHC}} function.}
}
\value{
  model The fitted model with adjusted variance and
  df.residual set to NULL
}
\description{
  This is an alternative to the rms-package robust
  covariance matrix that uses the sandwich package
  \code{\link{sandwich:::vcovHC}} function instead of the
  rms-built-in estimator. The advantage being that many
  more estimation types are available.
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

