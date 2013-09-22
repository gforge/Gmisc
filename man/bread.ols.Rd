\name{bread.ols}
\alias{bread.ols}
\title{Getting the bread for the vcovHC}
\usage{
  bread.ols(x, ...)
}
\arguments{
  \item{x}{The ols model fit}

  \item{...}{arguments passed to methods.}
}
\value{
  matrix The bread for the sandwich vcovHC function
}
\description{
  The original bread.lm uses the summary.lm function it
  seems like a quick fix and I've therefore created the
  original bread definition: $(X'X)^-1$
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

