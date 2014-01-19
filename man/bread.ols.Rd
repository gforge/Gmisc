\name{bread.ols}
\alias{bread.ols}
\title{Getting the bread for the vcovHC}
\usage{
\method{bread}{ols}(x, ...)
}
\arguments{
  \item{x}{The ols model fit}

  \item{...}{arguments passed to methods.}
}
\value{
matrix The bread for the sandwich vcovHC function
}
\description{
The original bread.lm uses the summary.lm function it seems
like a quick fix and I've therefore created the original
bread definition: $(X'X)^-1$
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

