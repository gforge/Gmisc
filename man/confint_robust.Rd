\name{confint_robust}
\alias{confint_robust}
\title{The confint function adapted for vcovHC}
\usage{
confint_robust(object, parm, level = 0.95, HC_type = "HC3",
  t_distribution = FALSE, ...)
}
\arguments{
  \item{object}{The regression model object, either an ols
  or lm object}

  \item{parm}{The names or number of the coefficients of
  interest}

  \item{level}{Level of confidence interval, defaults to
  0.95}

  \item{HC_type}{See options for
  \code{\link[sandwich]{vcovHC}}}

  \item{t_distribution}{A boolean for if the t-distribution
  should be used or not. Defaults to FALSE.}

  \item{...}{Additional parameters that are passed on.}
}
\value{
a matrix with the confidence intervals
}
\description{
The confint.lm uses the t-distribution as the default
confidence interval estimator. According to Cribari-Nieto's
study 2008 this should be the normal distribution. This is
also supported by my own resampling study with
heteroscedastic outcomes.
}
\examples{
n <- 50
x <- runif(n)
y <- x + rnorm(n)

fit <- lm(y~x)
confint_robust(fit, HC_type = "HC4m")
}
\author{
Max
}

