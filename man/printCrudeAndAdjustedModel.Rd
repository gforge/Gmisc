\name{printCrudeAndAdjustedModel}
\alias{printCrudeAndAdjustedModel}
\title{Prints table for a fitted object. It prints by default a latex table but can
also be converted into a HTML table that should be more compatible with common
word processors.}
\usage{
  printCrudeAndAdjustedModel(x, order, digits = 2,
    add_references = NULL, zero_effect = 1, groups = NULL,
    rowname.fn = NULL, html = FALSE, ...)
}
\arguments{
  \item{x}{An output from the
  getCrudeAndAdjustedModelData() function or a regression
  model}

  \item{order}{A vector with regular expresions for each
  group.}

  \item{digits}{The number of digits to round to}

  \item{add_references}{A vector with names. Sometimes you
  want to indicate the reference row for each group. This
  needs to be just as many as the groups as the order
  identified. Use NA if you don't want to have a reference
  for that particular group.}

  \item{zero_effect}{Used with references, tells if zero
  effect is in exponential form, i.e. exp(0) = 1, or in
  regular format, i.e. 0 = 0 (can be set to any value)}

  \item{groups}{A vector with group names if you want to
  have groups to some of the identified order groups. If
  you wish to skip one just us NA for that instance.}

  \item{rowname.fn}{A function that takes a rowname and
  sees if it needs beautifying. The function has only one
  parameter the coefficients name and should return a
  string or expression.}

  \item{html}{If HTML output through the htmlTable should
  be used instead of traditional latex() function}

  \item{...}{Passed onto the latex function}
}
\value{
  Returns a latex formatted table
}
\description{
  Prints table for a fitted object. It prints by default a
  latex table but can also be converted into a HTML table
  that should be more compatible with common word
  processors.
}
\examples{
# simulated data to use 
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
	x1 = runif(200),
	x2 = runif(200),
	x3 = runif(200))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

printCrudeAndAdjustedModel(fit, c("x[12]", "x3"))
}
\author{
  max
}
\seealso{
  Hmisc::latex
}

