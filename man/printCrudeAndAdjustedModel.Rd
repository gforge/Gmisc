\name{printCrudeAndAdjustedModel}
\alias{printCrudeAndAdjustedModel}
\title{Output crude and adjusted model data}
\usage{
  printCrudeAndAdjustedModel(model, order = FALSE,
    digits = 2, max = Inf, min = -Inf,
    sprintf_ci_str = "\%s to \%s", add_references = FALSE,
    reference_zero_effect = 1, groups = NULL,
    rowname.fn = NULL, use_labels = TRUE,
    output = c("html", "latex", "raw"), ...)
}
\arguments{
  \item{model}{A regression model}

  \item{order}{A vector with regular expressions for each
  group.}

  \item{digits}{The number of digits to round to}

  \item{max}{A number that specifies if any values should
  be abbreviated above this value, for instance a value of
  1000 would give a value of $> 1000$ for a value of 1001.
  This gives a prettier table when you have very wide
  confidence intervals.}

  \item{min}{A number that specifies if any values should
  be abbreviated above this value, for instance a value of
  -1000 would give a value of $< -1000$ for a value of
  -1001. This gives a prettier table when you have very
  wide confidence intervals.}

  \item{sprintf_ci_str}{A string according to
  \code{\link{sprintf}} to write the confidence interval
  where the first \%s is the lower and the second the
  upper.}

  \item{add_references}{True if it should use the dataset
  to look for references, otherwise supply the function
  with a vector with names. Sometimes you want to indicate
  the reference row for each group. This needs to be just
  as many as the groups as the order identified. Use NA if
  you don't want to have a reference for that particular
  group.}

  \item{reference_zero_effect}{Used with references, tells
  if zero effect is in exponential form, i.e. exp(0) = 1,
  or in regular format, i.e. 0 = 0 (can be set to any
  value)}

  \item{groups}{Only used together with regular expression
  for ordering and grouping.  Should be a vector with group
  names if you want to have groups to some of the
  identified order groups. If you wish to skip one just us
  NA for that instance.}

  \item{rowname.fn}{A function that takes a rowname and
  sees if it needs beautifying. The function has only one
  parameter the coefficients name and should return a
  string or expression.}

  \item{use_labels}{If the rowname.fn function doesn't
  change the name then the label should be used instead of
  the name, that is if there is a label and it isn't a
  factor.}

  \item{output}{Choose the type of output that you want
  returned, html, latex or raw.  The raw alternative is a
  list with the arguments that would be sent to the
  latex/htmlTable functions, where x is the main content of
  the table.}

  \item{...}{Passed onto the Hmisc::\code{\link{latex}}
  function, \code{\link{htmlTable}}}
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
	x3 = runif(200),
  x4 = factor(sample(LETTERS[1:4], size=200, replace=TRUE)))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

printCrudeAndAdjustedModel(fit, c("x[12]", "x3"), file="")

fit <- cph(s ~ x1 + x2 + x3 + x4, data=ds)
printCrudeAndAdjustedModel(fit, file="")

# Use some labels to prettify the output
# fro the mtcars dataset
data(mtcars)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10^3 kg" # not sure the unit is correct 

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

require(splines)
fit <- lm(mpg ~ bs(wt, 3) + gear + col, data=mtcars)
printCrudeAndAdjustedModel(fit, file="", add_references=TRUE, ctable=TRUE)

# Alterntive print - just an example, doesn't make sense to skip reference
printCrudeAndAdjustedModel(fit, 
                           file="", 
                           order=c("col", "gear"), 
                           groups=c("Color", "Gears"),
                           add_references=c("Black", NA),
                           ctable=TRUE,
                           output = "html")
}
\author{
  max
}

