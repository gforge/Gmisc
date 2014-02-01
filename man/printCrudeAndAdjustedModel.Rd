\name{printCrudeAndAdjustedModel}
\alias{print.printCrudeAndAdjusted}
\alias{printCrudeAndAdjustedModel}
\title{Output crude and adjusted model data}
\usage{
printCrudeAndAdjustedModel(model, order = FALSE, digits = 2, ci_max = Inf,
  ci_min = -Inf, sprintf_ci_str = "\%s to \%s", add_references = FALSE,
  add_references_pos = list(), reference_zero_effect = NULL,
  groups = NULL, rowname.fn = NULL, use_labels = TRUE,
  desc_column = FALSE, desc_show_tot_perc = FALSE, desc_numb_first = TRUE,
  desc_continuous_fn = describeMean, desc_prop_fn = describeProp,
  desc_factor_fn = describeFactors, desc_show_missing = FALSE,
  desc_digits = digits, desc_colnames = c("Total", "Event"),
  output = "html", ...)

\method{print}{printCrudeAndAdjusted}(x, rgroupCSSstyle = "",
  rgroupCSSseparator = "", ...)
}
\arguments{
  \item{model}{A regression model}

  \item{order}{A vector with regular expressions for each
  group.}

  \item{digits}{The number of digits to round to}

  \item{ci_max}{A number that specifies if any values
  should be abbreviated above this value, for instance a
  value of 1000 would give a value of \deqn{> -1000}{>
  -1000} for a value of 1001. This gives a prettier table
  when you have very wide confidence intervals.}

  \item{ci_min}{A number that specifies if any values
  should be abbreviated above this value, for instance a
  value of -1000 would give a value of \deqn{< -1000}{<
  -1000} for a value of -1001. This gives a prettier table
  when you have very wide confidence intervals.}

  \item{sprintf_ci_str}{A string according to
  \code{\link{sprintf}} to write the confidence interval
  where the first \%s is the lower and the second the
  upper.}

  \item{add_references}{True if it should use the data set
  to look for references, otherwise supply the function
  with a vector with names. Sometimes you want to indicate
  the reference row for each group. This needs to be just
  as many as the groups as the order identified. Use NA if
  you don't want to have a reference for that particular
  group.}

  \item{add_references_pos}{The position where a reference
  should be added. Sometimes you don't want the reference
  to be at the top, for instance if you have age groups
  then you may have < 25, 25-39, 40-55, > 55 and you have
  the reference to be 25-39 then you should set the
  reference list for \code{age_groups} as
  \code{add_references_pos = list(age_groups = 2)} so that
  you have the second group as the position for the
  reference.}

  \item{reference_zero_effect}{Used with references, tells
  if zero effect is in exponential form, i.e. \code{exp(0)
  = 1}, or in regular format, i.e. \code{0 = 0} (can be set
  to any value)}

  \item{groups}{Only used together with regular expression
  for ordering and grouping.  Should be a vector with group
  names if you want to have groups to some of the
  identified order groups. If you wish to skip one just us
  NA for that instance.}

  \item{rowname.fn}{A function that takes a row name and
  sees if it needs beautifying. The function has only one
  parameter the coefficients name and should return a
  string or expression.}

  \item{use_labels}{If the rowname.fn function doesn't
  change the name then the label should be used instead of
  the name, that is if there is a label and it isn't a
  factor.}

  \item{desc_column}{Add descriptive column to the crude
  and adjusted table}

  \item{desc_show_tot_perc}{Show percentages for the total
  column}

  \item{desc_numb_first}{Whether to show the number before
  the percentages}

  \item{desc_continuous_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeMean}}}

  \item{desc_prop_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeProp}}}

  \item{desc_factor_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeFactors}}}

  \item{desc_show_missing}{Show missing variables in the
  descriptive columns}

  \item{desc_digits}{Number of digits to use in the
  descriptive columns. Defaults to the general digits if
  not specified.}

  \item{desc_colnames}{The names of the two descriptive
  columns. By default Total and Event.}

  \item{output}{Set to latex if you want latex output}

  \item{...}{Passed onto the Hmisc::\code{\link{latex}}
  function, or to the \code{\link{htmlTable}} via the print
  call}

  \item{x}{The output object from the
  printCrudeAndAdjustedModel function}

  \item{rgroupCSSstyle}{Css style for the rgorup, if
  different styles are wanted for each of the rgroups you
  can just specify a vector with the number of elements.
  Passed on to \code{\link{htmlTable}}.}

  \item{rgroupCSSseparator}{The line between different
  rgroups. The line is set to the TR element of the lower
  rgroup, i.e. you have to set the border-top/padding-top
  etc to a line with the expected function. This is only
  used for rgroups that are printed. You can specify
  different separators if you give a vector of rgroup - 1
  length (this is since the first rgroup doesn't have a
  separator). Passed on to \code{\link{htmlTable}}.}
}
\value{
\code{matrix} Returns a matrix of class
printCrudeAndAdjusted that has a default print method
associated with
}
\description{
Prints table for a fitted object. It prints by default a
latex table but can also be converted into a HTML table
that should be more compatible with common word processors.
}
\details{
A word of warning: if you call this function and you've
changed any of the variables used in the original call,
i.e. the premises are changed, this function will not
remember the original values and the statistics will be
faulty!
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

fit <- cph(s ~ x1 + x2 + x3 + x4, data=ds, x=TRUE, y=TRUE)
printCrudeAndAdjustedModel(fit, file="", add_references = TRUE, 
                           desc_column=TRUE, order=c("x3", "x4"))

# Use some labels to prettify the output
# fro the mtcars dataset
data("mtcars")

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
fit_mtcar <- lm(mpg ~ wt + gear + col, data=mtcars)
printCrudeAndAdjustedModel(fit_mtcar, file="", 
                           add_references=TRUE,
                           ctable=TRUE, 
                           desc_column = TRUE)

printCrudeAndAdjustedModel(fit_mtcar, 
                           add_references=TRUE,
                           desc_column=TRUE,
                           order=c("Interc", "gear"))

# Alterntive print - just an example, doesn't make sense to skip reference
printCrudeAndAdjustedModel(fit_mtcar, 
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

