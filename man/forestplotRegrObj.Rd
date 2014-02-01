\name{forestplotRegrObj}
\alias{forestplotRegrObj}
\title{Foresplot for multiple models}
\usage{
forestplotRegrObj(regr.obj, title = NULL, clip = c(-Inf, Inf),
  col.zero = "lightgray", skip.variables = NULL, insertEmptyRows = NULL,
  order.regexps = NULL, order.addrows = FALSE, box.default.size = NULL,
  rowname.fn = NULL, xlab = NULL, xlog = NULL, estimate.txt = NULL,
  zero = NULL, ...)
}
\arguments{
  \item{regr.obj}{A regression model object. It should be
  of coxph, crr or glm class.  Warning: The glm is not
  fully tested.}

  \item{title}{The title of the plot. Optional}

  \item{clip}{Lower and upper limits for clipping
  confidence intervals to arrows}

  \item{col.zero}{The color of the zero effect line (HR=1)}

  \item{skip.variables}{Which variables to use. The
  variables should be the names of the fit output and not
  the true output names if you're using the
  rowname_translate_function.}

  \item{insertEmptyRows}{Add empty rows. This can either be
  a vector or a list.  When you have a vector the number
  indicates the row number where the empty row should be
  added, the format is: c(3, 5).  If you give a list you
  have the option of specifying the name of the row, the
  format is: \code{list(list(3, "my rowname"), list(5, "my
  other rowname"))}.  The rows will be added at the 3rd row
  and 5th row from the original position. Ie you don't have
  take into account that the 5:th row will be at the 6:th
  position after adding the 3rd row.}

  \item{order.regexps}{A regexp vector that searches for
  matches along the original rownames and reorders
  according to those.}

  \item{order.addrows}{If there are ordered groups then
  often you want empty rows that separate the different
  groups. Set this to true if you want to add these empty
  rows between groups.}

  \item{box.default.size}{The size of the boxes indicating
  the estimate in the forestplot. Default is the p-value.}

  \item{rowname.fn}{A function that takes a rowname and
  sees if it needs beautifying. The function has only one
  parameter the coefficients name and should return a
  string or expression.}

  \item{xlab}{The label of the x-axis}

  \item{xlog}{If TRUE, x-axis tick marks are exponentiated}

  \item{estimate.txt}{The text of the estimate, usually HR
  for hazard ratio, OR for odds ratio}

  \item{zero}{Indicates what is zero effect. For
  survival/logistic fits the zero is 1 while in most other
  cases it's 0.}

  \item{...}{Passed to \code{\link{forestplot2}}()}
}
\value{
Does not return anything
}
\description{
Plot survival objects with different lines for each
survival object. This gives a very nice overview of how
different outcomes compare but can also be used for
comparing different methods as long as they have the same
variable names.
}
\examples{
org.par <- par("ask" = TRUE)

# simulated data to test 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace=TRUE)
cov <- data.frame(
		x1 = runif(200),
		x2 = runif(200),
		x3 = runif(200))

library(rms)
dd <- datadist(cov)
options(datadist="dd")

fit1 <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data=cov)
fit2 <- cph(Surv(ftime, fstatus == 2) ~ x1 + x2 + x3, data=cov)

forestplotRegrObj (regr.obj = fit1, new_page=TRUE)

forestplotRegrObj (regr.obj = list(fit1, fit2),
                   legend = c("Status = 1", "Status = 2"), 
                   legend.title="Type of regression",
                   new_page=TRUE)
  
modifyNameFunction <- function(x){
  if (x == "x1")
    return ("Covariate A")
  
  if (x == "x2")
    return (expression(paste("My ", beta[2])))
  
  return (x)
}

forestplotRegrObj (regr.obj = list(fit1, fit2),
                   variablesOfInterest.regexp = "(x2|x3)",
                   reference.names = c("First model", "Second model"),
                   rowname.fn = modifyNameFunction, new_page=TRUE)
  
par(org.par)
}
\author{
max
}
\seealso{
\code{\link{forestplot2}}
}

