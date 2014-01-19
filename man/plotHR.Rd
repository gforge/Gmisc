\name{plotHR}
\alias{par}
\alias{plotHR}
\title{Plot a spline in a Cox regression model}
\usage{
plotHR(models, term = 1, se = TRUE, polygon_ci = TRUE, rug = "density",
  xlab = "", ylab = "Hazard Ratio", main = NULL, xlim = NULL,
  ylim = NULL, col.term = "#08519C", col.se = "#DEEBF7",
  col.dens = grey(0.9), lwd.term = 3, lty.term = 1, lwd.se = lwd.term,
  lty.se = lty.term, x.ticks = NULL, y.ticks = NULL, ylog = TRUE,
  cex = 1, y_axis_side = 2, plot.bty = "n", axes = TRUE, alpha = 0.05,
  ...)
}
\arguments{
  \item{models}{A single model or a list() with several
  models}

  \item{term}{The term of interest. Can be either the name
  or the number of the covariate in the model.}

  \item{se}{Boolean if you want the confidence intervals or
  not}

  \item{polygon_ci}{If you want a polygon as indicator for
  your confidence interval.  This can also be in the form
  of a vector if you have several models. Sometimes you
  only want one model to have a polygon and the rest to be
  dotted lines. This gives the reader an indication of
  which model is important.}

  \item{rug}{The rug is the density of the population along
  the spline variable. Often this is displayed as a jitter
  with bars that are thicker & more common when there are
  more observations in that area or a smooth density plot
  that looks like a mountain. Use "density" for the
  mountain view and "ticks" for the jitter format.}

  \item{xlab}{The label of the x-axis}

  \item{ylab}{The label of the y-axis}

  \item{main}{The main title of the plot}

  \item{xlim}{A vector with 2 elements containing the upper
  & the lower bound of the x-axis}

  \item{ylim}{A vector with 2 elements containing the upper
  & the lower bound of the y-axis}

  \item{col.term}{The color of the estimate line. If
  multiple lines you can have different colors by giving a
  vector.}

  \item{col.se}{The color of the confidence interval. If
  multiple lines you can have different colors by giving a
  vector.}

  \item{col.dens}{The color of the density plot. Ignored if
  you're using jitter}

  \item{lwd.term}{The width of the estimated line. If you
  have more than one model then provide the function with a
  vector if you want to have different lines for different
  width for each model.}

  \item{lty.term}{The typeof the estimated line, see lty.
  If you have more than one model then provide the function
  with a vector if you want to have different line types
  for for each model.}

  \item{lwd.se}{The line width of your confidence interval.
  This is ignored if you're using polygons for all the
  confidence intervals.}

  \item{lty.se}{The line type of your confidence interval.
  This is ignored if you're using polygons for all the
  confidence intervals.}

  \item{x.ticks}{The ticks for the x-axis if you desire
  other than the default.}

  \item{y.ticks}{The ticks for the y-axis if you desire
  other than the default.}

  \item{ylog}{Show a logarithmic y-axis. Not having a
  logarithmic axis might seem easier to understand but it's
  actually not really a good idea. The distance between HR
  0.5 and 2.0 should be the same. This will only show on a
  logarithmic scale and therefore it is strongly
  recommended to use the logarithmic scale.}

  \item{cex}{Increase if you want larger font size in the
  graph.}

  \item{plot.bty}{Type of box that you want. See the bty
  description in graphical parameters (par) If bty is one
  of "o" (the default), "l", "7", "c", "u", or "]" the
  resulting box resembles the corresponding upper case
  letter. A value of "n" suppresses the box.}

  \item{y_axis_side}{The side that the y axis is to be
  plotted, see axis() for details}

  \item{axes}{A boolean that is used to identify if axes
  are to be plotted}

  \item{alpha}{The alpha level for the confidence
  intervals}

  \item{...}{Any additional values that are to be sent to
  the plot() function}
}
\value{
The function does not return anything
}
\description{
This function is a more specialized version of the
\code{\link{termplot}()} function. It creates a simple plot
with the spline against hazard ratio.
}
\details{
The function allows for plotting multiple splines in one
graph. Sometimes you might want to show more than one
spline for the same variable. This allows you to create
that comparison.

Examples of a situation where I've used multiple splines in
one plot is when I want to look at a variables behavior in
different time periods. This is another way of looking at
the proportional hazards assumption. The Schoenfeld
residuals can be a little tricky to look at when you have
the splines.

Another example of when I've used this is when I've wanted
to plot adjusted and unadjusted splines. This can very
nicely demonstrate which of the variable span is mostly
confounded. For instance - younger persons may exhibit a
higher risk for a procedure but when you put in your
covariates you find that the increased hazard changes back
to the basic
}
\examples{
library(survival)
library(rms)

# Get data for example
n <- 1000
set.seed(731)

age <- round(50 + 12*rnorm(n), 1)
label(age) <- "Age"

sex <- factor(sample(c('Male','Female'), n, 
    rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)

smoking <- factor(sample(c('Yes','No'), n, 
    rep=TRUE, prob=c(.2, .75)))

h <- .02*exp(.02*(age-50)+.1*((age-50)/10)^3+.8*(sex=='Female')+2*(smoking=='Yes'))
dt <- -log(runif(n))/h
label(dt) <- 'Follow-up Time'

e <- ifelse(dt <= cens,1,0)
dt <- pmin(dt, cens)
units(dt) <- "Year"

# Add missing data to smoking
smoking[sample(1:n, round(n*0.05))] <- NA

# Create a data frame since plotHR will otherwise
# have a hard time getting the names of the variables
ds <- data.frame(
  dt = dt,
  e = e,
  age=age, 
  smoking=smoking,
  sex=sex)

Srv <- Surv(dt,e)
fit.coxph <- coxph(Srv ~ bs(age, 3) + sex + smoking, data=ds)

org_par <- par(xaxs="i", ask=TRUE)
plotHR(fit.coxph, term="age", plot.bty="o", xlim=c(30, 70), xlab="Age")

dd <- datadist(ds)
options(datadist='dd')
fit.cph <- cph(Srv ~ rcs(age,4) + sex + smoking, data=ds, x=TRUE, y=TRUE)

plotHR(fit.cph, term=1, plot.bty="l", xlim=c(30, 70), xlab="Age")

plotHR(fit.cph, term="age", plot.bty="l", xlim=c(30, 70), ylog=FALSE, rug="ticks", xlab="Age")

unadjusted_fit <- cph(Srv ~ rcs(age,4), data=ds, x=TRUE, y=TRUE)
plotHR(list(fit.cph, unadjusted_fit), term="age", xlab="Age",
		polygon_ci=c(TRUE, FALSE), 
		col.term = c("#08519C", "#77777799"),
		col.se = c("#DEEBF7BB", grey(0.6)),
		lty.term = c(1, 2),
		plot.bty="l", xlim=c(30, 70))
par(org_par)
}
\author{
Reinhard Seifert, Max Gordon
}
\references{
\url{http://rforge.org/plothr/}
}

