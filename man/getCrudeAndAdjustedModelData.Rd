\name{getCrudeAndAdjustedModelData}
\alias{getCrudeAndAdjustedModelData}
\alias{getCrudeAndAdjustedModelData.default}
\alias{getCrudeAndAdjustedModelData.rms}
\title{This function helps with printing regression models}
\usage{
getCrudeAndAdjustedModelData(fit, level, ...)

\method{getCrudeAndAdjustedModelData}{default}(fit, level = 0.95,
  remove_interaction_vars = TRUE, ...)

\method{getCrudeAndAdjustedModelData}{rms}(fit, level = 0.95,
  remove_interaction_vars = TRUE, ...)
}
\arguments{
  \item{fit}{The regression model}

  \item{level}{The confidence interval level}

  \item{remove_interaction_vars}{Removes the interaction
  terms as this makes no sense in the output}

  \item{...}{Not used}
}
\value{
Returns a matrix with the columns: \code{c("Crude", "2.5
\%", "97.5 \%", "Adjusted", "2.5 \%", "97.5 \%")}.  The row
order is not changed from the original fit. The percentages
can vary depending on the set level.
}
\description{
This function is used for getting the adjusted and
unadjusted values for a regression model. It takes a full
model and walks through each variable, removes in the
regression all variables except one then reruns that
variable to get the unadjusted value. This functions not
intended for direct use, it's better to use
\code{\link{printCrudeAndAdjustedModel}} that utilizes this
function.
}
\details{
This function saves a lot of time creating tables since it
compiles a fully unadjusted list of all your used
covariates.

If the model is an exponential poisson/logit/cox regression
model then it automatically reports the exp() values
instead of the original values

The function skips by default all spline variables since
this becomes very complicated and there is no simple
\deqn{\beta}{beta} to display. For the same reason it skips
any interaction variables since it's probably better to
display these as a contrast table.

Note that the rms regression has a separate function that
uses the rms:::summaryrms function that returns a matrix
that is then pruned.
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
  x4 = runif(200),
  x5 = runif(200))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

data_matrix <- getCrudeAndAdjustedModelData(fit)

print(data_matrix)

# If we have interaction then those variable are not
# reported
fit <- cph(s ~ x1 + x2 + x3 + x4*x5, data=ds)
data_matrix <- getCrudeAndAdjustedModelData(fit)

print(data_matrix)
}
\author{
max

max

max
}
\seealso{
\code{\link{printCrudeAndAdjustedModel}}
}

