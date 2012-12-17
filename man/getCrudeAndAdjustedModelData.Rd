\name{getCrudeAndAdjustedModelData}
\alias{getCrudeAndAdjustedModelData}
\title{This function helps with printing regression models}
\usage{
  getCrudeAndAdjustedModelData(fit, digits = 2, max = Inf)
}
\arguments{
  \item{fit}{The regression model}

  \item{digits}{Number of digits you want in your
  estimates}

  \item{max}{A number that specifies if any values should
  be abbreviated above this value, for instance a value of
  1000 would give a value of > 1000 for a value of 1001.
  This gives a prettier table when you have very wide
  confidence intervals.}
}
\value{
  Returns a matrix with the columns c("Crude", "95% CI",
  "Adjusted", "95% CI").  The rows are not changed from the
  original fit.
}
\description{
  This function is used for printing the adjusted and
  unadjusted values for a regression model. It takes a full
  model and walks through each variable, removes in the
  regression all variables except one then reruns that
  variable to get the unadjusted value.
}
\details{
  This function saves a lot of time creating tables since
  it compiles a fully unadjusted list of all your used
  covariates.

  If the model is an exponential logit/cox regression model
  then it automatically reports the exp() values instead of
  the original values

  The function skips by default all spline variables since
  this becomes very complicated and there is no simple
  \deqn{\beta}{beta} to display. For the same reason it
  skips any interaction variables since it's probably
  better to display these as a contrast table.
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
}

