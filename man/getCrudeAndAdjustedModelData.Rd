\name{getCrudeAndAdjustedModelData}
\alias{getCrudeAndAdjustedModelData}
\title{This function helps with printing regression models}
\usage{
  getCrudeAndAdjustedModelData(fit, digits = 2)
}
\arguments{
  \item{fit}{The regression model}

  \item{digits}{Number of digits you want in your
  estimates}
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
  \deqn{\beta}{beta} to display.
}
\examples{
# simulated data to use 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:1,200,replace=TRUE)
cov <- data.frame(
		x1 = runif(200),
		x2 = runif(200),
		x3 = runif(200))

library(rms)
fit <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data=cov)

data_matrix <- getCrudeAndAdjustedModelData(fit)

print(data_matrix)
}
\author{
  max
}

