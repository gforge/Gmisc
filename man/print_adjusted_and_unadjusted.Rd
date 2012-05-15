\name{print_adjusted_and_unadjusted}
\alias{print_adjusted_and_unadjusted}
\title{This function helps with printing regression models}
\usage{
  print_adjusted_and_unadjusted(fit, digits = 2)
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
\author{
  max
}

