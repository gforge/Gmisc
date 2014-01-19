\name{prGetFpDataFromFit}
\alias{prGetFpDataFromFit}
\title{Gets the confidence interval, p-values,
coefficients from a survival object}
\usage{
prGetFpDataFromFit(model_fit, conf.int = 0.95, exp = TRUE)
}
\arguments{
  \item{model_fit}{A regression fit from CRR, coxph, cph
  object}

  \item{conf.int}{The interval of interest 0-1, see levels
  in confint()}

  \item{exp}{If the value should be in exponential form
  (default)}
}
\value{
A matrix with the columns: \item{beta}{The estimated
coefficient} \item{p_val}{P-value} \item{low}{The lower
confidence interval} \item{high}{The upper confidence
interval} \item{order}{A column that later can be used in
ordering}
}
\description{
Gets the confidence interval, p-values, coefficients from a
survival object
}
\author{
max
}

