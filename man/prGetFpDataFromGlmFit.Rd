\name{prGetFpDataFromGlmFit}
\alias{prGetFpDataFromGlmFit}
\title{Gets the boundaries for a GLM fit that is poisson or quasipoisson based}
\usage{
prGetFpDataFromGlmFit(glm.fit, conf.int = 0.95, exp = TRUE)
}
\arguments{
  \item{glm.fit}{A regression model}

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
Gets the boundaries for a GLM fit that is poisson or
quasipoisson based
}
\author{
max
}

