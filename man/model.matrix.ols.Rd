\name{model.matrix.ols}
\alias{model.matrix.ols}
\title{A fix for the model.matrix}
\usage{
\method{model.matrix}{ols}(object, ...)
}
\arguments{
  \item{object}{A Model}

  \item{...}{Parameters passed on}
}
\value{
matrix
}
\description{
The model.matrix.lm that the ols falls back upon "forgets"
the intercept value and behaves unreliable in the vcovHC()
funcitons. I've therefore created this subfunction to
generate the actual model.matrix() by just accessing the
formula.
}
\author{
max
}

