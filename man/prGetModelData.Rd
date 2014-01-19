\name{prGetModelData}
\alias{prGetModelData}
\title{Get model data.frame}
\usage{
prGetModelData(x, check_subset = TRUE)
}
\arguments{
  \item{x}{The fitted model.}

  \item{check_subset}{Check if the model has been subsetted
  and if so subset the outcome variable too.}
}
\value{
data.frame

TODO: Check if this cannot be replaced by model.frame()
}
\description{
Combines the \code{\link{prExtractPredictorsFromModel}}
with the \code{\link{prExtractOutcomeFromModel}} to
generate a full model dataset
}
\author{
max
}

