\name{prExtractPredictorsFromModel}
\alias{prExtractPredictorsFromModel}
\title{Get model predictors}
\usage{
prExtractPredictorsFromModel(model, check_subset = TRUE)
}
\arguments{
  \item{model}{The fitted model.}

  \item{check_subset}{Check if the model has been subsetted
  and if so subset the outcome variable too.}
}
\value{
data.frame
}
\description{
Uses the model to extract the dataset. Throws error if
unable to find the data
}
\author{
max
}

