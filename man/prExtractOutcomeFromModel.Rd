\name{prExtractOutcomeFromModel}
\alias{prExtractOutcomeFromModel}
\title{Get model outcome}
\usage{
prExtractOutcomeFromModel(model, check_subset = TRUE)
}
\arguments{
  \item{model}{The fitted model.1}

  \item{check_subset}{Check if the model has been subsetted
  and if so subset the outcome variable too.}
}
\value{
vector
}
\description{
Uses the model to extract the outcome variable. Throws
error if unable to find the outcome.
}
\author{
max
}

