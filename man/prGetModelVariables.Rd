\name{prGetModelVariables}
\alias{prGetModelVariables}
\title{Get the models variables}
\usage{
  prGetModelVariables(model, remove_splines = TRUE)
}
\arguments{
  \item{model}{A model fit}

  \item{remove_splines}{If splines, etc. should be cleaned
  from the variables as these no longer are "pure"
  variables}
}
\value{
  vector with names
}
\description{
  This function extract the modelled variables. Any
  interaction terms are removed as those should already be
  represented by the individual terms.
}
\author{
  max
}

