\name{prGetModelVariables}
\alias{prGetModelVariables}
\title{Get the models variables}
\usage{
prGetModelVariables(model, remove_splines = TRUE,
  remove_interaction_vars = FALSE, add_intercept = FALSE)
}
\arguments{
  \item{model}{A model fit}

  \item{remove_splines}{If splines, etc. should be cleaned
  from the variables as these no longer are "pure"
  variables}

  \item{remove_interaction_vars}{If interaction variables
  are not interesting then these should be removed. Often
  in the case of \code{\link{printCrudeAndAdjustedModel}}
  it is impossible to properly show interaction variables
  and it's better to show these in a separate table}

  \item{add_intercept}{Adds the intercept if it exists}
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

