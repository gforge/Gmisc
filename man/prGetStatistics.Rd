\name{prGetStatistics}
\alias{prGetStatistics}
\title{Get statistics according to the type}
\usage{
  prGetStatistics(x, show_perc = FALSE, html = TRUE,
    digits = 1, numbers_first = TRUE, show_missing = TRUE,
    continuous_fn = describeMean,
    factor_fn = describeFactors, prop_fn = factor_fn)
}
\arguments{
  \item{x}{The variable that we want the statistics for}

  \item{show_perc}{If this is a factor/proportion variable
  then we might want to show the percentages}

  \item{html}{If the output should be in html or LaTeX
  formatting}

  \item{digits}{Number of decimal digits}

  \item{numbers_first}{If number is to be prior to the
  percentage}

  \item{show_missing}{If missing should be included}

  \item{continuous_fn}{A function for describing continuous
  variables defaults to \code{\link{describeMean}}}

  \item{prop_fn}{A function for describing proportions,
  defaults to the factor function}

  \item{factor_fn}{A function for describing factors,
  defaults to \code{\link{describeFactors}}}
}
\value{
  A matrix or a vector depending on the settings
}
\description{
  A simple function applied by the
  \code{\link{getDescriptionStatsBy}} for the total column.
  This function is also used by
  \code{\link{printCrudeAndAdjustedModel}} in case of a
  basic linear regression is asked for a raw stat column
}
\author{
  max
}

