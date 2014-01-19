\name{prGetStatistics}
\alias{prGetStatistics}
\title{Get statistics according to the type}
\usage{
prGetStatistics(x, show_perc = FALSE, html = TRUE, digits = 1,
  numbers_first = TRUE, show_missing = TRUE, show_all_values = FALSE,
  continuous_fn = describeMean, factor_fn = describeFactors,
  prop_fn = factor_fn, percentage_sign = percentage_sign)
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

  \item{show_all_values}{This is by default false as for
  instance if there is no missing and there is only one
  variable then it is most sane to only show one option as
  the other one will just be a complement to the first. For
  instance sex - if you know gender then automatically you
  know the distribution of the other sex as it's 100 \% -
  other \%.}

  \item{continuous_fn}{A function for describing continuous
  variables defaults to \code{\link{describeMean}}}

  \item{prop_fn}{A function for describing proportions,
  defaults to the factor function}

  \item{factor_fn}{A function for describing factors,
  defaults to \code{\link{describeFactors}}}

  \item{percentage_sign}{If you want to suppress the
  percentage sign you can set this variable to FALSE. You
  can also choose something else that the default % if you
  so wish by setting this variable.}
}
\value{
A matrix or a vector depending on the settings
}
\description{
A simple function applied by the
\code{\link{getDescriptionStatsBy}} for the total column.
This function is also used by
\code{\link{printCrudeAndAdjustedModel}} in case of a basic
linear regression is asked for a raw stat column
}
\author{
max
}

