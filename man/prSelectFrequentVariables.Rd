\name{prSelectFrequentVariables}
\alias{prSelectFrequentVariables}
\title{A functions for selecting the scores that have a certain frequency}
\usage{
  prSelectFrequentVariables(score, censored, min,
    min_with_occurences = NULL,
    censored_indicator = "reoperation")
}
\arguments{
  \item{score}{A matrix/df with all the columns being
  scores that are checked}

  \item{censored}{This is the censored/outcome vector. If a
  score occurs not so frequently but it is _very_ strongly
  related to the outcome the you have a less strict
  selection. This selection is the min_with_occurrences}

  \item{min}{The minimum of times that the score may occur
  and still be interesting.  Note this is NOT a percentage
  since it may be interesting to have very low percentages
  if your material is big.}

  \item{min_with_occurences}{A smaller number than min
  since this gives an option to select more rare indicators
  if they're strongly associated with the outcome}

  \item{censored_indicator}{The indicator for the censored
  vector that indicates that an event/outcome has occurred}
}
\value{
  Returns a vector with the column names that are to be
  kept, colnames(score)[keepers]
}
\description{
  Used to remove for instance HIV from Charlsons,
  Elixhausers and other rare scores that probably don't
  have any predictive value due to their rareness.
}
\author{
  max
}

