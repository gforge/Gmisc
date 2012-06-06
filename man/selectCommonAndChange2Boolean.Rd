\name{selectCommonAndChange2Boolean}
\alias{selectCommonAndChange2Boolean}
\title{A function for selecting covariates and changing the score to TRUE/FALSE}
\usage{
  selectCommonAndChange2Boolean(score, censored, min = 300,
    min_with_occurences = 10,
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
  Returns the selected columns from the score variable
}
\description{
  This function uses the select frequent scores and then
  changes the scores that are frequent into values of
  TRUE/FALSE. The scores values are usually not that
  interesting unless you merge them into a combined score
  and therefore this functions converts the score into a
  boolean value.
}
\examples{
n=1000
# Simulate a possible survival dataset
data <- data.frame(x1 = sample(c(NA, letters[1:2]), size=n, replace=TRUE),
  x2 = sample(c(rep(NA, length=30), 1:3), size=n, replace=TRUE),
  x3 = sample(c(letters[1:4]), size=n, replace=TRUE),
  fstatus = rbinom(n, 1, .2),
  ftime = runif(n)*10)

selectCommonAndChange2Boolean(data[,1:3],
  data$fstatus,
  min = 100,
  min_with_occurences = 10)


}
\author{
  max
}

