\name{describeMean}
\alias{describeMean}
\title{A function that returns a description mean that contains
the standard deviation}
\usage{
describeMean(x, html = FALSE, digits = 1, number_first = TRUE,
  show_missing = FALSE, show_missing_digits = digits,
  horizontal_proportions = NULL, percentage_sign = TRUE, plusmin_str,
  languge = "en", ...)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{digits}{The number of decimals used}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}

  \item{number_first}{If the number should be given or if
  the percentage should be presented first. The second is
  encapsulated in parentheses ().  This is only used
  together with the show_missing variable.}

  \item{show_missing}{This indicates if missing should be
  added as a separate row below all other.}

  \item{show_missing_digits}{The number of digits to use
  for the missing percentage, defaults to the overall
  \code{digits}.}

  \item{horizontal_proportions}{Is only active if
  show_missing since this is the only case of a proportion
  among continuous variables. This is default NULL and
  indicates that the proportions are to be interpreted in a
  vertical manner.  If we want the data to be horizontal,
  i.e. the total should be shown and then how these differ
  in the different groups then supply the function with the
  total number in each group, i.e. if done in a by manner
  as in \code{\link{getDescriptionStatsBy}} it needs to
  provide the number before the by() command. Note! This
  calls the \code{\link{describeFactors}} since the
  horizontal interpretation looses the vertical information
  in the second category and is thus better interpreted as
  a whole.}

  \item{percentage_sign}{If you want to suppress the
  percentage sign you can set this variable to FALSE. You
  can also choose something else that the default % if you
  so wish by setting this variable. Note, this is only used
  when combined with the missing information.}

  \item{plusmin_str}{Provide if you want anything other
  than the plus minus sign suited for the given output
  format.}

  \item{language}{The ISO-639-1 two-letter code for the
  language of interest. Currently only english is
  distinguished from the ISO format using a ',' as the
  separator in the \code{\link{outputInt}} function.}

  \item{...}{Passed on to \code{\link{describeFactors}}}
}
\value{
A string formatted for printing either latex by HTML
}
\description{
A function that returns a description mean that contains
the standard deviation
}
\examples{
describeMean(1:10)
describeMean(c(1:10, NA), show_missing=TRUE)
}
\author{
max
}
\seealso{
\code{\link{getDescriptionStatsBy}}
}

