\name{describe_median}
\alias{describe_median}
\title{A function that returns a description median that contains
the interquartile range or the full range}
\usage{
  describe_median(x, iqr = TRUE, html = FALSE, digits = 1,
    number_first = TRUE, show_missing = FALSE)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{iqr}{If interquartile range should be used}

  \item{digits}{The number of decimals used}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}

  \item{number_first}{If the number should be given or if
  the percentage should be presented first. The second is
  encapsulated in parentheses ().  This is only used
  together with the show_missing variable.}

  \item{show_missing}{This indicates if missing should be
  added as a separate row below all other.}
}
\value{
  A string formatted for printing either latex by HTML
}
\description{
  A function that returns a description median that
  contains the interquartile range or the full range
}
\examples{
describe_median(1:10)
describe_median(c(1:10, NA), show_missing=TRUE)
}
\author{
  max
}

