\name{describe_factors}
\alias{describe_factors}
\title{A function that returns a description proportion that contains
the number and the percentage}
\usage{
  describe_factors(x, html = FALSE, digits = 1,
    number_first = TRUE, show_missing = FALSE)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{digits}{The number of decimals used for the
  percentage}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}

  \item{number_first}{If the number should be given or if
  the percentage should be presented first. The second is
  encapsulated in parentheses ().}

  \item{show_missing}{This indicates if missing should be
  added as a separate row below all other.}
}
\value{
  A string formatted for printing either latex by HTML
}
\description{
  A function that returns a description proportion that
  contains the number and the percentage
}
\examples{
describe_factors(sample(50, x=c("A","B", "C"), replace=TRUE))
describe_factors(sample(50, x=c("A","B", "C", NA), replace=TRUE), show_missing=TRUE)
}
\author{
  max
}

