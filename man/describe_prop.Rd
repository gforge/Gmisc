\name{describe_prop}
\alias{describe_prop}
\title{A function that returns a description proportion that contains
the number and the percentage}
\usage{
  describe_prop(x, html = FALSE, digits = 1,
    number_first = TRUE)
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
}
\value{
  A string formatted for printing either latex by HTML
}
\description{
  A function that returns a description proportion that
  contains the number and the percentage
}
\examples{
describe_prop(factor(sample(50, x=c("A","B"), replace=TRUE)))
}
\author{
  max
}

