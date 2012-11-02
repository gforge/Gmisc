\name{describe_mean}
\alias{describe_mean}
\title{A function that returns a description mean that contains
the standard deviation}
\usage{
  describe_mean(x, html = FALSE, digits = 1)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{digits}{The number of decimals used}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}
}
\value{
  A string formatted for printing either latex by HTML
}
\description{
  A function that returns a description mean that contains
  the standard deviation
}
\examples{
describe_mean(1:10)
}
\author{
  max
}

