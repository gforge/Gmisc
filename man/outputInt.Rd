\name{outputInt}
\alias{outputInt}
\title{Returns a formatted large number with spaces if
x > 10^4. Used for nicer latex/html output that
is easier to convert to .doc format than the \\num{}
that the siunit package in LaTeX provides.}
\usage{
  outputInt(x)
}
\arguments{
  \item{x}{The integer variable}
}
\value{
  string
}
\description{
  Returns a formatted large number with spaces if x > 10^4.
  Used for nicer latex/html output that is easier to
  convert to .doc format than the \\num{} that the siunit
  package in LaTeX provides.
}
\examples{
outputInt(123456)
}
\author{
  max
}

