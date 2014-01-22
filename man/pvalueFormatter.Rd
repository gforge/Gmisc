\name{pvalueFormatter}
\alias{pvalueFormatter}
\title{Formats the p-values}
\usage{
pvalueFormatter(pvalues, two_dec.limit = 10^-2, sig.limit = 10^-4,
  html = TRUE)
}
\arguments{
  \item{pvalues}{The p-values}

  \item{two_dec.limit}{The limit for showing two decimals}

  \item{sig.limit}{The significance limit for < sign}

  \item{html}{If the less than sign should be < or &lt; as
  needed for html output.}
}
\value{
vector
}
\description{
Gets formatted p-values. For instance you often want 0.1234
to be 0.12 while also having two values up until a limit,
i.e. 0.01234 should be 0.012 while 0.001234 should be
0.001. Furthermore you want to have < 0.001 as it becomes
ridiculous to report anything below that value.
}
\examples{
pvalueFormatter(c(0.10234,0.010234, 0.0010234, 0.000010234))
}
\author{
Max
}

