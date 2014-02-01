\name{insertRowAndKeepAttr}
\alias{insertRowAndKeepAttr}
\title{Insert a row into a matrix}
\usage{
insertRowAndKeepAttr(m, r, v = NA, rName = "")
}
\arguments{
  \item{m}{matrix.}

  \item{r}{row number where the new row should be inserted}

  \item{v}{optional values for the new row.}

  \item{rName}{optional character string: the name of the
  new row.}
}
\value{
a matrix with one more row than the provided matrix m.
}
\description{
Inserts a row and keeps the attributes
\code{\link{copyAllNewAttributes}}
}
\examples{
test <- matrix(1:4, ncol=2)
attr(test, 'wow') <- 1000
test <- insertRowAndKeepAttr(test, 2)
print(attr(test, 'wow'))
}
\author{
max, Arne Henningsen
}

