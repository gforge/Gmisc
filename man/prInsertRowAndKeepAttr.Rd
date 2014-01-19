\name{prInsertRowAndKeepAttr}
\alias{prInsertRowAndKeepAttr}
\title{Insert a row to a matrix}
\usage{
prInsertRowAndKeepAttr(m, r, v = NA, rName = "")
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
\code{\link{prCopyAllAttribsExceptDim}}
}
\author{
max, Arne Henningsen
}

