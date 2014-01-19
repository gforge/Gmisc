\name{prCopyAllAttribsExceptDim}
\alias{prCopyAllAttribsExceptDim}
\title{A simple thing to keep the attributes}
\usage{
prCopyAllAttribsExceptDim(from, to)
}
\arguments{
  \item{from}{The from object}

  \item{to}{The to object}
}
\value{
object The to object
}
\description{
Skips the dimension as this is not intended to be copied
between matrices of different sizes and not rownames etc
between data frames
}
\author{
max
}

