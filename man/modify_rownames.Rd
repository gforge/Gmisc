\name{modify_rownames}
\alias{modify_rownames}
\title{Change names of the columns}
\usage{
  modify_rownames(rn)
}
\arguments{
  \item{rn}{A vector with strings repreesenting row names}
}
\value{
  Returns a list with all the names. This is due to that
  some names are changed into expressions, mostly to get
  the >= in a pretty way
}
\description{
  Used for switching the names for the variables to
  something more sensible than the default, usually not so
  pretty names.
}
\details{
  TODO: start using label
}
\author{
  max
}

