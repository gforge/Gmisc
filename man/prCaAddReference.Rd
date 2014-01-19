\name{prCaAddReference}
\alias{prCaAddReference}
\title{Adds a reference to value matrix}
\usage{
prCaAddReference(vn, matches, available_factors, values, add_references_pos,
  reference_zero_effect, ds, use_labels)
}
\arguments{
  \item{vn}{Variable name}

  \item{matches}{Rows that match}

  \item{available_factors}{Factors that are available}

  \item{values}{The value matrix}

  \item{add_references_pos}{The position within the factors
  if it exists}

  \item{reference_zero_effect}{The reference zero effect}

  \item{ds}{The data set}

  \item{use_labels}{If labels should be used for row names}
}
\value{
\code{matrix} A matrix with rgroup and n.rgroup attributes
}
\description{
Adds a reference to value matrix
}
\author{
max
}

