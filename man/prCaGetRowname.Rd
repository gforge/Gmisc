\name{prCaGetRowname}
\alias{prCaGetRowname}
\title{Gets the labelled rowname if it exists}
\usage{
prCaGetRowname(vn, use_labels, dataset)
}
\arguments{
  \item{vn}{The variable name}

  \item{use_labels}{If labels should be used}

  \item{dataset}{The dataset}
}
\value{
\code{string} The rowname
}
\description{
Looks for matches inside factors if rowname contains the
name of the column. Capitalizes the name.
}
\author{
max
}

