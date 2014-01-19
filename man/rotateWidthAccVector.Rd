\name{rotateWidthAccVector}
\alias{rotateWidthAccVector}
\title{Rotates a width to match a vector}
\usage{
rotateWidthAccVector(x, y, x_origo = NA, y_origo = NA, ...)
}
\arguments{
  \item{x}{The x point of the vector}

  \item{y}{The y point of the vector}

  \item{x_origo}{The x origin if other than 0}

  \item{y_origo}{The y origin if other than 0}

  \item{...}{Passed on to \code{\link{rotateWidthAccAngle}}
  after calculating the angle from the vector.}
}
\value{
list A list with left & right vectors of c(x, y) format and
a angle element
}
\description{
This function is provided with a vector and a width. The
vector is for knowing the direction/direction and this is
used for generating an angle. It then calls the subfunction
\code{\link{rotateWidthAccAngle}}.
}
\author{
max
}

