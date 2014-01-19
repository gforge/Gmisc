\name{getVectorAngle}
\alias{getVectorAngle}
\title{Gets an angle}
\usage{
getVectorAngle(x, y, x_origo = NA, y_origo = NA, default.units = "npc")
}
\arguments{
  \item{x}{The x point of the vector}

  \item{y}{The y point of the vector}

  \item{x_origo}{The x origin if other than 0}

  \item{y_origo}{The y origin if other than 0}

  \item{default.units}{As defined by the grid package}
}
\value{
angle in radians (see \code{\link{atan2}})
}
\description{
Uses a vector to get an angle by \code{\link{atan2}}.
}
\author{
max
}

