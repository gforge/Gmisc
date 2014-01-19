\name{prPushMarginViewport}
\alias{prPushMarginViewport}
\title{Pushes viewport with margins}
\usage{
prPushMarginViewport(bottom, left, top, right, name = NULL)
}
\arguments{
  \item{bottom}{The margin object, either in npc or a
  \code{\link[grid]{unit}} object}

  \item{left}{The margin object, either in npc or a
  \code{\link[grid]{unit}} object}

  \item{top}{The margin object, either in npc or a
  \code{\link[grid]{unit}} object}

  \item{right}{The margin object, either in npc or a
  \code{\link[grid]{unit}} object}

  \item{name}{The name of the last viewport}
}
\value{
\code{NULL}
}
\description{
A \code{\link[grid]{grid.layout}} object is used to
generate the margins. A second viewport selecting the
mid-row/col is used to create the effect of margins
}
\author{
max
}

