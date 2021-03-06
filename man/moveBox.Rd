% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxGrobs_move.R
\name{moveBox}
\alias{moveBox}
\title{Move a boxGrob}
\usage{
moveBox(
  element,
  x = NULL,
  y = NULL,
  space = c("absolute", "relative"),
  just = NULL
)
}
\arguments{
\item{element}{A \code{\link{boxGrob}}/\code{\link{boxPropGrob}} object.}

\item{x}{A \code{\link[grid:unit]{unit}} element or a numeric that can be converted to a \code{npc} unit object.}

\item{y}{A \code{\link[grid:unit]{unit}} element or a numeric that can be converted to a \code{npc} unit object.}

\item{space}{We can provide \code{absolute} that confers the box absolute position within the parent
\code{\link[grid:viewport]{viewport}}. If \code{relative} the movement is related to the current position.}

\item{just}{The justification of an argument as used by \code{\link[grid:viewport]{viewport}} some tiny differences:
(1) you only want to change the justification in the vertical direction you can retain the
existing justification by using \code{NA}, e.g. \code{c(NA, 'top')}, (2) if you specify only one string
and that string is either \code{top} or \code{bottom} it will assume vertical justification.}
}
\value{
The element with updated
}
\description{
Moves a \code{\link{boxGrob}}/\code{\link{boxPropGrob}} by modifying it's \code{\link[grid:viewport]{viewport}}.
This can be useful if you want to create a series of boxes whose position are relative
to each other and depend on each box's width/height.
}
\examples{
library(grid)
grid.newpage()

box <- boxGrob("A simple box", x = .5, y = .8)
moveBox(box, x = -.2, space = "relative")
}
\seealso{
Other flowchart components: 
\code{\link{align}},
\code{\link{boxGrob}()},
\code{\link{boxPropGrob}()},
\code{\link{connectGrob}()},
\code{\link{coords}()},
\code{\link{distance}()},
\code{\link{spread}}
}
\concept{flowchart components}
