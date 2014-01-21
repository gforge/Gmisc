\name{fpColors}
\alias{fpColors}
\title{A copy of rmeta meta.colors.}
\usage{
fpColors(all.elements, box = "black", lines = "gray", summary = "black",
  zero = "lightgray", text = "black", axes = "black")
}
\arguments{
  \item{all.elements}{A color for all the elements. If set
  to NULL then it's set to the par("fg") color}

  \item{box}{The color of the box indicating the estimate}

  \item{lines}{The color of the confidence lines}

  \item{summary}{The color of the summary}

  \item{zero}{The color of the zero line}

  \item{text}{The color of the text}

  \item{axes}{The color of the x-axis at the bottom}
}
\value{
list A list with the elements: \item{box}{the color of the
box/marker} \item{lines}{the color of the lines}
\item{summary}{the color of the summary} \item{zero}{the
color of the zero vertical line} \item{text}{the color of
the text} \item{axes}{the color of the axes}
}
\description{
If you have several values per row in a plot then you can
set the values to a vector where the first value represents
the first line/box, second the second line/box etc. The
vectors are only valid for the box & lines.
}
\details{
This function is a copy of the
\code{\link[rmeta]{meta.colors}} function in the rmeta
package.
}
\author{
Max Gordon, Thomas Lumley
}

