\name{prFpGetGraphTicksAndClips}
\alias{prFpGetGraphTicksAndClips}
\title{A helper function to forestplot2}
\usage{
  prFpGetGraphTicksAndClips(xticks, xticks.digits, xlog,
    xlab, lwd.xaxis, col, cex, clip, zero, x_range, nc)
}
\arguments{
  \item{xticks}{The xticks}

  \item{xticks.digits}{Number of digits for the xticks}

  \item{xlog}{If the axis should be log()}

  \item{xlab}{The x-axis label}

  \item{lwd.xaxis}{The line width of the x-axis}

  \item{col}{The color object}

  \item{cex}{The text size}

  \item{clip}{The clip margins}

  \item{zero}{The zero effect}

  \item{x_range}{The range that values span}

  \item{nc}{Number of columns}
}
\value{
  list Returns a list with axis_vp, axisGrob, labGrob, zero
  and clip
}
\description{
  Gets the x-label and zero-bar details
}
\author{
  Max
}

