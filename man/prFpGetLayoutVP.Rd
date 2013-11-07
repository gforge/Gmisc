\name{prFpGetLayoutVP}
\alias{prFpGetLayoutVP}
\title{Get the main foresplot}
\usage{
  prFpGetLayoutVP(lineheight, marList, labels, nr,
    legend_layout = NULL)
}
\arguments{
  \item{lineheight}{The line height}

  \item{marList}{The margin list}

  \item{labels}{The labels}

  \item{nr}{Number of rows}

  \item{legend_layout}{A legend layout object if
  applicable}
}
\value{
  \code{viewport} Returns the viewport needed
}
\description{
  The layout makes space for a legend if needed
}
\author{
  max
}

