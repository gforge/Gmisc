\name{prCaPrepareCrudeAndAdjusted}
\alias{prCaPrepareCrudeAndAdjusted}
\title{Prettify the text}
\usage{
prCaPrepareCrudeAndAdjusted(x, output, ci_max, ci_min, digits, sprintf_ci_str)
}
\arguments{
  \item{x}{The value matrix from getCrudeAndAdjusted}

  \item{output}{The type of output}

  \item{ci_max}{The max confidence interval}

  \item{ci_min}{The min confidence interval}

  \item{digits}{The number of decimal digits to use}

  \item{sprintf_ci_str}{The \code{\link{sprintf}} code for
  the confidence interval}
}
\value{
\code{matrix} A string matrix with the values formated
}
\description{
Sets the number of digits, formats the confidence interval
and changes the number of cols into 4 where the upper and
lower CI meet in one string column
}
\author{
max
}

