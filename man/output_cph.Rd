\name{output_cph}
\alias{output_cph}
\title{Prints the cph output}
\usage{
  output_cph(fit, round = TRUE)
}
\arguments{
  \item{fit}{The cph return object}

  \item{round}{How many integers to display}
}
\value{
  Returns a matrix with columns HR, 2.5%, 97.5 % and
  Pr(>|Z|)
}
\description{
  This is a different way of outputting the cph outcome
  from the rms package. I needed something a little simpler
  than the general summary and therefore created this
  function.
}
\author{
  max
}

