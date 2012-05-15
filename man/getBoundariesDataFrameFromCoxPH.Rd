\name{getBoundariesDataFrameFromCoxPH}
\alias{getBoundariesDataFrameFromCoxPH}
\title{Gets the boundaries for a fit}
\usage{
  getBoundariesDataFrameFromCoxPH(coxph.fit,
    conf.int = 0.95)
}
\arguments{
  \item{coxph.fit}{A cox regression model}

  \item{conf.int}{The confidence interval 0-1}
}
\value{
  A frame with all the data
}
\description{
  TODO: Change the confidence interval calculation to
  confint()
}
\author{
  max
}

