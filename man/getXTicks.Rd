\name{getXTicks}
\alias{getXTicks}
\title{Gets the x-ticks in a formatted version. This is since I'm not always
that fond of just pretty(1:10/5). In exponential form the ticks are
determined from the 2-base, meaning that you get an intuitive feeling
for when the value is doubled.}
\usage{
  getXTicks(low, high, clip = c(-Inf, Inf), exp = FALSE)
}
\arguments{
  \item{low}{lower bound}

  \item{high}{upper bound}

  \item{clip}{if the ci are clipped}

  \item{exp}{If the value should be in exponential form
  (default)}
}
\value{
  Returns a vector with the ticks apropriate
}
\description{
  Gets the x-ticks in a formatted version. This is since
  I'm not always that fond of just pretty(1:10/5). In
  exponential form the ticks are determined from the
  2-base, meaning that you get an intuitive feeling for
  when the value is doubled.
}
\examples{
test_data <- data.frame(coef=c(2, 0.5),
  low=c(1.5, 0.05),
  high=c(3, 0.75),
  boxsize=c(0.5, 0.5))

# Exponential form where the exponent base i 2 for easier understanding
getXTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=TRUE)

# Non exponential form with using pretty
getXTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=FALSE)
}
\author{
  max
}

