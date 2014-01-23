\name{prCaGetOrderVariables}
\alias{prCaGetOrderVariables}
\title{Re-order variables}
\usage{
prCaGetOrderVariables(names, order, ok2skip = FALSE)
}
\arguments{
  \item{names}{The names of the variables}

  \item{order}{The order regular expression}

  \item{ok2skip}{If you have the intercept then it should
  be ok for the function to skip that variable if it isn't
  found among the variable list}
}
\value{
\code{vector} A vector containing the greps
}
\description{
Re-order variables
}
\author{
max
}

