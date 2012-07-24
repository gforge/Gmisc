\name{getModelData4Forestplot}
\alias{getModelData4Forestplot}
\title{Gets the x-ticks in a formatted version. This is since I'm not always
that fond of just pretty(1:10/5). In exponential form the ticks are
determined from the 2-base, meaning that you get an intuitive feeling
for when the value is doubled.}
\usage{
  getModelData4Forestplot(regr.obj, exp,
    variablesOfInterest.regexp, ref_labels,
    add_first_as_ref)
}
\arguments{
  \item{regr.obj}{A list with all the fits that have
  variables that are to be identified through the regular
  expression}

  \item{variablesOfInterest.regexp}{A regular expression
  identifying the variables that are of interest of
  comparing. For instance it can be "(score|index|measure)"
  that finds scores in different models that should be
  compared.}

  \item{exp}{Report in exponential form. Default true since
  the function was built for use with survival models.}

  \item{add_first_as_ref}{If you want that the first
  variable should be reference for that group of variables.
  The ref is a variable with the estimate 1 or 0 depending
  if exp() and the confidence interval 0.}

  \item{ref_labels}{If add_first_as_ref is TRUE then this
  vector is used for the model fits.}
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
getTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=TRUE)

# Non exponential form with using pretty
getTicks(low = test_data$low, 
  high = test_data$high, 
  clip=c(-Inf, Inf), 
  exp=FALSE)


# A very simple example
getTicks(1:5*2.33, 
  exp=FALSE)

# A slightly more advanced exponential version
getTicks(1:10*.33, 
  digits=2,
  exp=TRUE)
}
\author{
  max
}

