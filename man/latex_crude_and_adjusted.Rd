\name{latex_crude_and_adjusted}
\alias{latex_crude_and_adjusted}
\title{Prints latex table for a fitted object}
\usage{
  latex_crude_and_adjusted(x, order, digits = 2,
    add_references = NULL, groups = NULL, caption = NULL,
    ctable = TRUE)
}
\arguments{
  \item{x}{An output from the
  print_adjusted_and_unadjusted() function or a regression
  model if you want to call the function from within}

  \item{order}{A vector with regular expresions for each
  group.}

  \item{add_references}{A vector with names. Sometimes you
  want to indicate the reference row for each group. This
  needs to be just as many as the groups as the order
  identified. Use NA if you don't want to have a reference
  for that particular group.}

  \item{groups}{A vector with group names if you want to
  have groups to some of the identified order groups. If
  you wish to skip one just us NA for that instance.}

  \item{caption}{The table caption}

  \item{ctable}{If the latex is to be in ctable format}
}
\value{
  Returns a latex formatted table
}
\description{
  Prints latex table for a fitted object
}
\author{
  max
}

