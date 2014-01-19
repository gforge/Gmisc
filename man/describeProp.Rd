\name{describeProp}
\alias{describeProp}
\title{A function that returns a description proportion that contains
the number and the percentage}
\usage{
describeProp(x, html = FALSE, digits = 1, number_first = TRUE,
  show_missing = FALSE, horizontal_proportions = NULL,
  default_ref = "First", percentage_sign = TRUE)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{digits}{The number of decimals used for the
  percentage}

  \item{html}{If HTML compatible output should be used
  instead of default LaTeX}

  \item{number_first}{If the number should be given or if
  the percentage should be presented first. The second is
  encapsulated in parentheses ().}

  \item{show_missing}{This indicates if missing should be
  added as a separate row below all other. This will always
  be converted into the describeFactor function if there is
  a missing row.}

  \item{horizontal_proportions}{This is default NULL and
  indicates that the proportions are to be interpreted in a
  vertical manner.  If we want the data to be horizontal,
  i.e. the total should be shown and then how these differ
  in the different groups then supply the function with the
  total number in each group, i.e. if done in a by manner
  as in \code{\link{getDescriptionStatsBy}} it needs to
  provide the number before the by() command. Note! This
  calls the \code{\link{describeFactors}} since the
  horizontal interpretation looses the vertical information
  in the second category and is thus better interpreted as
  a whole.}

  \item{default_ref}{If you use proportions with only one
  variable it can be useful to set the reference level that
  is of interest to show. This can wither be "First", level
  name or level number.}

  \item{percentage_sign}{If you want to suppress the
  percentage sign you can set this variable to FALSE. You
  can also choose something else that the default % if you
  so wish by setting this variable.}
}
\value{
A string formatted for printing either latex by HTML
}
\description{
A function that returns a description proportion that
contains the number and the percentage
}
\examples{
describeProp(factor(sample(50, x=c("A","B"), replace=TRUE)))
describeProp(factor(sample(50, x=c("A","B", NA), replace=TRUE)), show_missing=TRUE)
}
\author{
max
}
\seealso{
\code{\link{getDescriptionStatsBy}},
\code{\link{describeFactors}}
}

