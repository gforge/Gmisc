\name{prCaGetVnStats}
\alias{prCaGetVnStats}
\title{Gets the variable stats}
\usage{
  prCaGetVnStats(model, vn, outcome, output, ds,
    add_references, desc_digits, desc_continuous_fn,
    desc_prop_fn, desc_factor_fn, desc_show_missing,
    desc_show_tot_perc)
}
\arguments{
  \item{model}{The model}

  \item{vn}{The variable name}

  \item{outcome}{The outcome vector}

  \item{output}{Choose the type of output that you want
  returned, html, latex or raw.  The raw alternative is a
  list with the arguments that would be sent to the
  latex/htmlTable functions, where x is the main content of
  the table.}

  \item{ds}{The dataset}

  \item{add_references}{True if it should use the dataset
  to look for references, otherwise supply the function
  with a vector with names. Sometimes you want to indicate
  the reference row for each group. This needs to be just
  as many as the groups as the order identified. Use NA if
  you don't want to have a reference for that particular
  group.}

  \item{desc_column}{Add descriptive column to the crude
  and adjusted table}

  \item{desc_show_tot_perc}{Show percentages for the total
  column}

  \item{desc_numb_first}{Whether to show the number before
  the percentages}

  \item{desc_continuous_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeMean}}}

  \item{desc_prop_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeProp}}}

  \item{desc_factor_fn}{Stat function used for the
  descriptive statistics, defaults to
  \code{\link{describeFactors}}}

  \item{desc_show_missing}{Show missing variables in the
  descriptive columns}

  \item{desc_digits}{Number of digits to use in the
  descriptive columns. Defaults to the general digits if
  not specified.}
}
\value{
  \code{matrix} A matrix from
  \code{\link{getDescriptionStatsBy}} or
  \code{\link{prGetStatistics}}
}
\description{
  Gets the variable stats
}
\author{
  max
}

