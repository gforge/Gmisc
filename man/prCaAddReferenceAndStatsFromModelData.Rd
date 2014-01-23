\name{prCaAddReferenceAndStatsFromModelData}
\alias{prCaAddReferenceAndStatsFromModelData}
\title{Add reference according to the model}
\usage{
prCaAddReferenceAndStatsFromModelData(model, order, add_references,
  add_references_pos, reference_zero_effect, values, ds, output, desc_column,
  desc_show_tot_perc, desc_numb_first, desc_continuous_fn, desc_prop_fn,
  desc_factor_fn, desc_show_missing, desc_digits, desc_colnames, use_labels)
}
\arguments{
  \item{model}{The regression model}

  \item{order}{The order}

  \item{add_references}{True if it should use the dataset
  to look for references, otherwise supply the function
  with a vector with names. Sometimes you want to indicate
  the reference row for each group. This needs to be just
  as many as the groups as the order identified. Use NA if
  you don't want to have a reference for that particular
  group.}

  \item{add_references_pos}{The position where a reference
  should be added. Sometimes you don't want the reference
  to be at the top, for instance if you have age groups
  then you may have < 25, 25-39, 40-55, > 55 and you have
  the reference to be 25-39 then you should set the
  reference list for \code{age_groups} as
  \code{add_references_pos = list(age_groups = 2)} so that
  you have the second group as the position for the
  reference.}

  \item{reference_zero_effect}{The zero effect that the
  reference uses}

  \item{values}{The values that are to be outputted}

  \item{ds}{The dataset}

  \item{output}{Choose the type of output that you want
  returned, html, latex or raw.  The raw alternative is a
  list with the arguments that would be sent to the
  latex/htmlTable functions, where x is the main content of
  the table.}

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

  \item{desc_colnames}{The names of the two descriptive
  columns. By default Total and Event.}

  \item{use_labels}{If labels should be used for rownames}
}
\value{
list
}
\description{
This is of course for factored variables and not in
general.
}
\author{
max
}

