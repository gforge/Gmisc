\name{getDescriptionStatsBy}
\alias{getDescriptionStatsBy}
\title{Creating of description statistics}
\usage{
  getDescriptionStatsBy(x, by, digits = 1, html = FALSE,
    NEJMstyle = FALSE, numbers_first = TRUE,
    statistics = FALSE, min_pval = 10^-4,
    show_missing = FALSE, continuous_fn = describeMean,
    prop_fn = describeProp, factor_fn = describeFactors,
    show_all_values = FALSE, hrzl_prop = FALSE,
    add_total_col = hrzl_prop, total_col_show_perc = TRUE,
    use_units = FALSE)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{by}{The variable that you want to split into
  different columns}

  \item{digits}{The number of decimals used}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}

  \item{NEJMstyle}{Adds - no (\%) at the end to
  proportions}

  \item{numbers_first}{If the number should be given or if
  the percentage should be presented first. The second is
  encapsulated in parentheses ().}

  \item{show_missing}{Show the missing values. This adds
  another row if there are any missing values.}

  \item{continuous_fn}{The method to describe continuous
  variables. The default is \code{\link{describeMean}}.}

  \item{prop_fn}{The method used to describe proportions,
  see \code{\link{describeProp}}.}

  \item{factor_fn}{The method used to describe factors, see
  \code{\link{describeFactors}}.}

  \item{statistics}{Add statistics, fisher test for
  proportions and Wilcoxon for continuous variables}

  \item{min_pval}{The minimum p-value before doing a "<
  0.0001"}

  \item{hrzl_prop}{This is default FALSE and indicates that
  the proportions are to be interpreted in a vertical
  manner.  If we want the data to be horizontal, i.e. the
  total should be shown and then how these differ in the
  different groups then set this to TRUE.}

  \item{use_units}{If the Hmisc package's units() function
  has been employed it may be interesting to have a column
  at the far right that indicates the unit measurement. If
  this column is specified then the total column will
  appear before the units (if specified as last).}

  \item{show_all_values}{This is by default false as for
  instance if there is no missing and there is only one
  variable then it is most sane to only show one option as
  the other one will just be a complement to the first. For
  instance sex - if you know gender then automatically you
  know the distribution of the other sex as it's 100 \% -
  other \%.}

  \item{total_col_show_perc}{This is by default true but if
  requested the percentages are surpressed as this
  sometimes may be confusing.}

  \item{add_total_col}{This adds a total column to the
  resulting table.  You can also specify if you want the
  total column "first" or "last" in the column order.}
}
\value{
  Returns a vector if vars wasn't specified and it's a
  continuous or binary statistic. If vars was a matrix then
  it appends the result to the end of that matrix. If the x
  variable is a factor then it does not append and you get
  a warning.
}
\description{
  A function that returns a description statistic that can
  be used for creating a publication "table 1" when you
  want it by groups. The function identifies if the
  variable is a continuous, binary or a factored variable.
  The format is inspired by NEJM, Lancet & BMJ.
}
\examples{
data(mtcars)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10^3 kg" # not sure the unit is correct 

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), 
                     size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

mpg_data <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, use_units = TRUE)
wt_data <- getDescriptionStatsBy(mtcars$wt, mtcars$am, use_units = TRUE)
print(rbind(mpg_data, wt_data))

latex(
  object   = rbind(mpg_data, wt_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Continuous \\\\& binary variables", 
  colheads = c(sprintf("\%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

gear_data <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
col_data <- getDescriptionStatsBy(mtcars$col, mtcars$am)

# I use these together with latex to get a nice table
latex(
  object   = rbind(gear_data, col_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Factored variables",
  colheads = sprintf("\%s (\%\%)", levels(mtcars$am)),
  rowlabel = "Variable",
  rgroup   = c(label(gear_data),
               label(col_data)),
  n.rgroup = c(NROW(gear_data),
               NROW(col_data)),
  ctable   = TRUE)

# A little more advanced
mtcars$mpg[sample(1:NROW(mtcars), size=4)] <- NA
getDescriptionStatsBy(mtcars$mpg, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE)

# Do the horizontal version
getDescriptionStatsBy(mtcars$col, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE)

mtcars$wt_with_missing <- mtcars$wt
mtcars$wt_with_missing[sample(1:NROW(mtcars), size=8)] <- NA
getDescriptionStatsBy(mtcars$wt_with_missing, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE, total_col_show_perc = FALSE)


mtcars$col_with_missing <- mtcars$col
mtcars$col_with_missing[sample(1:NROW(mtcars), size=5)] <- NA
getDescriptionStatsBy(mtcars$col_with_missing, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE, total_col_show_perc = FALSE)

}
\author{
  max
}
\seealso{
  \code{\link{describeMean}}, \code{\link{describeProp}},
  \code{\link{describeFactors}}, \code{\link{htmlTable}}
}

