\name{getDescriptionStatsBy}
\alias{getDescriptionStatsBy}
\title{A function that returns a description statistic that can be used
for creating a publication "table 1" when you want it by groups.
The function identifies if the variable is a continuous, binary
or a factored variable. The format is inspired by NEJM, Lancet &
BMJ.}
\usage{
  getDescriptionStatsBy(x, by, vars = FALSE, digits = 1,
    html = FALSE, NEJMstyle = FALSE, numbers_first = TRUE)
}
\arguments{
  \item{x}{The variable that you want the statistics for}

  \item{by}{The variable that you want to split into
  different columns}

  \item{vars}{If this statistic is for a continuous or a
  binary statistic it's easy to append the current
  statistic to the previous}

  \item{digits}{The number of decimals used}

  \item{html}{If HTML compatible output shoudl be used
  instead of default LaTeX}

  \item{NEJMstyle}{Adds - no (\%) at the end to
  proportions}
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
label(mtcars$cyl) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

vars <- getDescriptionStatsBy(mtcars$mpg, mtcars$am)
vars <- getDescriptionStatsBy(mtcars$wt, mtcars$am, vars = vars)
print(vars)

latex(
  object   = vars,
  file     = "", # skip this if you want the latex to render
  caption  = "Continuous \\\\& binary variables", 
  colheads = c(sprintf("\%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

cyl_data <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
col_data <- getDescriptionStatsBy(mtcars$col, mtcars$am)

# I use these together with latex to get a nice table
latex(
  object   = rbind(cyl_data, col_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Factored variables",
  colheads = sprintf("\%s (\%\%)", levels(mtcars$am)),
  rowlabel = "Variable",
  rgroup   = c(label(cyl_data),
               label(col_data)),
  n.rgroup = c(NROW(cyl_data),
               NROW(col_data)),
  ctable   = TRUE)
}
\author{
  max
}

