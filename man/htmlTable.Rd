\name{htmlTable}
\alias{htmlTable}
\title{Outputting HTML tables}
\usage{
  htmlTable(x, title = first.word(deparse(substitute(x))),
    headings = NA,
    align = paste(c("l", rep("c", ncol(x) - 1)), collapse = ""),
    halign = paste(rep("c", ncol(x)), collapse = ""),
    cgroup = NULL, n.cgroup = NULL, cgroup.just = NULL,
    rgroup = NULL, n.rgroup = NULL,
    rgroupCSSstyle = "font-weight: 900;",
    rgroupCSSseparator = "border-top: 1px solid grey;",
    rowlabel = title, rowlabel.pos = "bottom",
    ctable = FALSE, compatibility = "LibreOffice",
    rowname = NA, caption = NULL, caption.loc = "top",
    tfoot = NULL, label = NULL, output = TRUE, ...)
}
\arguments{
  \item{x}{The matrix/data.frame with the data}

  \item{title}{The title of the table. Used for labeling
  etc.}

  \item{headings}{a vector of character strings specifying
  column headings, defaulting to \code{x}'s
  \code{colnames}}

  \item{align}{a character strings specifying column
  alignments, defaulting to \code{paste(c("l",
  rep('c',ncol(n_table)-1)),collapse='')} to center. Valid
  alignments are l = left, c = center and r = right. You
  can also specify \code{align='c|c'} and other LaTeX
  tabular formatting.}

  \item{halign}{a character strings specifying alignment
  for column headings, defaulting to centered.}

  \item{cgroup}{a vector or a matrix of character strings
  defining major column headings. The default is to have
  none. This is also known as "the column spanner". If you
  want a column not to have a spanner then put that column
  as "". If you pass cgroup and n.crgroup as matrices you
  can have multiline cgroups. If the different levels have
  different number of elements you need to set the ones
  that lack elements to NA. For instance \code{cgroup =
  rbind(c("first", "second", NA), c("a", "b", "c"))}.}

  \item{n.cgroup}{a vector or matrix containing the number
  of columns for which each element in cgroup is a heading.
  For example, specify \code{cgroup=c("Major 1","Major
  2")}, \code{n.cgroup=c(3,3)} if \code{"Major 1"} is to
  span columns 1-3 and \code{"Major 2"} is to span columns
  4-6.  \code{rowlabel} does not count in the column
  numbers. You can omit \code{n.cgroup} if all groups have
  the same number of columns.}

  \item{cgroup.just}{The justification of the c.groups}

  \item{rgroup}{A vector of character strings containing
  headings for row groups.  \code{n.rgroup} must be present
  when \code{rgroup} is given. The first
  \code{n.rgroup[1]}rows are sectioned off and
  \code{rgroup[1]} is used as a bold heading for them. The
  usual row dimnames (which must be present if
  \code{rgroup} is) are indented. The next
  \code{n.rgroup[2]} rows are treated likewise, etc. If you
  don't want a row to be part of a row group then you just
  put "" for that row, remember to add the corresponding
  number of rows in n.rgroup.}

  \item{n.rgroup}{integer vector giving the number of rows
  in each grouping. If \code{rgroup} is not specified,
  \code{n.rgroup} is just used to divide off blocks of rows
  by horizontal lines. If \code{rgroup} is given but
  \code{n.rgroup} is omitted, \code{n.rgroup} will default
  so that each row group contains the same number of rows.}

  \item{rgroupCSSstyle}{Css style for the rgorup, if
  different styles are wanted for each of the rgroups you
  can just specify a vector with the number of elements}

  \item{rgroupCSSseparator}{The line between different
  rgroups. The line is set to the TR element of the lower
  rgroup, i.e. you have to set the border-top/padding-top
  etc to a line with the expected function. This is only
  used for rgroups that are printed. You can specify
  different separators if you give a vector of rgroup - 1
  length (this is since the first rgroup doesn't have a
  separator).}

  \item{rowlabel}{If x has row dimnames, rowlabel is a
  character string containing the column heading for the
  row dimnames. The default is the name of the argument for
  x.}

  \item{rowlabel.pos}{Where the rowlabel should be
  positioned. This value can be "top", "bottom", "header",
  or a integer between \code{1} and \code{nrow(cgroup) +
  1}. The options "bottom", "header" are the same, where
  the row label is presented at the same level as the
  header.}

  \item{rowname}{Default is rownames of matrix or
  data.frame.}

  \item{caption}{a text string to use as a caption to print
  at the top of the first page of the table. Default is no
  caption.}

  \item{caption.loc}{set to \code{"bottom"} to position a
  caption below the table instead of the default of
  \code{"top"}.}

  \item{tfoot}{Add a table footer if needed at the bottom
  of the table using the \code{<tfoot>} html element.}

  \item{label}{a text string representing a symbolic label
  for the table for referencing as an anchor. All you need
  to do is to reference the table, for instance \code{<a
  href="#anchor_name">see table 2</a>}}

  \item{ctable}{If the table should have a double top
  border or a single a' la LaTeX ctable style}

  \item{compatibility}{Is default set to \code{LibreOffice}
  as some settings need to be in old html format as Libre
  Office can't handle some commands such as the css
  caption-alignment. Note: this option is not yet fully
  implemented for all details, in the future I aim to
  generate a html-correct table and one that is aimed at
  Libre Office compatibility. Word-compatibility is
  difficult as Word ignores most settings and destroys all
  layout attempts (at least that is how my 2010 version
  behaves).}

  \item{...}{Currently not used, here for compatibility
  reasons}

  \item{output}{Set to false if you don't want an immediate
  print}
}
\value{
  Returns a string with the output table if output is not
  set
}
\description{
  This is a function for outputting a more advanced table
  than xtable allows. It's aim is to provide the Hmisc
  \code{\link{latex}()} colgroup and rowgroup functions in
  HTML. The code outputted is perhaps a little raw compared
  to fully CSS formatted HTML. The reason for this is that
  I've chosen maximum compatibility with
  LibreOffice/OpenOffice that lacks any more advanced
  understanding of HTML & CSS.
}
\details{
  If you set the option table_counter you will get a Table
  1,2,3 etc before each table, just set
  \code{options(table_counter=TRUE)}. If you set it to a
  number then that number will correspond to the start of
  the table_counter. The table_counter option will also
  contain the number of the last table, this can be useful
  when referencing it in text. By setting the option
  \code{options(table_counter_str = "<b>Table \%s:</b> ")}
  you can manipulate the counter table text that is added
  prior to the actual caption. Note, you should use the
  \code{\link{sprintf}} \code{\%s} instead of \code{\%d} as
  the software converts all numbers to characters for
  compatibility reasons. If you set
  \code{options(table_counter_roman = TRUE)} then the table
  counter will use Roman numumerals instead of Arabic.

  Note that when using complex cgroup alignments with
  multiple levels not every browser is able to handle this.
  For instance the RStudio webkit browser seems to have
  issues with this and a bug has been filed:
  "http://code.google.com/p/chromium/issues/detail?id=305130"

  If you in your knitr html-document get a "structure"
  noted under the rowlabel heading then this is an effect
  from the automated rowlabel name. It is copied from the
  title that in turn uses the \code{x} to find an
  apropriate name:
  \code{title=first.word(deparse(substitute(x)))}. All you
  need to do is simply set either the \code{title} or the
  \code{rowlabel} arguments to get rid of this "bug".
}
\examples{
# A simple example
mx <- matrix(1:6, ncol=3) 
rownames(mx) <- LETTERS[1:NROW(mx)] 
colnames(mx) <- sprintf("Col \%s", LETTERS[1:NCOL(mx)])
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
  n.cgroup=c(2,1), cgroup=c("First", "Second"))

# A slightly more advanced example
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

mpg_data_mean <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, 
                                       use_units = TRUE, html=TRUE)
mpg_data_median <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, 
                                         use_units = TRUE, html=TRUE, 
                                         continuous_fn=describeMedian)
wt_data <- getDescriptionStatsBy(mtcars$wt, mtcars$am, 
                                 use_units = TRUE, html=TRUE)

vars <- rbind(mpg_data_mean, mpg_data_median, wt_data)
rownames(vars) <- c("Mean (SD)", "Median (IQR)", "Mean (SD)")
htmlTable(
  vars,
  caption  = "Continuous & binary variables", 
  n.rgroup=c(2,1), rgroup=c("Gas", "Weight"),
  n.cgroup=c(2,1), 
  cgroup=c(splitLines4Table("results", sprintf("n=\%d", NROW(mtcars)), html=TRUE),
           ""),
  colheads = c(sprintf("\%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

}
\author{
  max
}
\seealso{
  \code{\link{latex}}, \code{\link{getDescriptionStatsBy}},
  \code{\link{splitLines4Table}}
}

