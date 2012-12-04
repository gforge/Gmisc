\name{htmlTable}
\alias{htmlTable}
\title{This is a simple function for outputting a more advanced
table than xtable allows. It's aim is to provide the Hmisc
latex() colgroup and rowgroup functions in HTML. The
code outputted is perhaps a little raw compared to fully
CSS formatted HTML. The reason for this is that I've chosen
maximum compatibility with OpenOffice that lacks any more
sofisticated understanding of HTML & CSS.}
\usage{
  htmlTable(x, title = first.word(deparse(substitute(x))),
    headings = colnames(x),
    align = paste(rep("c", ncol(x)), collapse = ""),
    halign = paste(rep("c", ncol(x)), collapse = ""),
    cgroup = NULL, n.cgroup = NULL,
    cgroup.just = rep("c", length(n.cgroup)),
    rgroup = NULL, n.rgroup = NULL,
    rgroupCSSstyle = "font-weight: 900;",
    rgroupCSSseparator = "border-top: 1px solid grey;",
    rowlabel = title, ctable = FALSE,
    rowname = rownames(x), caption = NULL,
    caption.loc = "top", label = title, output = TRUE, ...)
}
\arguments{
  \item{x}{The matrix/data.frame with the data}

  \item{title}{The title of the table. Used for labelling
  etc.}

  \item{headings}{a vector of character strings specifying
  column headings, defaulting to \code{x}'s
  \code{colnames}}

  \item{align}{a character strings specifying column
  alignments, defaulting to
  \code{paste(rep('c',ncol(x)),collapse='')} to center.
  You may specify \code{align='c|c'} and other LaTeX
  tabular formatting.}

  \item{halign}{a character strings specifying alignment
  for column headings, defaulting to centered.}

  \item{cgroup}{a vector of character strings defining
  major column headings. The default is to have none. This
  is also known as "the column spanner".}

  \item{n.cgroup}{a vector containing the number of columns
  for which each element in cgroup is a heading. For
  example, specify \code{cgroup=c("Major 1","Major 2")},
  \code{n.cgroup=c(3,3)} if \code{"Major 1"} is to span
  columns 1-3 and \code{"Major 2"} is to span columns 4-6.
  \code{rowlabel} does not count in the column numbers. You
  can omit \code{n.cgroup} if all groups have the same
  number of columns.}

  \item{cgroup.just}{The justification of the c.groups}

  \item{rgroup}{A vector of character strings containing
  headings for row groups.  \code{n.rgroup} must be present
  when \code{rgroup} is given. The first
  \code{n.rgroup[1]}rows are sectioned off and
  \code{rgroup[1]} is used as a bold heading for them. The
  usual row dimnames (which must be present if
  \code{rgroup} is) are indented. The next
  \code{n.rgroup[2]} rows are treated likewise, etc.}

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

  \item{rowname}{Default is rownames of matrix or
  data.frame.  Specify \code{rowname=NULL} to suppress the
  use of row names.}

  \item{caption}{a text string to use as a caption to print
  at the top of the first page of the table. Default is no
  caption.}

  \item{caption.loc}{set to \code{"bottom"} to position a
  caption below the table instead of the default of
  \code{"top"}.}

  \item{label}{a text string representing a symbolic label
  for the table for referencing as an anchor}

  \item{ctable}{If the table should have a double top
  border or a single a' la LaTeX ctable style}

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
  This is a simple function for outputting a more advanced
  table than xtable allows. It's aim is to provide the
  Hmisc latex() colgroup and rowgroup functions in HTML.
  The code outputted is perhaps a little raw compared to
  fully CSS formatted HTML. The reason for this is that
  I've chosen maximum compatibility with OpenOffice that
  lacks any more sofisticated understanding of HTML & CSS.
}
\examples{
mx <- matrix(1:6, ncol=3) 
rownames(mx) <- LETTERS[1:NROW(mx)] 
colnames(mx) <- sprintf("Col \%s", LETTERS[1:NCOL(mx)])
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
  n.cgroup=c(2,1), cgroup=c("First", "Second"))
}
\author{
  max
}

