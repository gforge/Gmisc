\name{forestplot2}
\alias{forestplot2}
\title{Create a forestplot}
\usage{
  forestplot2(labeltext, mean, lower, upper, align = NULL,
    is.summary = FALSE, fontfamily.summary = NULL,
    fontfamily.labelrow = NULL, clip = c(-Inf, Inf),
    xlab = "", zero = 0, graphwidth = "auto",
    col = fpColors(), xlog = FALSE, xticks = NULL,
    xticks.digits = 2, lwd.xaxis = NULL, lwd.zero = NULL,
    lwd.ci = NULL, cex = 1, boxsize = NULL,
    mar = unit(rep(0.05, times = 4), "npc"), legend = NULL,
    legend.pos = c("top", "right"), legend.cex = cex * 0.8,
    ...)
}
\arguments{
  \item{labeltext}{A list, matrix, vector or expression
  with the names of each row. The list should be wrapped in
  m x n number to resemble a matrix:
  \code{list(list("rowname 1 col 1", "rowname 2 col 1"),
  list("r1c2", expression(beta))} You can also provide a
  matrix although this cannot have expressions by design:
  \code{matrix(c("rowname 1 col 1", "rowname 2 col 1",
  "r1c2", "beta"), ncol=2)} Use NA:s for blank spaces and
  if you provide a full column with NA then that column is
  a empty column that adds some space.}

  \item{mean}{A vector or a matrix with the averages}

  \item{lower}{The lower bound of the confidence interval
  for the forestplot, needs to be the same format as the
  mean, i.e. matrix/vector of equal columns & length}

  \item{upper}{The upper bound of the confidence interval
  for the forestplot, needs to be the same format as the
  mean, i.e. matrix/vector of equal columns & length}

  \item{align}{Vector giving alignment (l,r,c) for columns
  of table}

  \item{is.summary}{A vector indicating by TRUE/FALSE if
  the value is a summary value which means that it will
  have a different font-style.}

  \item{fontfamily.summary}{The fontfamily of the summary}

  \item{fontfamily.labelrow}{The fontfamily of a regular
  row}

  \item{clip}{Lower and upper limits for clipping
  confidence intervals to arrows}

  \item{xlab}{x-axis label}

  \item{zero}{x-axis coordinate for zero line}

  \item{graphwidth}{Width of confidence interval graph, see
  \code{\link{unit}} for details on how to utilize mm etc.}

  \item{col}{See \code{\link{fpColors}}}

  \item{xlog}{If TRUE, x-axis tick marks are exponentiated}

  \item{xticks}{Optional user-specified x-axis tick marks.
  Specify NULL to use the defaults, numeric(0) to omit the
  x-axis.}

  \item{xticks.digits}{The number of digits to allow in the
  x-axis if this is created by default.}

  \item{lwd.xaxis}{lwd for the xaxis}

  \item{lwd.zero}{lwd for the vertical line that gives the
  no-effect line}

  \item{lwd.ci}{lwd for the confidence bands}

  \item{cex}{The font adjustment}

  \item{boxsize}{Override the default box size based on
  precision}

  \item{mar}{A numerical vector of the form c(bottom, left,
  top, right) of the type \code{unit()}}

  \item{...}{Not used}
}
\value{
  void
}
\description{
  forestplot2 is based on the rmeta 2.16 forestplot
  function. This function resolves some limitations of the
  original functions such as: \itemize{ \item{Adding
  expressions: }{Allows use of expressions, e.g.
  \code{expression(beta)}} \item{Multiple bands: }{Using
  multiple confidence bands for the same label}
  \item{Autosize: }{Adapts to windows size} }
}
\details{
  Using multiple bands for the same label can be
  interesting when one wants to compare different outcomer.
  It can also be an alternative when you want to show both
  crude and adjusted estimates.

  Known issues: the x-axis does not entirely respect the
  margin. Autosizing boxes is not always the best option,
  try to set these manually as much as possible.
}
\examples{
##############################################################
# Simple examples of how to do a forestplot with forestplot2 #
##############################################################

ask <- par(ask=TRUE)

# A basic example, create some fake data
row_names <- list(list("test = 1", expression(test >= 2)))
test_data <- data.frame(coef=c(1.59, 1.24),
  low=c(1.4, 0.78),
  high=c(1.8, 1.55))
forestplot2(row_names, 
            test_data$coef, 
            test_data$low, 
            test_data$high, 
            zero = 1, 
            cex  = 2,
            xlab = "Lab axis txt")


# An advanced test
test_data <- data.frame(coef1=c(1, 1.59, 1.3, 1.24),
  coef2=c(1, 1.7, 1.4, 1.04),
  low1=c(1, 1.3, 1.1, 0.99),
  low2=c(1, 1.6, 1.2, 0.7),
  high1=c(1, 1.94, 1.6, 1.55),
  high2=c(1, 1.8, 1.55, 1.33))

col_no <- grep("coef", colnames(test_data))
row_names <- list(
  list("Category 1", "Category 2", "Category 3", expression(Category >= 4)),
  list("ref", 
    substitute(expression(bar(x) == val), 
               list(val = round(rowMeans(test_data[2, col_no]), 2))), 
    substitute(expression(bar(x) == val), 
               list(val = round(rowMeans(test_data[3, col_no]), 2))), 
    substitute(expression(bar(x) == val), 
               list(val = round(rowMeans(test_data[4, col_no]), 2))))
)

coef <- with(test_data, cbind(coef1, coef2))
low <- with(test_data, cbind(low1, low2))
high <- with(test_data, cbind(high1, high2))
forestplot2(row_names, coef, low, high, zero = 1, boxsize=0.5,
            col=fpColors(box=c("royalblue", "gold"),
                         line=c("darkblue", "orange")),
            xlab="The estimates")

# An example of how the exponential works
test_data <- data.frame(coef=c(2.45, 0.43),
  low=c(1.5, 0.25),
  high=c(4, 0.75),
  boxsize=c(0.5, 0.5))
row_names <- cbind(c("Name", "Variable A", "Variable B"),
                   c("HR", test_data$coef))
test_data <- rbind(rep(NA, 3), test_data)

forestplot2(labeltext = row_names, 
            is.summary=c(TRUE, FALSE, FALSE),
            mean      = test_data$coef, 
            lower     = test_data$low, 
            upper     = test_data$high, 
            boxsize   = test_data$boxsize,
            zero      = 1,
            xlog      = TRUE,
            col = fpColors(lines="red", box="darkred"))

par(ask=ask)
}
\author{
  Max Gordon, Thomas Lumley
}

