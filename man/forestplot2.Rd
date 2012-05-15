\name{forestplot2}
\alias{forestplot2}
\title{Create a forestplot}
\usage{
  forestplot2(labeltext, mean, lower, upper, align = NULL,
    is.summary = FALSE, fontfamily.summary = NULL,
    fontfamily.labelrow = NULL, clip = c(-Inf, Inf),
    xlab = "", zero = 0, graphwidth = unit(51, "mm"),
    col = meta.colors(), xlog = FALSE, xticks = NULL,
    increase.line_height = 1.2, lwd.xaxis = NULL,
    lwd.zero = NULL, lwd.forestlines = NULL, cex = 1,
    boxsize = NULL, ...)
}
\arguments{
  \item{labeltext}{A list with the names of each row. This
  has to have the same amount of rows that the means but it
  can have more if you want to have more complex rowlabels
  where you might want to have the estimates in plain text.
  Use NA:s for blank spaces}

  \item{mean}{A vector with te averages}

  \item{lower}{The lower bound of the confidence interval
  for the forestplot}

  \item{upper}{The upper bound of the confidence interval
  for the forestplot}

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

  \item{graphwidth}{Width of confidence interval graph}

  \item{col}{See meta.colors}

  \item{xlog}{If TRUE, x-axis tick marks are exponentiated}

  \item{xticks}{Optional user-specified x-axis tick marks.
  Specify NULL to use the defaults,numeric(0) to omit the
  x-axis.}

  \item{increase.line_height}{If you plot multiple bars for
  each line you often want to increase the line height.}

  \item{lwd.xaxis}{lwd for the xaxis}

  \item{lwd.zero}{lwd for the vertical line that gives the
  no-effect line}

  \item{lwd.forestlines}{lwd for the confidence bands}

  \item{cex}{The font adjustment}

  \item{boxsize}{Override the default box size based on
  precision}

  \item{...}{Not used}
}
\value{
  void
}
\description{
  forestplot2 is based on the rmeta 2.16 forestplot
  function. The original rmeta package is requiered for the
  meta.colors() function. This function resolves some
  limitations of the original functions such as adding
  expressions and using multiple confidence bands for the
  same label.
}
\details{
  Using multiple bands for the same label is judged to be
  interesting when one wants to compare different outcomes.
}
\examples{
#
# Simple examples of how to do a forestplots with forestplot2
#
###############################################################################

# A basic test
row_names <- list(list("test = 1", expression(test >= 2)))
test_data <- data.frame(coef=c(1.59, 1.24),
        low=c(1.3, 0.99),
        high=c(1.8, 1.55))
ask <- par(ask=TRUE)
forestplot2(row_names, test_data$coef, test_data$low, test_data$high, zero = 1)


# An advanced test
row_names <- list(
        list("variable = 0", "variable = 1", expression(variable >= 2)),
        list(expression(bar(x)==1.8), expression(bar(x) == 1.4), "some cell data"))
test_data <- data.frame(coef1=c(1.59, 1.3, 1.24),
        coef2=c(1.7, 1.4, 1.04),
        low1=c(1.3, 1.1, 0.99),
        low2=c(1.6, 1.2, 0.7),
        high1=c(1.94, 1.6, 1.55),
        high2=c(1.8, 1.55, 1.33))

attach(test_data)
coef <- cbind(coef1, coef2)
low <- cbind(low1, low2)
high <- cbind(high1, high2)
forestplot2(row_names, coef, low, high, zero = 1, 
        col=meta.colors(box=c("royalblue", "gold"), ,line=c("darkblue", "orange"), summary="royalblue"))
detach(test_data)
par(ask=ask)
}
\author{
  max
}

