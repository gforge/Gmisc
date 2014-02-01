\name{fpDrawNormalCI}
\alias{fpDrawCircleCI}
\alias{fpDrawDiamondCI}
\alias{fpDrawNormalCI}
\alias{fpDrawPointCI}
\alias{fpDrawSummaryCI}
\title{Draw standard confidence intervals}
\usage{
fpDrawNormalCI(lower_limit, estimate, upper_limit, size, y.offset = 0.5,
  clr.line, clr.marker, lwd, ...)

fpDrawDiamondCI(lower_limit, estimate, upper_limit, size, y.offset = 0.5,
  clr.line, clr.marker, lwd, ...)

fpDrawCircleCI(lower_limit, estimate, upper_limit, size, y.offset = 0.5,
  clr.line, clr.marker, lwd, ...)

fpDrawPointCI(lower_limit, estimate, upper_limit, size, y.offset = 0.5,
  clr.line, clr.marker, lwd, pch = 1, ...)

fpDrawSummaryCI(lower_limit, estimate, upper_limit, size, col, y.offset = 0.5,
  ...)
}
\arguments{
  \item{lower_limit}{The lower limit of the confidence
  line.  A native numeric variable that can actually be
  outside the boundaries. If you want to see if it is
  outside then convert it to 'npc' and see if the value
  ends up more than 1 or less than 0. Here's how you do the
  conversion: \code{convertX(unit(upper_limit, "native"),
  "npc", valueOnly = TRUE)} and the
  \code{\link[grid]{convertX}} together with
  \code{\link[grid]{unit}} is needed to get the right
  values while you need to provide the valueOnly as you
  cannot compare a unit object.}

  \item{estimate}{The estimate indicating the placement of
  the actual box. Note, this can also be outside bounds and
  is provided in a numeric format the same way as the
  \code{lower_limit}.}

  \item{upper_limit}{The upper limit of the confidence
  line. See lower_limit for details.}

  \item{size}{The actual size of the box/diamond/marker.
  This provided in the 'snpc' format to generate a perfect
  marker. If you provide anything else it will be converted
  prior to that.}

  \item{y.offset}{If you have multiple lines they need an
  offset in the y-direction.}

  \item{clr.line}{The color of the line.}

  \item{clr.marker}{The color of the box}

  \item{lwd}{Line width}

  \item{...}{Allows additional parameters for sibling
  functions}

  \item{pch}{Type of point see
  \code{\link[grid]{grid.points}} for details}

  \item{col}{The color of the summary diamond.}
}
\value{
\code{void} The function outputs the line using grid
compatible functions and does not return anything.
}
\description{
A function that is used to draw the different confidence
intervals for the non-summary lines. Use the
\code{fpDrawNormalCI} function as a template if you want to
make your own funky line + marker.
}
\examples{
ask <- par(ask=TRUE)

test_data <- data.frame(coef1=c(1, 1.59, 1.3, 1.24),
                        coef2=c(1, 1.7, 1.4, 1.04))

test_data$low1 <- test_data$coef1 - 1.96*c(0, .2, .1, .15)
test_data$high1 <- test_data$coef1 + 1.96*c(0, .2, .1, .15)

test_data$low2 <- test_data$coef2 - 1.96*c(0, .1, .15, .2)
test_data$high2 <- test_data$coef2 + 1.96*c(0, .1, .15, .2)

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

# Change all to diamonds
forestplot2(row_names, coef, low, high, 
            confintNormalFn=fpDrawDiamondCI,
            main="Cool study",
            zero = 1, boxsize=0.5,
            col=fpColors(box=c("royalblue", "gold"),
                         line=c("darkblue", "orange"),
                         summary=c("darkblue", "red")),
            xlab="The estimates",
            new_page = TRUE, 
            legend.title="Group",
            legend=c("Treatment", "Placebo"),
            legend.pos=list("topright"),
            legend.r = unit(.1, "snpc"),
            legend.gp = gpar(col="#CCCCCC", lwd=1.5))

# Change first to diamonds
forestplot2(row_names, coef, low, high, 
            confintNormalFn=c("fpDrawDiamondCI", 
                              rep("fpDrawNormalCI", 
                                  times=nrow(coef)-1)),
            main="Cool study",
            zero = 1, boxsize=0.5,
            col=fpColors(box=c("royalblue", "gold"),
                         line=c("darkblue", "orange"),
                         summary=c("darkblue", "red")),
            xlab="The estimates",
            new_page = TRUE, 
            legend.title="Group",
            legend=c("Treatment", "Placebo"),
            legend.pos=list("topright"),
            legend.r = unit(.1, "snpc"),
            legend.gp = gpar(col="#CCCCCC", lwd=1.5))

# You can also use a list with the actual functions
# as long as it is formatted [[row]][[column]]
# Note: if you have a non-square input then
# the software will reformat [[col]][[row]]
# to [[row]][[col]]
forestplot2(row_names, coef, low, high, 
            confintNormalFn=list(list(fpDrawDiamondCI, fpDrawCircleCI),
                                 list(fpDrawNormalCI, fpDrawNormalCI),
                                 list(fpDrawNormalCI, fpDrawCircleCI),
                                 list(fpDrawNormalCI, fpDrawNormalCI)),
            main="Cool study",
            zero = 1, boxsize=0.5,
            col=fpColors(box=c("royalblue", "gold"),
                         line=c("darkblue", "orange"),
                         summary=c("darkblue", "red")),
            xlab="The estimates",
            new_page = TRUE, 
            legend.title="Group",
            legend=c("Treatment", "Placebo"),
            legend.pos=list("topright"),
            legend.r = unit(.1, "snpc"),
            legend.gp = gpar(col="#CCCCCC", lwd=1.5))

par(ask=ask)
}
\author{
Max
}
\seealso{
\code{\link{forestplot2}}
}

