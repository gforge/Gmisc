\name{fpDrawPointCI}
\alias{fpDrawPointCI}
\title{Draw confidence intervals}
\usage{
fpDrawPointCI(lower_limit, estimate, upper_limit, size, y.offset = 0.5,
  clr.line, clr.box, lwd, pch = 1, ...)
}
\arguments{
  \item{lower_limit}{The lower limit of the line.  A native
  numeric variable that can actually be outside the
  boundaries. If you want to see if it is outside then
  convert it to 'npc' and see if the value ends up more
  than 1 or less than 0. Here's how you do the conversion:
  \code{convertX(unit(upper_limit, "native"), "npc",
  valueOnly = TRUE)} and the \code{\link[grid]{convertX}}
  together with \code{\link[grid]{unit}} is needed to get
  the right values while you need to provide the valueOnly
  as you cannot compare a unit object.}

  \item{estimate}{The estimate indicating the placement of
  the actual point. Note, this can also be outside bounds
  and is provided in a numeric format the same way as the
  \code{lower_limit}.}

  \item{upper_limit}{The upper limit of the line. See
  lower_limit for details.}

  \item{size}{The actual diameter of the point. This
  provided in the 'snpc' format. If you provide anything
  else it will be converted prior to that.}

  \item{y.offset}{If you have multiple lines they need an
  offset in the y-direction.}

  \item{clr.line}{The color of the line.}

  \item{clr.box}{The color of the point}

  \item{lwd}{Line width}

  \item{pch}{Type of point}

  \item{...}{Allows additional parameters for sibling
  functions}
}
\value{
\code{void} The function outputs the line using grid
compatible functions and does not return anything.
}
\description{
This function is the same as \code{\link{fpDrawNormalCI}}
with the difference that it draws a point using the
\code{\link[grid]{grid.points}} function.
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
            lineheight = "auto",
            xlab = "Lab axis txt",
            new_page = TRUE)


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
forestplot2(row_names, coef, low, high, 
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
            col = fpColors(lines="red", box="darkred"),
            new_page = TRUE)

par(ask=ask)
}
\author{
Max
}

