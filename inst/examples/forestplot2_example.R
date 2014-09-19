##############################################################
# Simple examples of how to do a forestplot with forestplot2 #
##############################################################

ask <- par(ask=TRUE)
library(grid)

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

# Print two plots side by side using the grid
# package's layout option for viewports
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col = 1))
forestplot2(row_names,
            test_data$coef,
            test_data$low,
            test_data$high,
            zero = 1,
            cex  = 2,
            lineheight = "auto",
            xlab = "Lab axis txt")
popViewport()
pushViewport(viewport(layout.pos.col = 2))
forestplot2(row_names,
            test_data$coef,
            test_data$low,
            test_data$high,
            zero = 1,
            cex  = 2,
            lineheight = "auto",
            xlab = "Lab axis txt")
popViewport(2)


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
            zero = c(0.98, 1.02), boxsize=0.5,
            col=fpColors(box=c("royalblue", "gold"),
                         line=c("darkblue", "orange"),
                         summary=c("darkblue", "red")),
            xlab="The estimates",
            new_page = TRUE,
            legend=c("Treatment", "Placebo"),
            legend_args = fpLegend(pos = list("topright"),
                                   title="Group",
                                   r = unit(.1, "snpc"),
                                   gp = gpar(col="#CCCCCC", lwd=1.5)))

# An example of how the exponential works
test_data <- data.frame(coef=c(2.45, 0.43),
  low=c(1.5, 0.25),
  high=c(4, 0.75),
  boxsize=c(0.5, 0.5))
row_names <- cbind(c("Name", "Variable A", "Variable B"),
                   c("HR", test_data$coef))
test_data <- rbind(rep(NA, 3), test_data)

forestplot2(labeltext = row_names,
            test_data[,c("coef", "low", "high")],
            is.summary=c(TRUE, FALSE, FALSE),
            boxsize   = test_data$boxsize,
            zero      = 1,
            xlog      = TRUE,
            col = fpColors(lines="red", box="darkred"),
            new_page = TRUE)


par(ask=ask)
