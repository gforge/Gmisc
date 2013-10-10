#
# Simple examples of how to do a forestplot with forestplot2
#
###############################################################################

ask <- par(ask=TRUE)

# A basic example, create some fake data
row_names <- list(list("test = 1", expression(test >= 2)))
test_data <- data.frame(coef=c(1.59, 1.24),
  low=c(1.3, 0.99),
  high=c(1.8, 1.55))
forestplot2(row_names, test_data$coef, test_data$low, test_data$high, zero = 1)


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

attach(test_data)

coef <- cbind(coef1, coef2)
low <- cbind(low1, low2)
high <- cbind(high1, high2)
forestplot2(row_names, coef, low, high, zero = 1, boxsize=0.5,
  col=fpColors(box=c("royalblue", "gold"),
    line=c("darkblue", "orange")))

detach(test_data)

# An example of how the exponential works
row_names <- list(list("Variable A", "Variable B"))
test_data <- data.frame(coef=c(2.45, 0.43),
  low=c(1.5, 0.25),
  high=c(4, 0.75),
  boxsize=c(0.5, 0.5))

forestplot2(labeltext = row_names, 
  mean      = test_data$coef, 
  lower     = test_data$low, 
  upper     = test_data$high, 
  boxsize   = test_data$boxsize,
  zero      = 1,
  xlog      = TRUE,
  col = fpColors(lines="red", box="darkred"))

par(ask=ask)
