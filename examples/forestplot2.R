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
