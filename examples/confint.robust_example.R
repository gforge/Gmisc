n <- 50
x <- runif(n)
y <- x + rnorm(n)

fit <- lm(y~x)
confint.robust(fit, HC_type = "HC4m")
