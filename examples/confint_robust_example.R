n <- 50
x <- runif(n)
y <- x + rnorm(n)

fit <- lm(y~x)
confint_robust(fit, HC_type = "HC4m")
