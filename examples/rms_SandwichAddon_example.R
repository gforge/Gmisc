x1 <- runif(200)
x2 <- runif(200)
y <- x1 + x2 + rnorm(200)

f    <- ols(y ~ rcs(x1, 4) + x2)
bread(f)
vcovHC(f)

robcov_alt(f, type="HC4m")
