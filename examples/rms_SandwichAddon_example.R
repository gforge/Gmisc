# Generate some data
n <- 500
x1 <- runif(n)*2
x2 <- runif(n)
y <- x1^3 + x2 + rnorm(n)

dd <- datadist(x1, x2, y)
org.op <- options(datadist = "dd")

# Main function
f    <- ols(y ~ rcs(x1, 3) + x2)

# Check the bread
bread(f)
# Check the HC-matrix
vcovHC(f, type="HC4m")
# Adjust the model so that it uses the HC4m variance
f_rob <- robcov_alt(f, type="HC4m")
# Get the new HC4m-matrix
# - this function just returns the f_rob$var matrix
vcov(f_rob)
# Now check the confidence interval for the function
confint(f_rob)

options(org.op)
