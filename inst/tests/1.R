#########################################
# Test the getCrudeAndAdjustedModelData #
#########################################
source('getCrudeAndAdjustedModelData.R')

set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x1 = runif(200),
  x2 = runif(200),
  x3 = factor(sample(LETTERS[1:3], size=200, replace=TRUE)))


library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

# Check that it doesn't include the rcs() spline since this doesn't
# make sense 
fit <- cph(s ~ x1 + rcs(x2, 3) + strat(x3), data=ds)
data_matrix <- getCrudeAndAdjustedModelData(fit)

data_matrix <- getCrudeAndAdjustedModelData(fit)

expect_that(NROW(data_matrix), equals(1))
expect_that(NCOL(data_matrix), equals(4))
