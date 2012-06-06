# simulated data to use 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:1,200,replace=TRUE)
cov <- data.frame(
		x1 = runif(200),
		x2 = runif(200),
		x3 = runif(200))

library(rms)
fit <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data=cov)

data_matrix <- getCrudeAndAdjustedModelData(fit)

print(data_matrix)