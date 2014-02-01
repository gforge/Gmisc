org.par <- par("ask" = TRUE)

# simulated data to test 
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace=TRUE)
cov <- data.frame(
		x1 = runif(200),
		x2 = runif(200),
		x3 = runif(200))

library(rms)
dd <- datadist(cov)
options(datadist="dd")

fit1 <- cph(Surv(ftime, fstatus == 1) ~ x1 + x2 + x3, data=cov)
fit2 <- cph(Surv(ftime, fstatus == 2) ~ x1 + x2 + x3, data=cov)

forestplotRegrObj (regr.obj = fit1, new_page=TRUE)

forestplotRegrObj (regr.obj = list(fit1, fit2),
                   legend = c("Status = 1", "Status = 2"), 
                   legend.title="Type of regression",
                   new_page=TRUE)
  
modifyNameFunction <- function(x){
  if (x == "x1")
    return ("Covariate A")
  
  if (x == "x2")
    return (expression(paste("My ", beta[2])))
  
  return (x)
}

forestplotRegrObj (regr.obj = list(fit1, fit2),
                   variablesOfInterest.regexp = "(x2|x3)",
                   reference.names = c("First model", "Second model"),
                   rowname.fn = modifyNameFunction, new_page=TRUE)
  
par(org.par)
