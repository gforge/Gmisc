# simulated data to use 
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x1 = runif(200),
  x2 = runif(200),
  x3 = runif(200))

library(survival)
library(rms)

dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

if (isFitCoxPH(fit))
  print("Correct, the cph is of cox PH hazard type")

fit <- coxph(s ~ x1 + x2 + x3, data=ds)
if (isFitCoxPH(fit))
  print("Correct, the coxph is of cox PH hazard type")

library(cmprsk)
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace=TRUE)
cov <- matrix(runif(600),nrow=200)
dimnames(cov)[[2]] <- c('x1','x2','x3')
fit <- crr(ftime,fstatus,cov)

if (isFitCoxPH(fit))
  print(paste("Correct, the competing risk regression is",
      "considered a type of cox regression",
      "since it has a Hazard Ratio"))

