# Get data for example
hmohiv<-read.table("http://www.ats.ucla.edu/stat/R/examples/asa/hmohiv.csv", sep=",", header = TRUE)

library(survival)
surv <- with(hmohiv, Surv(time, censor))
fit <- coxph(surv ~ pspline(age) + drug, data=hmohiv)

org_par <- par(xaxs="i", yaxs="i", ask=TRUE)
plotHR(fit, terms="age", bty="l", xlim=c(25, 55))

library(rms)
dd <- datadist(hmohiv) # compute data distribution summary
options(datadist='dd') # for plotting

fit <- cph(surv ~ rcs(age, 5) + drug, data=hmohiv, x=T, y=T)
par(xaxs="i", yaxs="i")
plotHR(fit, terms=1, bty="l", xlim=c(25, 55))

fit <- cph(surv ~ rcs(age, 5), data=hmohiv, x=T, y=T)
plotHR(fit, terms="age", bty="l", xlim=c(25, 55))

fit <- cph(surv ~ rcs(age, 5), data=hmohiv, x=T, y=T)
plotHR(fit, terms="age", bty="l", xlim=c(25, 55), y.ticks=c(1,2,3), ylog=F, rug="ticks")

par(org_par)