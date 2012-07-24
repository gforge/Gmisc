library(survival)
library(rms)

# Get data for example
n <- 1000
set.seed(731)

age <- 50 + 12*rnorm(n)
label(age) <- "Age"

sex <- factor(sample(c('Male','Female'), n, 
    rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)

h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
dt <- -log(runif(n))/h
label(dt) <- 'Follow-up Time'

e <- ifelse(dt <= cens,1,0)
dt <- pmin(dt, cens)
units(dt) <- "Year"

# Create a data frame since plotHR will otherwise
# have a hard time getting the names of the variables
ds <- data.frame(
  dt = dt,
  e = e,
  age=age, 
  sex=sex)
dd <- datadist(ds)
options(datadist='dd')

Srv <- Surv(dt,e)
fit <- coxph(Srv ~ pspline(age) + sex, data=ds)

org_par <- par(xaxs="i", yaxs="i", ask=TRUE)
plotHR(fit, terms="age", bty="l", xlim=c(30, 70))

fit <- cph(Srv ~ rcs(age,4) + sex, data=ds, x=TRUE, y=TRUE)
par(xaxs="i", yaxs="i")
plotHR(fit, terms=1, bty="l", xlim=c(30, 70))

plotHR(fit, terms="age", bty="l", xlim=c(30, 70), ylog=FALSE, rug="ticks")

par(org_par)
