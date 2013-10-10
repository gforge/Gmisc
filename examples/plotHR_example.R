library(survival)
library(rms)

# Get data for example
n <- 1000
set.seed(731)

age <- round(50 + 12*rnorm(n), 1)
label(age) <- "Age"

sex <- factor(sample(c('Male','Female'), n, 
    rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)

smoking <- factor(sample(c('Yes','No'), n, 
    rep=TRUE, prob=c(.2, .75)))

h <- .02*exp(.02*(age-50)+.1*((age-50)/10)^3+.8*(sex=='Female')+2*(smoking=='Yes'))
dt <- -log(runif(n))/h
label(dt) <- 'Follow-up Time'

e <- ifelse(dt <= cens,1,0)
dt <- pmin(dt, cens)
units(dt) <- "Year"

# Add missing data to smoking
smoking[sample(1:n, round(n*0.05))] <- NA

# Create a data frame since plotHR will otherwise
# have a hard time getting the names of the variables
ds <- data.frame(
  dt = dt,
  e = e,
  age=age, 
  smoking=smoking,
  sex=sex)

Srv <- Surv(dt,e)
fit.coxph <- coxph(Srv ~ bs(age, 3) + sex + smoking, data=ds)

org_par <- par(xaxs="i", ask=TRUE)
plotHR(fit.coxph, term="age", plot.bty="o", xlim=c(30, 70), xlab="Age")

dd <- datadist(ds)
options(datadist='dd')
fit.cph <- cph(Srv ~ rcs(age,4) + sex + smoking, data=ds, x=TRUE, y=TRUE)

plotHR(fit.cph, term=1, plot.bty="l", xlim=c(30, 70), xlab="Age")

plotHR(fit.cph, term="age", plot.bty="l", xlim=c(30, 70), ylog=FALSE, rug="ticks", xlab="Age")

unadjusted_fit <- cph(Srv ~ rcs(age,4), data=ds, x=TRUE, y=TRUE)
plotHR(list(fit.cph, unadjusted_fit), term="age", xlab="Age",
		polygon_ci=c(TRUE, FALSE), 
		col.term = c("#08519C", "#77777799"),
		col.se = c("#DEEBF7BB", grey(0.6)),
		lty.term = c(1, 2),
		plot.bty="l", xlim=c(30, 70))
par(org_par)
