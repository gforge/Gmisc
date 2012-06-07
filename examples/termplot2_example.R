par_ask <- par(ask=TRUE)
require(graphics)

had.splines <- "package:splines" %in% search()
if(!had.splines) rs <- require(splines)

x <- 1:100
z <- factor(rep(c(LETTERS[c(3,3,3,2,2,3)], LETTERS[1:4]),10))
y <- rnorm(100, sin(x/10)+as.numeric(z))
model <- glm(y ~ ns(x,6) + z)

par_mfrow <- par(mfrow=c(2,2)) ## 2 x 2 plots for same model :

termplot2(model, main = paste("termplot( ", deparse(model$call)," ...)"))
termplot2(model, rug=TRUE)
termplot2(model, rug=TRUE, rug.type="density")

termplot2(model, partial.resid=TRUE, se = TRUE, main = TRUE)
termplot2(model, partial.resid=TRUE, se=TRUE, main = TRUE, se.type="polygon", 
  col.term = "darkblue", col.se="lightblue")

termplot2(model, partial.resid=TRUE, smooth=panel.smooth, span.smth=1/4)

if(!had.splines && rs) detach("package:splines")

# Survival model for use in showing an exponential model
library(survival)

# Dataset from the survival library 
survobj <- with(lung, Surv(time, status==2))
lung$sex <- with(lung, factor(sex, levels=1:2, labels=c("Male", "Female")))

surv_model <- coxph(survobj ~ ns(age, 4)+sex, data=lung)

par(mfrow=c(1,2))
termplot2(surv_model, rug=TRUE, rug.type="density", se.type="polygon", yscale="exponential")


par(par_mfrow)
par(par_ask)