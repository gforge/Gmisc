# ** Borrowed code from the lrm example **

#Fit a logistic model containing predictors age, blood.pressure, sex
#and cholesterol, with age fitted with a smooth 5-knot restricted cubic 
#spline function and a different shape of the age relationship for males 
#and females.  

n <- 1000    # define sample size
set.seed(17) # so can reproduce the results
age            <- rnorm(n, 50, 10)
blood.pressure <- rnorm(n, 120, 15)
cholesterol    <- rnorm(n, 200, 25)
sex            <- factor(sample(c('female','male'), n,TRUE))
label(age)            <- 'Age'      # label is in Hmisc
label(cholesterol)    <- 'Total Cholesterol'
label(blood.pressure) <- 'Systolic Blood Pressure'
label(sex)            <- 'Sex'
units(cholesterol)    <- 'mg/dl'   # uses units.default in Hmisc
units(blood.pressure) <- 'mmHg'

#To use prop. odds model, avoid using a huge number of intercepts by
#grouping cholesterol into 40-tiles

# Specify population model for log odds that Y=1
L <- .4*(sex=='male') + .045*(age-50) +
     (log(cholesterol - 10)-5.2)*(-2*(sex=='female') + 2*(sex=='male'))
# Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
y <- ifelse(runif(n) < plogis(L), 1, 0)
cholesterol[1:3] <- NA   # 3 missings, at random

ddist <- datadist(age, blood.pressure, cholesterol, sex)
options(datadist='ddist')

fit_lrm <- lrm(y ~ blood.pressure + sex * (age + rcs(cholesterol,4)),
               x=TRUE, y=TRUE)
             
if (isFitLogit(fit_lrm) == TRUE)
  print("Correct, the lrm is a logistic regression")

fit_lm <- lm(blood.pressure ~ sex)
if (isFitLogit(fit_lm) == FALSE)
  print("Correct, the lm is not a logistic regression")

fit_glm_logit <- glm(y ~ blood.pressure + sex * (age + rcs(cholesterol,4)), 
  family = binomial())

if (isFitLogit(fit_glm_logit) == TRUE)
  print("Correct, the glm with a family of binomial is a logistic regression")

fit_glm <- glm(blood.pressure ~ sex)
if (isFitLogit(fit_glm) == FALSE)
  print("Correct, the glm without logit as a family is not a logistic regression")
