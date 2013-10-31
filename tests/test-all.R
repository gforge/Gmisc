
# Just a simple simulation
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
  x1 = runif(200),
  x2 = runif(200),
  x3 = factor(sample(LETTERS[1:3], size=200, replace=TRUE)))

library("rms")
# The dataset setup for the rms tests
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

dd <- datadist(age, blood.pressure, 
  cholesterol, sex)

library('testthat')
test_package('Gmisc')
## Can't get it to work properly... with the test_package
#my_test_dir <- gsub("Gmisc.*", "Gmisc/inst/tests", getwd())
#test_dir(my_test_dir)
