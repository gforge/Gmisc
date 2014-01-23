# simulated data to use 
set.seed(10)
ds <- data.frame(
  ftime = rexp(200),
  fstatus = sample(0:1,200,replace=TRUE),
	x1 = runif(200),
	x2 = runif(200),
	x3 = runif(200),
  x4 = factor(sample(LETTERS[1:4], size=200, replace=TRUE)))

library(rms)
dd <- datadist(ds)
options(datadist="dd")

s <- Surv(ds$ftime, ds$fstatus == 1)
fit <- cph(s ~ x1 + x2 + x3, data=ds)

printCrudeAndAdjustedModel(fit, c("x[12]", "x3"), file="")

fit <- cph(s ~ x1 + x2 + x3 + x4, data=ds, x=TRUE, y=TRUE)
printCrudeAndAdjustedModel(fit, file="", add_references = TRUE, 
                           desc_column=TRUE, order=c("x3", "x4"))

# Use some labels to prettify the output
# fro the mtcars dataset
data("mtcars")

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10^3 kg" # not sure the unit is correct 

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

require(splines)
fit_mtcar <- lm(mpg ~ wt + gear + col, data=mtcars)
printCrudeAndAdjustedModel(fit_mtcar, file="", 
                           add_references=TRUE,
                           ctable=TRUE, 
                           desc_column = TRUE)

printCrudeAndAdjustedModel(fit_mtcar, 
                           add_references=TRUE,
                           desc_column=TRUE,
                           order=c("Interc", "gear"))

# Alterntive print - just an example, doesn't make sense to skip reference
printCrudeAndAdjustedModel(fit_mtcar, 
                           file="", 
                           order=c("col", "gear"), 
                           groups=c("Color", "Gears"),
                           add_references=c("Black", NA),
                           ctable=TRUE,
                           output = "html")
