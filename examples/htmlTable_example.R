# A simple example
mx <- matrix(1:6, ncol=3) 
rownames(mx) <- LETTERS[1:NROW(mx)] 
colnames(mx) <- sprintf("Col %s", LETTERS[1:NCOL(mx)])
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
  n.cgroup=c(2,1), cgroup=c("First", "Second"))

# A slightly more advanced example
data(mtcars)

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

mpg_data_mean <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, use_units = TRUE, html=TRUE)
mpg_data_median <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, use_units = TRUE, html=TRUE, continuous_function=describe_median)
wt_data <- getDescriptionStatsBy(mtcars$wt, mtcars$am, use_units = TRUE, html=TRUE)

vars <- rbind(mpg_data_mean, mpg_data_median, wt_data)
rownames(vars) <- c("Mean (SD)", "Median (IQR)", "Mean (SD)")
htmlTable(
  vars,
  caption  = "Continuous & binary variables", 
  n.rgroup=c(2,1), rgroup=c("Gas", "Weight"),
  n.cgroup=c(2,1), 
  cgroup=c(splitLines4Table("results", sprintf("n=%d", NROW(mtcars)), html=TRUE),
           ""),
  colheads = c(sprintf("%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

