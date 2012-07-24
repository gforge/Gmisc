data(mtcars)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10^3 kg" # not sure the unit is correct 

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$cyl) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"), size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

vars <- getDescriptionStatsBy(mtcars$mpg, mtcars$am)
vars <- getDescriptionStatsBy(mtcars$wt, mtcars$am, vars = vars)
print(vars)

latex(
  object   = vars,
  file     = "", # skip this if you want the latex to render
  caption  = "Continuous \\& binary variables", 
  colheads = c(sprintf("%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

cyl_data <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
col_data <- getDescriptionStatsBy(mtcars$col, mtcars$am)

# I use these together with latex to get a nice table
latex(
  object   = rbind(cyl_data, col_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Factored variables",
  colheads = sprintf("%s (%%)", levels(mtcars$am)),
  rowlabel = "Variable",
  rgroup   = c(label(cyl_data),
               label(col_data)),
  n.rgroup = c(NROW(cyl_data),
               NROW(col_data)),
  ctable   = TRUE)
