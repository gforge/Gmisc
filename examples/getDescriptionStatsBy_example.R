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
mtcars$col <- factor(sample(c("red", "black", "silver"), 
                     size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

mpg_data <- getDescriptionStatsBy(mtcars$mpg, mtcars$am, use_units = TRUE)
wt_data <- getDescriptionStatsBy(mtcars$wt, mtcars$am, use_units = TRUE)
print(rbind(mpg_data, wt_data))

latex(
  object   = rbind(mpg_data, wt_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Continuous \\& binary variables", 
  colheads = c(sprintf("%s (SD)", levels(mtcars$am)), "units"),
  rowlabel = "Variable",
  ctable   = TRUE)

gear_data <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
col_data <- getDescriptionStatsBy(mtcars$col, mtcars$am)

# I use these together with latex to get a nice table
latex(
  object   = rbind(gear_data, col_data),
  file     = "", # skip this if you want the latex to render
  caption  = "Factored variables",
  colheads = sprintf("%s (%%)", levels(mtcars$am)),
  rowlabel = "Variable",
  rgroup   = c(label(gear_data),
               label(col_data)),
  n.rgroup = c(NROW(gear_data),
               NROW(col_data)),
  ctable   = TRUE)

# A little more advanced
mtcars$mpg[sample(1:NROW(mtcars), size=4)] <- NA
getDescriptionStatsBy(mtcars$mpg, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE)

# Do the horizontal version
getDescriptionStatsBy(mtcars$col, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE)

mtcars$wt_with_missing <- mtcars$wt
mtcars$wt_with_missing[sample(1:NROW(mtcars), size=8)] <- NA
getDescriptionStatsBy(mtcars$wt_with_missing, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE, total_col_show_perc = FALSE)


mtcars$col_with_missing <- mtcars$col
mtcars$col_with_missing[sample(1:NROW(mtcars), size=5)] <- NA
getDescriptionStatsBy(mtcars$col_with_missing, mtcars$am, statistics=TRUE, 
                      show_missing=TRUE, hrzl_prop = TRUE, total_col_show_perc = FALSE)

