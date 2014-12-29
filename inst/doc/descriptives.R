## ----, message=FALSE-----------------------------------------------------
data(mtcars)
# For labelling we use the label()
# function from the Hmisc package
library(Hmisc)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10<sup>3</sup> kg" # not sure the unit is correct

mtcars$am <- factor(mtcars$am, levels=0:1, labels=c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"),
                     size=NROW(mtcars), replace=TRUE))
label(mtcars$col) <- "Car color"

## ------------------------------------------------------------------------
library(Gmisc)
getDescriptionStatsBy(x = mtcars$mpg, 
                      by = mtcars$am)

## ------------------------------------------------------------------------
getDescriptionStatsBy(x = mtcars$mpg, 
                      by = mtcars$am,
                      continuous_fn = describeMedian)

## ------------------------------------------------------------------------
getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = mtcars$am,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        ...)
  
}
getTable1Stats(mtcars$mpg)

## ------------------------------------------------------------------------
getTable1Stats(mtcars$mpg, use_units = TRUE)

## ----, results='asis'----------------------------------------------------
t1 <- list()
t1[["Gas"]] <-
  getTable1Stats(mtcars$mpg)
  
t1[["Weight&dagger;"]] <-
  getTable1Stats(mtcars$wt)

t1[["Color"]] <- 
  getTable1Stats(mtcars$col)

# If we want to use the labels set in the beginning
# we add an element without a name
t1 <- c(t1,
        list(getTable1Stats(mtcars$gear)))

htmlTable(mergeDesc(t1),
          css.rgroup = "",
          caption  = "Basic descriptive statistics from the mtcars dataset",
          tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

## ----, results='asis', warning=FALSE-------------------------------------
# A little more advanced input
mtcars$mpg_w_missing <- mtcars$mpg
mtcars$mpg_w_missing[sample(1:NROW(mtcars), size=5)] <- NA
mtcars$wt_w_missing <- mtcars$wt
mtcars$wt_w_missing[sample(1:NROW(mtcars), size=8)] <- NA

t1 <- list()
t1[["Gas"]] <-
  getTable1Stats(mtcars$mpg_w_missing, statistics=TRUE)
  
t1[["Weight&dagger;"]] <-
  getTable1Stats(mtcars$wt, statistics=TRUE)

t1[["Color"]] <- 
  getTable1Stats(mtcars$col, statistics=TRUE)

# If we want to use the labels set in the beginning
# we add an element without a name
t1 <- c(t1,
        list(getTable1Stats(mtcars$gear, statistics=TRUE)))

htmlTable(mergeDesc(t1),
          css.rgroup = "",
          caption  = "Basic descriptive statistics from the mtcars dataset",
          tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

