## ---- message=FALSE------------------------------------------------------
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

## ------------------------------------------------------------------------
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

mergeDesc(t1,
          htmlTable_args = list(css.rgroup = "",
                                caption  = "Basic descriptive statistics from the mtcars dataset",
                                tfoot = "&dagger; The weight is in 10<sup>3</sup> kg"))

## ------------------------------------------------------------------------
mergeDesc(getTable1Stats(mtcars$mpg),
          `Weight&dagger;` = getTable1Stats(mtcars$wt),
          Color = getTable1Stats(mtcars$col),
          getTable1Stats(mtcars$gear),
          htmlTable_args = list(css.rgroup = "",
                                caption  = "Basic descriptive statistics from the mtcars dataset",
                                tfoot = "&dagger; The weight is in 10<sup>3</sup> kg"))

## ---- warning=FALSE------------------------------------------------------
getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = mtcars$am,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        statistics = TRUE,
                        ...)
  
}

t1 <- list()
t1[["Gas"]] <-
  getTable1Stats(mtcars$mpg)
  
t1[["Weight&dagger;"]] <-
  getTable1Stats(mtcars$wt)

t1[["Color"]] <- 
  getTable1Stats(mtcars$col)

library(magrittr)
mergeDesc(t1,
          getTable1Stats(mtcars$gear)) %>%
  htmlTable(css.rgroup = "",
            caption  = "Basic descriptive statistics from the mtcars dataset",
            tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

## ---- warning=FALSE------------------------------------------------------
getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = mtcars$am,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        statistics = list(continuous = getPvalChiSq, 
                                          factor = getPvalChiSq, 
                                          proportion = getPvalFisher),
                        ...)
  
}

t1 <- list()
t1[["Gas"]] <-
  getTable1Stats(mtcars$mpg)
  
t1[["Weight&dagger;"]] <-
  getTable1Stats(mtcars$wt)

t1[["Color"]] <- 
  getTable1Stats(mtcars$col)

mergeDesc(t1,
          getTable1Stats(mtcars$gear)) %>%
  htmlTable(css.rgroup = "",
            caption  = "P-values generated from a custom set of values",
            tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

