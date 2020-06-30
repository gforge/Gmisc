data(mtcars)
# For labelling we use the label()
# function from the Hmisc package
library(Hmisc)
library(magrittr)

label(mtcars$mpg) <- "Gas"
units(mtcars$mpg) <- "Miles/(US) gallon"

label(mtcars$wt) <- "Weight"
units(mtcars$wt) <- "10<sup>3</sup> kg" # not sure the unit is correct

mtcars$am <- factor(mtcars$am, levels = 0:1, labels = c("Automatic", "Manual"))
label(mtcars$am) <- "Transmission"

mtcars$gear <- factor(mtcars$gear)
label(mtcars$gear) <- "Gears"

# Make up some data for making it slightly more interesting
mtcars$col <- factor(sample(c("red", "black", "silver"),
                     size = NROW(mtcars), replace = TRUE))
label(mtcars$col) <- "Car color"

mergeDesc(getDescriptionStatsBy(mtcars$mpg, mtcars$am,
                                header_count = TRUE,
                                use_units = TRUE),
          getDescriptionStatsBy(mtcars$wt, mtcars$am,
                                header_count = TRUE,
                                use_units = TRUE),
          htmlTable_args = list(caption  = "Basic continuous stats from the mtcars dataset"))

tll <- list()
tll[["Gear (3 to 5)"]] <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
tll <- c(tll,
         list(getDescriptionStatsBy(mtcars$col, mtcars$am)))

mergeDesc(tll,
          htmlTable_args = list(caption  = "Factored variables")) %>%
  htmlTable::addHtmlTableStyle(css.rgroup = "")

tl_no_units <- list()
tl_no_units[["Gas (mile/gallons)"]] <-
  getDescriptionStatsBy(mtcars$mpg, mtcars$am,
                        header_count = TRUE)
tl_no_units[["Weight (10<sup>3</sup> kg)"]] <-
  getDescriptionStatsBy(mtcars$wt, mtcars$am,
                        header_count = TRUE)
mergeDesc(tl_no_units,
          tll) %>%
  htmlTable::addHtmlTableStyle(css.rgroup = "")


# A little more advanced
mtcars$mpg[sample(1:NROW(mtcars), size = 5)] <- NA
getDescriptionStatsBy(mtcars$mpg, mtcars$am, statistics = TRUE)

# Do the horizontal version
getDescriptionStatsBy(mtcars$col, mtcars$am,
                      statistics = TRUE, hrzl_prop = TRUE)

mtcars$wt_with_missing <- mtcars$wt
mtcars$wt_with_missing[sample(1:NROW(mtcars), size = 8)] <- NA
getDescriptionStatsBy(mtcars$wt_with_missing, mtcars$am, statistics = TRUE,
                      hrzl_prop = TRUE, total_col_show_perc = FALSE)


mtcars$col_with_missing <- mtcars$col
mtcars$col_with_missing[sample(1:NROW(mtcars), size = 5)] <- NA
getDescriptionStatsBy(mtcars$col_with_missing, mtcars$am, statistics = TRUE,
                      hrzl_prop = TRUE, total_col_show_perc = FALSE)


\dontrun{
  ## There is also a LaTeX wrapper
  tll <- list(
    getDescriptionStatsBy(mtcars$gear, mtcars$am),
    getDescriptionStatsBy(mtcars$col, mtcars$am))

  latex(mergeDesc(tll),
        caption  = "Factored variables",
        file = "")
}
