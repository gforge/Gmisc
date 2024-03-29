library(magrittr)
library(dplyr)
library(htmlTable)

data(mtcars)
mtcars %<>%
  mutate(am = factor(am, levels = 0:1, labels = c("Automatic", "Manual")),
         vs = factor(vs, levels = 0:1, labels = c("V-shaped", "straight")),
         drat_prop = drat > median(drat),
         drat_prop = factor(drat_prop,
                            levels = c(FALSE, TRUE),
                            labels = c("High ratio", "Low ratio")),
         carb_prop = carb > 2,
         carb_prop = factor(carb_prop,
                            levels = c(FALSE, TRUE),
                            labels = c("&le; 2", "&gt; 2")),
         across(c(gear, carb, cyl), factor))

# A simple bare-bone example
mtcars %>%
  getDescriptionStatsBy(`Miles per gallon` = mpg,
                        Weight = wt,
                        `Carborators &le; 2` = carb_prop,
                        by = am) %>%
  htmlTable(caption  = "Basic continuous stats from the mtcars dataset")
invisible(readline(prompt = "Press [enter] to continue"))

# For labeling & units we use set_column_labels/set_column_unit that use
# the Hmisc package annotation functions
mtcars %<>%
  set_column_labels(am = "Transmission",
                    mpg = "Gas",
                    wt = "Weight",
                    gear = "Gears",
                    disp = "Displacement",
                    vs = "Engine type",
                    drat_prop = "Rear axel ratio",
                    carb_prop = "Carburetors") %>%
  set_column_units(mpg = "Miles/(US) gallon",
                   wt = "10<sup>3</sup> lbs",
                   disp = "cu.in.")

mtcars %>%
  getDescriptionStatsBy(mpg,
                        wt,
                        `Gear&dagger;` = gear,
                        drat_prop,
                        carb_prop,
                        vs,
                        by = am,
                        header_count = TRUE,
                        use_units = TRUE,
                        show_all_values = TRUE)  %>%
  addHtmlTableStyle(pos.caption = "bottom") %>%
  htmlTable(caption  = "Stats from the mtcars dataset",
            tfoot = "&dagger; Number of forward gears")
invisible(readline(prompt = "Press [enter] to continue"))

# Using the default parameter we can
mtcars %>%
  getDescriptionStatsBy(mpg,
                        wt,
                        `Gear&dagger;` = gear,
                        drat_prop,
                        carb_prop,
                        vs,
                        by = am,
                        header_count = TRUE,
                        use_units = TRUE,
                        default_ref = c(drat_prop = "Low ratio",
                                        carb_prop = "&gt; 2"))  %>%
  addHtmlTableStyle(pos.caption = "bottom") %>%
  htmlTable(caption  = "Stats from the mtcars dataset",
            tfoot = "&dagger; Number of forward gears")
invisible(readline(prompt = "Press [enter] to continue"))

# We can also use lists
tll <- list()
tll[["Gear (3 to 5)"]] <- getDescriptionStatsBy(mtcars$gear, mtcars$am)
tll <- c(tll,
         list(getDescriptionStatsBy(mtcars$disp, mtcars$am)))

mergeDesc(tll,
          htmlTable_args = list(caption  = "Factored variables")) %>%
  htmlTable::addHtmlTableStyle(css.rgroup = "")
invisible(readline(prompt = "Press [enter] to continue"))

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
invisible(readline(prompt = "Press [enter] to continue"))

# Other settings
mtcars$mpg[sample(1:NROW(mtcars), size = 5)] <- NA
getDescriptionStatsBy(mtcars$mpg,
                      mtcars$am,
                      statistics = TRUE)
invisible(readline(prompt = "Press [enter] to continue"))

# Do the horizontal version
getDescriptionStatsBy(mtcars$gear,
                      mtcars$am,
                      statistics = TRUE,
                      hrzl_prop = TRUE)
invisible(readline(prompt = "Press [enter] to continue"))

mtcars$wt_with_missing <- mtcars$wt
mtcars$wt_with_missing[sample(1:NROW(mtcars), size = 8)] <- NA
getDescriptionStatsBy(mtcars$wt_with_missing, mtcars$am, statistics = TRUE,
                      hrzl_prop = TRUE, total_col_show_perc = FALSE)
invisible(readline(prompt = "Press [enter] to continue"))

\dontrun{
  ## There is also a LaTeX wrapper
  tll <- list(
    getDescriptionStatsBy(mtcars$gear, mtcars$am),
    getDescriptionStatsBy(mtcars$col, mtcars$am))

  latex(mergeDesc(tll),
        caption  = "Factored variables",
        file = "")
}
