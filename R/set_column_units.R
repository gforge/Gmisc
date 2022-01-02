#' Add [Hmisc::unit()] to multiple columns
#'
#' Add label attribute using `dplyr` syntax using the [Hmisc::unit()]
#'
#' @param x The data frame that we want to define units on
#' @param ... Variable names with their intended unit, e.g. `hp = "Hp"`.
#'
#' @return The original data.frame
#' @export
#'
#' @importFrom Hmisc units
#' @examples
#' library(magrittr)
#' data(mtcars)
#' mtcars_with_units <- mtcars %>%
#'   set_column_units(wt = "1000 lbs")
#' Hmisc::units(mtcars_with_units$wt)
#' @family Hmisc helpers
set_column_units <- function(x, ...) {
  units_to_set <- list(...)
  stopifnot(is.data.frame(x))

  stopifnot(all(sapply(units_to_set, is.character)))
  stopifnot(all(sapply(units_to_set, length) == 1))
  stopifnot(all(names(units_to_set) %in% colnames(x)))

  for (n in names(units_to_set)) {
    units(x[[n]]) <- units_to_set[[n]]
  }

  x
}

