#' Add [Hmisc::label()] to multiple columns
#'
#' Add label attribute using `dplyr` syntax using the [Hmisc::label()]
#'
#' @param x The data frame that we want to label
#' @param ... Variable names with their intended label, e.g. `mpg = "Miles per gallon"`.
#'
#' @return The original data.frame
#' @export
#'
#' @importFrom Hmisc label
#' @examples
#' library(magrittr)
#' data(mtcars)
#' mtcars_with_labels <- mtcars %>%
#'   set_column_labels(mpg = "Gas",
#'                     cyl = "Cylinders",
#'                     hp = "Strength")
#' Hmisc::label(mtcars_with_labels$mpg)
#' @family Hmisc helpers
set_column_labels <- function(x, ...) {
  labels <- list(...)
  stopifnot(is.data.frame(x))

  stopifnot(all(sapply(labels, is.character)))
  stopifnot(all(sapply(labels, length) == 1))
  stopifnot(all(names(labels) %in% colnames(x)))

  for (n in names(labels)) {
    label(x[[n]]) <- labels[[n]]
  }

  x
}

