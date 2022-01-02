#' Add a units column to the results
#'
#' @param results The results that we want to add the column to
#'
#' @return results with added column
#' @inheritParams getDescriptionStatsBy
prAddDescUnitColumn <- function(results, x, use_units, units_column_name) {
  org <- results
  label <- attr(results, "label")
  column_names <- attr(results, "column_names")
  if (isTRUE(use_units)) {
    if (units(x) != "") {
      unitcol <- rep(sprintf("%s", units(x)), times = NROW(results))
      unitcol[rownames(results) == "Missing"] <- ""
    } else {
      unitcol <- rep("", times = NROW(results))
    }
    if (length(unitcol) != nrow(results)) {
      stop(
        "There is an discrepancy in the number of rows in the units",
        " and the by results: ", length(unitcol), " units vs ", nrow(results), " results",
        "\n Units:", paste(unitcol, collapse = ", "),
        "\n Rows results:", paste(rownames(results), collapse = ", ")
      )
    }
    results <- cbind(results, unitcol)
    column_names <- c(column_names, units_column_name)
  } else if (use_units == "name") {
    if (units(x) != "") {
      label <- sprintf("%s (%s)", label, units(x))
    }
  }

  org %>%
    copyAllNewAttributes(results) %>%
    structure(label = label,
              column_names = column_names)
}
