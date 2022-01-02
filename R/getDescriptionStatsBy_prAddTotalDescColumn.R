#' Add a total column to the results
#'
#' @param results The results that we want to add the column to
#'
#' @return results with added column
#' @inheritParams getDescriptionStatsBy
prAddTotalDescColumn <- function(results,
                                 x,
                                 by,
                                 numbers_first,
                                 total_col_show_perc,
                                 show_all_values,
                                 useNA,
                                 useNA.digits,
                                 html,
                                 digits,
                                 continuous_fn,
                                 factor_fn,
                                 prop_fn,
                                 percentage_sign,
                                 header_count = NULL,
                                 add_total_col) {
  org <- results
  column_names <- attr(results, "column_names")
  total_table <- prGetStatistics(x[is.na(by) == FALSE],
                                 numbers_first = numbers_first,
                                 show_perc = total_col_show_perc,
                                 show_all_values = show_all_values,
                                 useNA = useNA,
                                 useNA.digits = useNA.digits,
                                 html = html,
                                 digits = digits,
                                 continuous_fn = continuous_fn,
                                 factor_fn = factor_fn,
                                 prop_fn = prop_fn,
                                 percentage_sign = percentage_sign
  )

  if (!is.matrix(total_table)) {
    total_table <- matrix(total_table, ncol = 1, dimnames = list(names(total_table)))
  }

  if (nrow(total_table) != nrow(results)) {
    stop(
      "There is a discrepancy in the number of rows in the total table",
      " and the by results: ", nrow(total_table), " total vs ", nrow(results), " results",
      "\n Rows total:", paste(rownames(total_table), collapse = ", "),
      "\n Rows results:", paste(rownames(results), collapse = ", ")
    )
  }

  cn_tot <- prGetDescHeader(by = c(Total = length(x[is.na(by) == FALSE])),
                            header_count = header_count,
                            html = html,
                            already_table_format = TRUE)
  if (add_total_col != "last") {
    results <- cbind(total_table, results)
    column_names <- c(cn_tot, column_names)
  } else {
    results <- cbind(results, total_table)
    column_names <- c(column_names, cn_tot)
  }

  org %>%
    copyAllNewAttributes(results) %>%
    structure(column_names = column_names)
}
