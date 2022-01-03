#' Add a p-value column to the results
#'
#' @param results The results that we want to add the column to
#'
#' @return results with added column
#' @inheritParams getDescriptionStatsBy
prAddDescStats <- function(results,
                           x,
                           by,
                           statistics,
                           statistics.suppress_warnings,
                           statistics.sig_lim,
                           statistics.two_dec_lim,
                           html) {
  org <- results
  column_names <- attr(results, "column_names")

  if (statistics.suppress_warnings) {
    pval <- suppressWarnings(statistics(x = x, by = by))
  } else {
    pval <- statistics(x = x, by = by)
  }

  if (is.numeric(pval) &&
      pval <= 1 &&
      pval >= 0) {
    pval <- txtPval(pval,
                    lim.sig = statistics.sig_lim,
                    lim.2dec = statistics.two_dec_lim,
                    html = html
    )
    results <- cbind(results, c(pval, rep("", nrow(results) - 1)))
    column_names <- c(column_names, "P-value")
  } else if (is.character(pval) && !is.null(attr(pval, "colname"))) {
    results <- cbind(results, c(pval, rep("", nrow(results) - 1)))
    column_names <- c(column_names, attr(pval, "colname"))
  } else {
    stop("Your statistics function should either return a numerical value from 0 to 1 or a character with the attribute 'colname'")
  }

  org %>%
    copyAllNewAttributes(results) %>%
    structure(column_names = column_names)
}
