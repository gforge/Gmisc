#' @rdname getDescriptionStatsBy
#' @import htmlTable
#' @export
htmlTable.Gmisc_getDescriptionStatsBy <- function(x, ...) {
  if (attr(x, "multiple")) {
    htmlTable_args <- list(...)
    htmlTable_args <- htmlTable_args[names(htmlTable_args) %in% formalArgs(htmlTable)]
    if (length(htmlTable_args) > 0) {
      x$htmlTable_args <- htmlTable_args
    }

    merged_desc <- do.call(mergeDesc, x)

    # Retain htmlTable formatting if used htmlTable::addHtmlTableStyle()
    return(copyAllNewAttributes(from = x, to = merged_desc))
  }

  htmlTable_args <- list(...)
  htmlTable_args <- htmlTable_args[names(htmlTable_args) %in% formalArgs(htmlTable)]
  htmlTable_args$x <- x
  class(htmlTable_args$x) <- tail(class(htmlTable_args$x), -1)

  do.call(htmlTable, htmlTable_args)
}

#' @rdname getDescriptionStatsBy
#' @export
#' @importFrom knitr asis_output
#' @importFrom knitr knit_print
print.Gmisc_getDescriptionStatsBy <- function(x, ...) {
  htmlTable(x, ...) %>%
    print()
}

#' @rdname getDescriptionStatsBy
#' @export
knit_print.Gmisc_getDescriptionStatsBy <- function(x, ...) {
  htmlTable(x, ...) %>%
    knit_print()
}

#' @rdname getDescriptionStatsBy
#' @export
length.Gmisc_getDescriptionStatsBy <- function(x) {
  if (attr(x, "multiple")) {
    return(nrow(attr(x, "raw_data")))
  }

  length(attr(x, "raw_data"))
}
