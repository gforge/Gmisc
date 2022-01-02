#' @rdname getDescriptionStatsBy
#' @import htmlTable
#' @export
htmlTable.Gmisc.getDescriptionStatsBy.multiple <- function(x, ...) {
  htmlTable_args <- list(...)
  htmlTable_args <- htmlTable_args[names(htmlTable_args) %in% formalArgs(htmlTable)]
  if (length(htmlTable_args) > 0) {
    x$htmlTable_args <- htmlTable_args
  }

  merged_desc <- do.call(mergeDesc, x)

  # Retain htmlTable formatting if used htmlTable::addHtmlTableStyle()
  copyAllNewAttributes(from = x, to = merged_desc)
}

#' @rdname getDescriptionStatsBy
#' @inheritParams mergeDesc
#' @export
htmlTable.Gmisc.getDescriptionStatsBy <- function(x, ...) {
  htmlTable_args <- list(...)
  htmlTable_args <- htmlTable_args[names(htmlTable_args) %in% formalArgs(htmlTable)]
  htmlTable_args$x <- x
  class(htmlTable_args$x) <- tail(class(htmlTable_args$x), -1)

  do.call(htmlTable, htmlTable_args)
}

#' @rdname getDescriptionStatsBy
#' @export
print.Gmisc.getDescriptionStatsBy.multiple <- function(x, ...) {
  htmlTable(x, ...) %>%
    print()
}

#' @rdname getDescriptionStatsBy
#' @export
#' @importFrom knitr asis_output
#' @importFrom knitr knit_print
print.Gmisc.getDescriptionStatsBy <- function(x, ...) {
  htmlTable(x, ...) %>%
    print()
}

#' @rdname getDescriptionStatsBy
#' @export
knit_print.Gmisc.getDescriptionStatsBy.multiple <- function(x, ...) {
  htmlTable(x, ...) %>%
    knit_print()
}

#' @rdname getDescriptionStatsBy
#' @export
#' @importFrom knitr asis_output
#' @importFrom knitr knit_print
knit_print.Gmisc.getDescriptionStatsBy <- function(x, ...) {
  htmlTable(x, ...) %>%
    knit_print()
}
