#' A wrapper around the \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}} function
#'
#' Calls the \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}} after extracting the
#' \code{rgroup}/\code{n.rgroup} arguments.
#'
#' @param x The \code{\link{mergeDesc}} object
#' @param ... Passed onto \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}}
#'
#' @rdname htmlTable_latex
#' @importFrom Hmisc latex
#' @keywords internal
#' @export
latex.descMrg <- function(x, ...) {
  dots <- list(...)
  if (!"rgroup" %in% names(dots)) {
    return(NextMethod(
      generic = NULL, object = x,
      rgroup = attr(x, "rgroup"),
      n.rgroup = attr(x, "n.rgroup"),
      ...
    ))
  }

  return(NextMethod(
    generic = NULL, object = x,
    ...
  ))
}


#' @rdname htmlTable_latex
#' @export
#' @import htmlTable
#' @import magrittr
htmlTable.descMrg <- function(x, ...) {
  dots <- list(...)
  htmlTable_args <- attr(x, "htmlTable_args")
  for (n in names(htmlTable_args)) {
    if (!n %in% names(dots)) {
      dots[[n]] <- htmlTable_args[[n]]
    }
  }

  # Merge calls
  class(x) <- class(x)[!(class(x) %in% "descMrg")]
  args <- c(
    list(x = x),
    dots
  )
  if (!"rgroup" %in% names(args)) {
    args[["rgroup"]] <- attr(x, "rgroup")
    args[["n.rgroup"]] <- attr(x, "n.rgroup")
  }

  fastDoCall(htmlTable, args)
}


#' @rdname htmlTable_latex
#' @export
#' @import htmlTable
print.descMrg <- function(x, ...) {
  htmlTable(x, ...) %>%
    print()
}


#' @rdname htmlTable_latex
#' @import htmlTable
#' @export
#' @importFrom knitr asis_output
#' @importFrom knitr knit_print
knit_print.descMrg <- function(x, ...) {
  htmlTable(x, ...) %>%
    knit_print()
}
