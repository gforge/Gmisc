#' A function for converting a show_missing variable.
#'
#' The variable is supposed to be directly compatible with
#' \code{\link[base]{table}}(..., useNA = show_missing). It throws an error
#' if not compatible. It is mostly useful for custom describe functions.
#'
#' \emph{Deprecated:} This function will be deprecated
#' as all functions now use the useNA style in order to comply
#' with standard R naming.
#'
#' @param show_missing Boolean or "no", "ifany", "always"
#' @return string
#'
#' @export
convertShowMissing <- function(show_missing) {
  if (missing(show_missing) ||
    show_missing == FALSE ||
    show_missing == "no") {
    return("no")
  }

  if (show_missing == TRUE) {
    return("ifany")
  }

  if (!show_missing %in% c("no", "ifany", "always")) {
    stop(
      "You have set an invalid option for show_missing variable",
      " '", show_missing, "' - it should be TRUE/FALSE",
      " or one of the options: no, ifany or always."
    )
  }

  if (length(show_missing) > 1) {
    stop("You have an invalid show_missing variable of more than one elements: ", length(show_missing))
  }

  return(show_missing)
}