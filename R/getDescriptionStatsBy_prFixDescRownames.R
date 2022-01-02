#' Fix rownames for descriptive results
#'
#' Helper for [getDescriptionStatsBy] that fixes row names
#'
#' @param results A matrix with the results
#' @param t The [base::by()] output
#' @param name Name if row names are missing or the results is a single row
#'
#' @return The results with fixed names
prFixDescRownames <- function(results, t, name) {
  # Add the proper row names
  if ("matrix" %in% class(t[[1]])) {
    rownames(results) <- rownames(t[[1]])
  } else {
    rownames(results) <- names(t[[1]])
  }

  # This is an effect from the numeric variable not having
  # a naming function
  if (is.null(rownames(results)) && nrow(results) == 1) {
    rownames(results) <- name
  }

  return(results)
}
