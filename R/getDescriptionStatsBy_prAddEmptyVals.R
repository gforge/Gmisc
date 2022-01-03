#' Convert the by-list into a matrix compatible format
#'
#' Helper for [getDescriptionStatsBy] that fixes empty values in matrix
#' so that they are compatible with the matrix
#'
#' @param t Output from [prNumericDescs], [prPropDescs], or [prFactorDescs].
#'
#' @inheritParams getDescriptionStatsBy
#' @return A fixed list
prAddEmptyVals <- function(t, missing_value) {
  # Convert the list into a list with vectors instead of matrices
  for (n in names(t)) {
    if (is.matrix(t[[n]])) {
      tmp_names <- rownames(t[[n]])
      t[[n]] <- as.vector(t[[n]])
      names(t[[n]]) <- tmp_names
    }
  }

  # TODO: This function does not respect the order in
  # the factored variable. This could potentially be
  # a problem although probably more theoretical
  all_row_names <- c()
  for (n in names(t)) {
    all_row_names <- union(all_row_names, names(t[[n]]))
  }

  # No rownames exist, this occurs often
  # when there is only one row and that row doesn't
  # have a name
  if (is.null(all_row_names)) {
    return(t)
  }

  # The missing NA element should always be last
  if (any(is.na(all_row_names))) {
    all_row_names <- append(all_row_names[is.na(all_row_names) == FALSE], NA)
  }

  ret <- list()
  for (n in names(t)) {
    # Create an empty array
    ret[[n]] <- rep(missing_value, times = length(all_row_names))
    names(ret[[n]]) <- all_row_names
    # Loop and add all the values
    for (nn in all_row_names) {
      if (nn %in% names(t[[n]])) {
        if (is.na(nn)) {
          ret[[n]][is.na(names(ret[[n]]))] <- t[[n]][is.na(names(t[[n]]))]
        } else {
          ret[[n]][nn] <- t[[n]][nn]
        }
      }
    }
  }

  return(ret)
}
