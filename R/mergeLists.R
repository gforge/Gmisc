#' Merging of multiple lists
#'
#' The merge allows for a recursive component
#' where the lists are compared on the subelement.
#' If one does not contain that element it will get NA
#' in for those parameters.
#'
#' @param ... Any number of lists that you want to merge
#' @param lapplyOutput The \code{\link[base]{lapply}} function outputs a number
#'  of lists and this is for specifically merging all of those.
#' @return Returns a list with all the given lists.
#'
#' @example  inst/examples/mergeLists_example.R
#' @export
mergeLists <- function(..., lapplyOutput = NULL) {
  lst <- list(...)
  if (is.list(lapplyOutput)) {
    lst <- c(lst, lapplyOutput)
  }

  if (length(lst) == 1) {
    warning(
      "You have provided the function with only one list,",
      " are you sure you didn't intend to use the lapplyOutput argument?"
    )
    return(lst)
  } else if (length(lst) == 0) {
    stop("No lists to merge")
  }

  m <- lst[[1]]
  for (i in 2:length(lst)) {
    m <- mapply(mergeLists_internal, m, lst[[i]], SIMPLIFY = FALSE)
  }
  return(m)
}

#' A helper function for mergeLists that does the actual work.
#'
#' @param o_element Old element
#' @param n_element New element to add to the old element
#' @return list with the two merged
#'
#' @keywords internal
mergeLists_internal <- function(o_element, n_element) {
  if (is.list(n_element)) {
    # Fill in non-existant element with NA elements
    if (length(n_element) != length(o_element)) {
      n_unique <- names(n_element)[!names(n_element) %in% names(o_element)]
      if (length(n_unique) > 0) {
        for (n in n_unique) {
          if (is.matrix(n_element[[n]])) {
            o_element[[n]] <- matrix(NA,
              nrow = nrow(n_element[[n]]),
              ncol = ncol(n_element[[n]])
            )
          } else {
            o_element[[n]] <- rep(NA,
              times = length(n_element[[n]])
            )
          }
        }
      }

      o_unique <- names(o_element)[!names(o_element) %in% names(n_element)]
      if (length(o_unique) > 0) {
        for (n in o_unique) {
          if (is.matrix(n_element[[n]])) {
            n_element[[n]] <- matrix(NA,
              nrow = nrow(o_element[[n]]),
              ncol = ncol(o_element[[n]])
            )
          } else {
            n_element[[n]] <- rep(NA,
              times = length(o_element[[n]])
            )
          }
        }
      }
    }

    # Now merge the two lists
    return(mergeLists(
      o_element,
      n_element
    ))
  }
  if (length(n_element) > 1) {
    new_cols <- ifelse(is.matrix(n_element), ncol(n_element), length(n_element))
    old_cols <- ifelse(is.matrix(o_element), ncol(o_element), length(o_element))
    if (new_cols != old_cols) {
      stop(
        "Your length doesn't match on the elements,",
        " new element (", new_cols, ") ! = ",
        " old element (", old_cols, ")"
      )
    }
  }

  if (length(n_element) > 1) {
    return(rbind(o_element,
      n_element,
      deparse.level = 0
    ))
  }
  return(c(
    o_element,
    n_element
  ))
}