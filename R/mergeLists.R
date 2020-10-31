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
#' @param sortNames Set to false if you don't want the names to be sorted.
#'  This can also be done via the option `Gmisc.mergeList.sort`.
#' @return Returns a list with all the given lists.
#'
#' @example  inst/examples/mergeLists_example.R
#' @export
mergeLists <- function(..., lapplyOutput = NULL, sortNames = getOption("Gmisc.mergeList.sort", default = TRUE)) {
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
  if (is.null(names(m))) {
    for (i in 2:length(lst)) {
      nextList <- lst[[i]]
      if (!is.null(names(nextList))) {
        stop("The list ", nextList, " has names - cannot merge list with no-name elements with named elements.",
             " Handling the merge becomes unpredictable and unsafe. You should consider writing your own routine for this.")
      }
      m <- mapply(mergeLists_internal, m, nextList, SIMPLIFY = FALSE)
    }
  } else {
    for (i in 2:length(lst)) {
      nextList <- lst[[i]]
      if (is.null(names(nextList))) {
        stop("The list ", nextList, " does not have names - cannot merge list with named elements with no-named elements.",
             " Handling the merge becomes unpredictable and unsafe. You should consider writing your own routine for this.")
      }
      elementNames <- union(names(m), names(nextList))
      if (sortNames) {
        elementNames <- sort(elementNames)
      }
      for (n in elementNames) {
        if (is.null(m[[n]])) {
          m[[n]] <- nextList[[n]]
        } else if (!is.null(m[[n]])) {
          m[[n]] <- mergeLists_internal(o_element = m[[n]], n_element = nextList[[n]])
        }
      }
    }
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
  if (is.list(o_element) != is.list(n_element)) {
    stop("Can't mix list and non-lists check out the sections containing ", o_element, " and ", n_element)
  }

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
    if (is.null(dim(n_element)) && is.null(dim(o_element))) {
      return(c(o_element, n_element))
    }

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
