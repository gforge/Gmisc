#' An R alternative to the lodash \code{get} in JavaScript
#'
#' This is a handy function for retrieving items deep in a nested structure
#' without causing error if not found
#'
#' @param sourceList The \code{list()}/\code{c()} that is to be searched for the element
#' @param path A string that can be separated by [,] or ., the string \code{"elementname1.1.elementname"}
#'  is equivalent to \code{"elementname1[[1]]]elementname"}. Note that the function doesn't check
#'  the validity of the path - it only separates and tries to address that element with `[[]]`.
#' @param default The value to return if the element isn't found
#'
#' @return Returns a sub-element from \code{sourceList} or the \code{default} value.
#' @importFrom stringr str_detect str_split str_replace_all
#' @examples
#' source <- list(a = list(b = 1, `odd.name` = 'I hate . in names', c(1,2,3)))
#' retrieve(source, "a.b")
#' retrieve(source, "a.b.1")
#' retrieve(source, "a.odd\\.name")
#' retrieve(source, "a.not_in_list")
#'
#' @family lodash similar functions
#' @export
retrieve <- function(sourceList, path, default = NA) {
  path <- str_split(path, "(?<!\\\\)[\\[\\].]+")[[1]]
  for (element in path) {
    element <- str_replace_all(element, "\\\\", "")
    if (str_detect(element, "^[0-9]+$")) {
      element <- as.numeric(element)
      if (length(sourceList) < element || element <= 0) {
        return(default)
      }
    } else if (!(element %in% names(sourceList))) {
      return(default)
    }

    sourceList <- sourceList[[element]]
  }
  return(sourceList)
}

#' An R alternative to the lodash \code{has} in JavaScript
#'
#' This is a handy function for checking if item exist in a nested structure
#'
#' @param sourceList The \code{list()}/\code{c()} that is to be searched for the element
#' @param path A string that can be separated by [,] or ., the string \code{"elementname1.1.elementname"}
#'  the validity of the path - it only separates and tries to address that element with `[[]]`.
#'
#' @return Returns a boolean.
#' @importFrom stringr str_detect str_split
#' @examples
#' has(list(a = list(b = 1)), "a.b")
#'
#' @family lodash similar functions
#' @export
has <- function(sourceList, path) {
  uniqueNotFoundId <- "__@GMISC_NOT_FOUND@__"
  value <- retrieve(sourceList, path, default = uniqueNotFoundId)
  if (length(value) > 1) {
    return(TRUE)
  }

  return(value != uniqueNotFoundId)
}
