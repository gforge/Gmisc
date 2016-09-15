#' Checks if unit or number 0-1
#'
#' @param var Variable 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importfrom checkClass assert checkmate
assert_unit <- function(var, allow_missing = TRUE){
  if (missing(var) && allow_missing)
    return()

  assert(
    checkClass(var, "unit"),
    checkNumeric(var, lower = 0, upper = 1),
    .var.name = dep_var(var)
  )
}

#' Assert that its a string or number 0-1
#'
#' @param var Varible 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importfrom checkString assert checkmate
assert_just <- function(var, allow_missing = TRUE){
  if (missing(var) && allow_missing)
    return()

  assert(
    checkString(var, pattern = "left|right|top|bottom"),
    checkNumeric(var, lower = 0, upper = 1),
    .var.name = dep_var(var))
}


#' Assert that its a string or number 0-1
#'
#' @param var Varible 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importfrom checkString assert checkmate
assert_just <- function(var, allow_missing = TRUE){
  if (missing(var) && allow_missing)
    return()

  assert(
    checkString(var, pattern = "left|right|top|bottom"),
    checkNumeric(var, lower = 0, upper = 1),
    .var.name = dep_var(var)
  )
}

#' Assert valid color
#'
#' @param var Varible 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importfrom checkString assert checkmate
assert_just <- function(var, allow_missing = TRUE){
  if (missing(var) && allow_missing)
    return()

  assert(
    checkString(var, pattern = "left|right|top|bottom|center"),
    checkNumeric(var, lower = 0, upper = 1),
    .var.name = dep_var(var)
  )
}

dep_var <- function(x) {
  nn <- substitute(x)
  on <- do.call("substitute", list(as.name(nn), parent.frame(1)))
  paste0(deparse(deparse(on), width.cutoff = 500),
         collapse = "\n")
}

