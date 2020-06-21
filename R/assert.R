#' Checks if unit or number 0-1
#'
#' @param var Variable 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importFrom checkmate assert checkClass checkNumeric
assert_unit <- function(var, allow_missing = TRUE) {
  if (missing(var) && allow_missing) {
    return()
  }

  assert(
    checkClass(var, "unit"),
    checkNumeric(var, lower = 0, upper = 1),
    .var.name = dep_var(var)
  )
}

#' Assert that its a string or number
#'
#' @param var Varible 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importFrom checkmate assert checkString checkNumeric
assert_label <- function(var, allow_missing = TRUE) {
  if (missing(var) && allow_missing) {
    return()
  }

  assert(
    checkString(var),
    checkNumeric(var),
    .var.name = dep_var(var)
  )
}

#' Assert valid color
#'
#' @param var Variable 2 check
#' @param allow_missing Allow missing variables
#'
#' @return void
#' @rdname assert
#' @keywords internal
#' @importFrom checkmate assert checkCharacter checkNumeric
assert_just <- function(var, allow_missing = TRUE) {
  if (missing(var) && allow_missing) {
    return()
  }

  assert(
    checkCharacter(var, pattern = "^left|right|top|bottom|center|centre$", max.len = 2),
    checkNumeric(var, lower = 0, upper = 1, max.len = 2),
    .var.name = dep_var(var)
  )
}

dep_var <- function(x) {
  nn <- substitute(x)
  on <- do.call("substitute", list(as.name(nn), parent.frame(1)))
  paste0(deparse(deparse(on), width.cutoff = 500),
    collapse = "\n"
  )
}