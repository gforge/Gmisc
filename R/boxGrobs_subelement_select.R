# Internal helper: resolve subelement selectors into canonical literal paths
#
# Handles three input shapes:
#   - A `stringr_regex` object (from `stringr::regex()`)  -> match top-level names of `x`
#   - A plain list                                         -> resolve each element recursively
#   - Anything else (character, numeric, c("name", idx))  -> wrap in list() as a single literal path
#
# The recursive list branch deliberately does NOT split atomic vectors
# (e.g. `c("groups", 1)`) into individual segments -- those remain one path.
#
# @param subelement A path, list of paths, or `stringr::regex()` selector.
# @param x         The named box list being operated on.
# @return A list of atomic path vectors ready for the callers' path-resolution logic.
# @keywords internal
# @noRd
prResolveSubelementSelector <- function(subelement, x) {
  if (inherits(subelement, "stringr_regex")) {
    nms <- names(x)

    if (is.null(nms)) {
      warning(
        "Cannot use regex subelement selection because the target box list has no names.",
        call. = FALSE
      )
      return(list())
    }

    is_match <- stringr::str_detect(nms, subelement)
    is_match[is.na(is_match)] <- FALSE
    matched <- nms[is_match]

    if (length(matched) == 0) {
      warning(
        sprintf(
          "Regex subelement selector `%s` matched no top-level subelement names.",
          as.character(subelement)
        ),
        call. = FALSE
      )
      return(list())
    }

    return(as.list(matched))
  }

  # A plain list: resolve each element independently so that
  # mixed lists like list(stringr::regex("^groups"), "other_exact") work,
  # while atomic vectors like c("groups", 1) inside a list remain a single path.
  if (is.list(subelement)) {
    return(unlist(
      lapply(subelement, prResolveSubelementSelector, x = x),
      recursive = FALSE
    ))
  }

  # Literal path: a character string, a numeric index, or a multi-segment
  # vector like c("groups", 1).  Wrap in a list to produce one path entry.
  list(subelement)
}
