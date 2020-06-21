#' A simple thing to keep the attributes
#'
#' Skips the attributes that the to object already has
#' to avoid overwriting dim and other important attributes
#'
#' @param from The from object
#' @param to The to object
#' @param attr2skip An optional lists of attributes
#'  that you may want to avoid having copied
#' @param attr2force An optional lists of attributes
#'  that you may want to force copy even if they already
#'  exist in the new object
#' @return \code{object} The \code{to} argument object
#'
#' @examples
#' a <- "test"
#' attr(a, 'wow') <- 1000
#' b <- a
#' b <- copyAllNewAttributes(a, b)
#' print(attr(b, 'wow'))
#'
#' @export
copyAllNewAttributes <- function(from, to, attr2skip = c(), attr2force = c()) {
  for (name in names(attributes(from))) {
    # Don't overwrite attributes
    if (!name %in% names(attributes(to)) &
          !name %in% attr2skip |
          name %in% attr2force)
      attr(to, name) <- attr(from, name)
  }
  return (to)
}
