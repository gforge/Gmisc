#' Deprecated function(s) in \pkg{Gmisc}
#'
#' The \code{forestplot2} has moved to \pkg{forestplot} package and changed name to \code{\link[forestplot]{forestplot}}
#'
#' @param ... Parameters to be passed to the new version of the function
#' @export
#' @keywords internal
#' @import forestplot
#' @rdname Gmisc-deprecated
#' @name Gmisc-deprecated
#' @docType package
forestplot2 <- function(...) {
  .Deprecated(new = "forestplot",
              package = "forestplot",
              old = "forestplot2",
              msg = "The forestplot2 has moved to a separate package as of 1.0, the 'forestplot' package")
  forestplot(...)
}
