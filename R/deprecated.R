# Deprecated function names
#' See \code{\link{txtMergeLines}}
#' 
#' @param ... passed onto \code{\link{txtMergeLines}}
#' @examples
#' splitLines4Table("hello", "world")
#' @keywords internal
#' @export
splitLines4Table <- function(...){
  warning("splitLines4Table is deprecated, use txtMergeLines instead")
  txtMergeLines(...)
}