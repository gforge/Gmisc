#' Output boxes
#' 
#' Outputs a list of boxes as produced by either the
#' spread or align functions for boxGrobs.
#' 
#' @param x A list of a set of [`boxGrob`]/[`boxPropGrob`] to plot
#' @param ... Ignored argument
#' @export
print.Gmisc_list_of_boxes <- function(x, ...) {
  for (box in x) {
    grid.draw(box)
  }
}
