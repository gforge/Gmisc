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
    if (is.grob(box)) {
      grid.draw(box)
    } else if (inherits(box, "Gmisc_list_of_boxes")) {
      for (i in 1:length(box)) {
        print(box[[i]])
      }
    } else {
      stop("Element is not a grob or a Gmisc_list_of_boxes", class(box))
    }
  }
}
