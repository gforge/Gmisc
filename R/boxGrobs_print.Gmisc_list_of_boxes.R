#' Output boxes
#'
#' Outputs a list of boxes as produced by either the
#' spread or align functions for boxGrobs.
#'
#' @param x A list of a set of [`boxGrob`]/[`boxPropGrob`] to plot
#' @param ... Ignored argument
#'
#' @details Boxes marked with the `draw_on_top` attribute (for example via
#'  `insert(..., on_top = TRUE)`) are drawn last — after the other boxes and
#'  after any stored connections — so they remain visible even when they
#'  overlap surrounding boxes. All other boxes keep their list order.
#' @export
print.Gmisc_list_of_boxes <- function(x, ...) {
  draw_element <- function(box) {
    if (is.grob(box)) {
      grid.draw(box)
    } else if (inherits(box, "Gmisc_list_of_boxes")) {
      for (i in 1:length(box)) {
        print(box[[i]])
      }
    } else if (is.list(box)) {
      # treat plain lists containing boxes as nested Gmisc_list_of_boxes for printing
      print(prExtendClass(box, "Gmisc_list_of_boxes"))
    } else {
      stop("Element is not a grob or a Gmisc_list_of_boxes", class(box))
    }
  }

  on_top <- vapply(x, function(box) isTRUE(attr(box, "draw_on_top")), logical(1))

  # Draw regular boxes in list order
  for (box in x[!on_top]) {
    draw_element(box)
  }

  # Draw stored connections
  conns <- attr(x, "connections")
  if (!is.null(conns)) {
    for (cg in conns) {
      grid.draw(cg)
    }
  }

  # Draw overlay boxes last so they stay on top of boxes and connections
  for (box in x[on_top]) {
    draw_element(box)
  }

  invisible(x)
}
