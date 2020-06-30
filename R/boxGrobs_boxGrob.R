#' Create a box with text
#'
#' Creates a \code{\link[grid:grid.grob]{grob}} box with text inside it.
#'
#' @param label The label to print - should be a number, text or expression.
#' @param y The y position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param x The x position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param width The box automatically adapts the size but you can force by specifying the width
#' @param height The box automatically adapts the size but you can force by specifying the height
#' @param just The justification for the text: left, center or right.
#' @param bjust The justification for the box: left, center, right, top or bottom.
#'  See the \code{just} option for the \code{\link[grid]{viewport}}
#' @param txt_gp The \code{\link[grid]{gpar}} style to apply to the text. Set \code{boxGrobTxt} option
#'  if you want to customize all the boxes at once.
#' @param box_gp The \code{\link[grid]{gpar}} style to apply to the box. Set \code{boxGrob} option
#'  if you want to customize all the boxes at once.
#' @param name a character identifier for the \code{grob}. Used to find the \code{grob} on the display
#'  list and/or as a child of another grob.
#'
#' @return A grob
#' @export
#'
#' @rdname box
#' @importFrom checkmate assert_class assert checkString checkNumeric
#' @family flowchart components
#' @order 1
#' @examples
#' library(grid)
#' grid.newpage()
#' boxGrob("My box")
boxGrob <- function(label,
                    y = unit(.5, "npc"),
                    x = unit(.5, "npc"),
                    width,
                    height,
                    just = "center",
                    bjust = "center",
                    txt_gp = getOption("boxGrobTxt", default = gpar(color = "black")),
                    box_gp = getOption("boxGrob", gpar(fill = "white")),
                    name = NULL) {
  assert(
    checkString(label),
    checkNumeric(label),
    is.language(label)
  )
  assert_unit(y)
  assert_unit(x)
  assert_unit(width)
  assert_unit(height)
  assert_just(just)
  assert_just(bjust)
  assert_class(txt_gp, "gpar")
  assert_class(box_gp, "gpar")

  x <- prAsUnit(x)
  y <- prAsUnit(y)

  txt_padding <- unit(4, "mm")
  txt <- textGrob(
    label = label,
    x = prGetX4Txt(just, txt_padding), y = .5,
    just = just, gp = txt_gp,
    name = "label"
  )

  if (missing(height)) {
    height <- grobHeight(txt) + txt_padding + txt_padding
  } else {
    height <- prAsUnit(height)
  }

  if (missing(width)) {
    width <- grobWidth(txt) + txt_padding + txt_padding
  } else {
    width <- prAsUnit(width)
  }

  vp_args <- list(
    x = x,
    y = y,
    width = width,
    height = height,
    just = bjust
  )

  rect <- roundrectGrob(x = .5, y = .5, gp = box_gp, name = "rect_around")
  gl <- grobTree(
    gList(
      rect,
      txt
    ),
    vp = do.call(viewport, vp_args),
    name = name,
    cl = "box"
  )

  structure(gl,
    coords = prCreateBoxCoordinates(viewport_data = vp_args),
    viewport_data = vp_args
  )
}
