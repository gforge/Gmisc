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
#' @param box_gp The \code{\link[grid]{gpar}} style to apply to the box function of `box_fn` below.
#' @param box_fn Function to create box for the text. Parameters of `x=0.5`, `y=0.5` and `box_gp` will
#'  be passed to this function and return a \code{grob} object.
#' @seealso The package provides several convenience shape helpers that can be
#' passed to `boxGrob(..., box_fn = ...)`: \code{boxDiamondGrob},
#' \code{boxEllipseGrob}, \code{boxRackGrob}, \code{boxServerGrob},
#' \code{boxDatabaseGrob}, \code{boxDocumentGrob}, \code{boxDocumentsGrob}, and
#' \code{boxTapeGrob}. For examples see the vignette: \code{vignette("Grid-based_flowcharts", package = "Gmisc")}.
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
#' # Note: grid functions are explicitly namespaced in examples to avoid
#' # relying on attaching the grid package in R CMD check.
#' grid::grid.newpage()
#' boxGrob("My box")
boxGrob <- function(label,
                    y = unit(.5, "npc"),
                    x = unit(.5, "npc"),
                    width,
                    height,
                    just = "center",
                    bjust = "center",
                    txt_gp = getOption("boxGrobTxt", default = gpar(
                      color = "black",
                      cex = 1
                    )),
                    box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                    box_fn = roundrectGrob,
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

  # Slightly larger default padding so specialized shapes (diamond/ellipse)
  # have breathing room and text doesn't sit too tight against the border.
  txt_padding <- unit(6 * ifelse(is.null(txt_gp$cex), 1, txt_gp$cex), "mm")

  # Call the box function early to collect any suggested padding attributes
  # (e.g., diamonds may request extra padding). This allows the padding to
  # influence text layout and the computed box width/height.
  rect <- do.call(box_fn, list(x = .5, y = .5, gp = box_gp))
  extra_pad <- attr(rect, "box_fn_padding")
  if (!is.null(extra_pad)) {
    tryCatch(
      {
        txt_padding <- txt_padding + extra_pad
      },
      error = function(e) {
        # ignore silently if attribute is not a proper unit
      }
    )
  }

  # Create text grob using the (possibly) adjusted padding
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
