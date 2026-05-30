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
#' @param txt_padding Padding between text and box border. Can be provided as a
#'  \code{\link[grid]{unit}} or numeric (interpreted as millimetres). You can set
#'  a global default with \code{options(boxGrobTxtPadding = ...)}.
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
#' @param badge_label Optional badge text to display at the top of the box.
#' @param badge_position The position of the badge: "top" (currently only "top" supported).
#' @param badge_gp The \code{\link[grid]{gpar}} style to apply to the badge background.
#' @param badge_txt_gp The \code{\link[grid]{gpar}} style to apply to the badge text.
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
                    txt_padding = getOption("boxGrobTxtPadding", default = unit(6 * ifelse(is.null(txt_gp$cex), 1, txt_gp$cex), "mm")),
                    box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                    box_fn = roundrectGrob,
                    name = NULL,
                    badge_label = NULL,
                    badge_position = "top",
                    badge_gp = gpar(fill = "steelblue", col = NA),
                    badge_txt_gp = gpar(col = "white", cex = 0.7)) {
  assert(
    checkString(label),
    checkNumeric(label),
    is.language(label)
  )
  assert_unit(y)
  assert_unit(x)
  assert_unit(width)
  assert_unit(height)
  if (is.numeric(txt_padding)) {
    txt_padding <- unit(txt_padding, "mm")
  }
  assert_unit(txt_padding)
  assert_just(just)
  assert_just(bjust)
  assert_class(txt_gp, "gpar")
  assert_class(box_gp, "gpar")


  x <- prAsUnit(x)
  y <- prAsUnit(y)

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

  # Build the inner gList. When a badge is requested, add badge grobs inside the
  # same viewport so that all x/y coordinates share the same coordinate system
  # (viewport npc + absolute mm offsets) — no compound-unit misalignment.
  inner <- if (!is.null(badge_label)) {
    badge_h   <- unit(4.5, "mm")
    badge_w   <- unit(11,  "mm")
    badge_pad <- unit(1.5, "mm")   # gap from the left edge of the box

    # Center of the badge pill in viewport coordinates.
    # unit(0, "npc") = left edge of viewport; badge_w * 0.5 + badge_pad moves
    # the center rightward so the pill's left side sits badge_pad from the box edge.
    b_x <- unit(0, "npc") + badge_w * 0.5 + badge_pad
    b_y <- unit(1, "npc")   # exactly the top border of the box

    badge_bg <- roundrectGrob(
      x = b_x, y = b_y,
      width = badge_w, height = badge_h,
      r = unit(2, "mm"),   # rounded corners — standard badge shape
      just = "center",
      gp = badge_gp
    )
    badge_lbl <- textGrob(
      label = as.character(badge_label),
      x = b_x, y = b_y,
      just = "center",
      gp = badge_txt_gp
    )
    gList(rect, txt, badge_bg, badge_lbl)
  } else {
    gList(rect, txt)
  }

  gl <- grobTree(
    inner,
    vp = do.call(viewport, vp_args),
    name = name,
    cl = "box"
  )

  structure(gl,
    coords = prCreateBoxCoordinates(viewport_data = vp_args),
    viewport_data = vp_args
  )
}
