#' Create a composite box with header and body text
#'
#' Creates a \code{\link[grid:grid.grob]{grob}} box with a centered header and
#' left-justified body text. Useful for flowchart elements that combine a title
#' with bullet points or multi-line descriptions.
#'
#' @param header The header text (centered, typically bold).
#' @param body The body text (left-justified, typically with bullets or multiple lines).
#' @param y The y position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param x The x position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param width The box automatically adapts the size but you can force by specifying the width
#' @param height The box automatically adapts the size but you can force by specifying the height
#' @param bjust The justification for the box: left, center, right, top or bottom.
#'  See the \code{just} option for the \code{\link[grid]{viewport}}
#' @param header_gp The \code{\link[grid]{gpar}} style to apply to the header text.
#'  Defaults to bold text at slightly larger size.
#' @param body_gp The \code{\link[grid]{gpar}} style to apply to the body text.
#' @param box_gp The \code{\link[grid]{gpar}} style to apply to the box function of `box_fn` below.
#' @param box_fn Function to create box for the text. Parameters of `x=0.5`, `y=0.5` and `box_gp` will
#'  be passed to this function and return a \code{grob} object.
#' @param name a character identifier for the \code{grob}. Used to find the \code{grob} on the display
#'  list and/or as a child of another grob.
#'
#' @return A grob with class "box" compatible with all Gmisc flowchart helpers
#'  (\code{\link{coords}}, \code{\link{distance}}, \code{\link{connectGrob}},
#'  \code{\link{spreadVertical}}, \code{\link{alignHorizontal}}, etc.)
#' @export
#'
#' @rdname boxHeaderGrob
#' @importFrom checkmate assert_class assert checkString checkNumeric
#' @family flowchart components
#' @examples
#' library(grid)
#' grid.newpage()
#'
#' # Simple example with header and bullets
#' box1 <- boxHeaderGrob(
#'   header = "Early rehabilitation",
#'   body = paste("0-1 weeks", "• Instruction", "• Pendulum + assisted ROM", sep = "\n"),
#'   header_gp = gpar(fontsize = 11, fontface = "bold"),
#'   body_gp = gpar(fontsize = 9),
#'   box_gp = gpar(fill = "#E8F5E9", col = "#2E7D32")
#' )
#' grid.draw(box1)
#'
#' # Works with all helpers
#' box2 <- boxHeaderGrob(
#'   header = "Delayed rehabilitation",
#'   body = "0-3 weeks\n• Sling immobilisation",
#'   y = 0.3,
#'   box_gp = gpar(fill = "#FFF8E1", col = "#EF6C00")
#' )
#'
#' # Distance calculation works
#' d <- distance(box1, box2, type = "v")
#'
#' # Connect boxes
#' grid.draw(box2)
#' connectGrob(box1, box2, type = "vertical")
boxHeaderGrob <- function(header,
                          body,
                          y = unit(.5, "npc"),
                          x = unit(.5, "npc"),
                          width,
                          height,
                          bjust = "center",
                          header_gp = gpar(fontsize = 10.5, fontface = "bold", color = "black"),
                          body_gp = gpar(fontsize = 9, color = "black"),
                          box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                          box_fn = roundrectGrob,
                          name = NULL) {
  assert(
    checkString(header),
    checkNumeric(header),
    is.language(header)
  )
  assert(
    checkString(body),
    checkNumeric(body),
    is.language(body)
  )
  assert_unit(y)
  assert_unit(x)
  assert_unit(width)
  assert_unit(height)
  assert_just(bjust)
  assert_class(header_gp, "gpar")
  assert_class(body_gp, "gpar")
  assert_class(box_gp, "gpar")

  x <- prAsUnit(x)
  y <- prAsUnit(y)

  # Larger base padding for composite boxes with multi-line content
  txt_padding <- unit(4, "mm")
  margin_after_header <- unit(4, "mm")

  # Call the box function early to collect any suggested padding attributes
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

  # Create header grob (centered at top with padding)
  header_txt <- textGrob(
    label = header,
    x = 0.5,
    y = unit(1, "npc") - txt_padding,
    just = c("center", "top"),
    gp = header_gp,
    name = "header"
  )

  # Create body grob (left-justified at bottom with padding)
  # Position at same padding as box edge for clean alignment
  body_left_margin <- txt_padding
  body_txt <- textGrob(
    label = body,
    x = body_left_margin,
    y = unit(0, "npc") + txt_padding,
    just = c("left", "bottom"),
    gp = body_gp,
    name = "body"
  )

  # Calculate size based on both text grobs
  if (missing(width)) {
    # Width is simply the maximum of text widths plus padding on both sides
    width <- unit.pmax(grobWidth(header_txt), grobWidth(body_txt)) + txt_padding + txt_padding
  } else {
    width <- prAsUnit(width)
  }

  if (missing(height)) {
    height <- grobHeight(header_txt) + grobHeight(body_txt) + txt_padding + txt_padding + margin_after_header
  } else {
    height <- prAsUnit(height)
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
      header_txt,
      body_txt
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
