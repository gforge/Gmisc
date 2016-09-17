#' Create a box with text
#'
#' Creates a grob box with text inside it.
#'
#' @param label The label to print
#' @param y The y position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param x The x position to put the box at. Can be either in \code{npc} (i.e. 0-1) or a \code{\link[grid]{unit}}.
#' @param width The box autosizes but you can force by specifying the width
#' @param height The box autosizes but you can force by specifying the height
#' @param just The justification for the text: left, center or right.
#' @param bjust The justification for the box: left, center, right, top or bottom.
#'  See the \code{just} option for the \code{\link[grid]{viewport}}
#' @param txt_gp The \code{\link[grid]{gpar}} style to apply to the text.
#' @param box_gp The \code{\link[grid]{gpar}} style to apply to the box.
#'
#' @return A grob
#' @export
#'
#' @rdname box
#' @importFrom checkmate assert_class assert checkString checkNumeric
#' @family box-functions
#' @examples
#' library(grid)
#' grid.newpage()
#' boxGrob("My box")
boxGrob <- function (label,
                     y = unit(.5, "npc"),
                     x = unit(.5, "npc"),
                     width,
                     height,
                     just = "center",
                     bjust = "center",
                     txt_gp = gpar(color="black"),
                     box_gp = gpar(fill="#D8F0D1")) {

  assert(
    checkString(label),
    checkNumeric(label)
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
  txt <- textGrob(label = label,
                  x = prGetX4Txt(just, txt_padding), y = .5,
                  just = just, gp = txt_gp)

  if (missing(height))
    height <- grobHeight(txt) + txt_padding + txt_padding
  else
    height <- prAsUnit(height)

  if (missing(width))
    width <- grobWidth(txt) + txt_padding + txt_padding
  else
    width <- prAsUnit(width)


  rect <- roundrectGrob(x=.5, y=.5, gp = box_gp)
  gl <- grobTree(gList(rect,
                       txt),
                 vp = viewport(x = x, y = y,
                               width = width, height = height,
                               just = bjust))

  # Adjust center depending on the viewport position
  x <- prAdjustXPos(bjust, x, width)
  y <- prAdjustYPos(bjust, y, height)
  half_width <- unit(prCnvrtX(width)/2, "mm")
  half_height <- unit(prCnvrtY(height)/2, "mm")

  structure(gl,
            class=c("box", class(gl)),
            coords = list(
              left = x - half_width,
              right = x + half_width,
              bottom = y - half_height,
              top = y + half_height,
              x = x,
              y = y))
}


#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname box
#' @export
print.box <- function(x, ...)
{
  grid.draw(x, ...)
}

#' @rdname box
#' @export
plot.box <- print.box


#' Create a box with a color split
#'
#' Creates a grob box with text inside it and a color split in the
#' horizontal axes that allow indicating different proportions. The
#' box can also have a title that spanse the two color areas and
#' that has its own background.
#'
#' @inheritParams boxGrob
#' @param label_left The label for the left area
#' @param label_right The label for the right area
#' @param prop The proportion to split along
#' @param txt_left_gp The \code{\link[grid]{gpar}} style to apply to the left text.
#' @param txt_right_gp The \code{\link[grid]{gpar}} style to apply to the right text.
#' @param box_left_gp The \code{\link[grid]{gpar}} style to apply to the left box.
#' @param box_right_gp The \code{\link[grid]{gpar}} style to apply to the right box.
#' @param box_highlight_gp The \code{\link[grid]{gpar}} style to apply to the background of the main label.
#'
#' @return A box grob
#' @export
#'
#' @importFrom checkmate assert_class assert checkString checkNumeric assert_number
#' @family box-functions
#' @examples
#' library(grid)
#' grid.newpage()
#' boxPropGrob("Main label", "Left text", "Right text", prop = .3)
boxPropGrob <- function (label,
                          label_left,
                          label_right,
                          prop,
                          y = unit(.5, "npc"),
                          x = unit(.5, "npc"),
                          width,
                          height,
                          just = "center",
                          bjust = "center",
                          txt_gp = gpar(color="black"),
                          txt_left_gp = gpar(col="black"),
                          txt_right_gp = gpar(col ="black"),
                          box_left_gp = gpar(fill="#81BFD4"),
                          box_right_gp = gpar(fill="#D8F0D1"),
                          box_highlight_gp = gpar(fill="#ffffff55", col=NA)) {

  assert_label(label)
  assert_label(label_left)
  assert_label(label_right)
  assert_unit(y)
  assert_unit(x)
  assert_unit(width)
  assert_unit(height)
  assert_just(just)
  assert_just(bjust)
  assert_class(txt_gp, "gpar")
  assert_class(txt_left_gp, "gpar")
  assert_class(txt_right_gp, "gpar")
  assert_class(box_left_gp, "gpar")
  assert_class(box_right_gp, "gpar")
  assert_class(box_highlight_gp, "gpar")

  assert_number(prop, lower = 0, upper = 1)

  x <- prAsUnit(x)
  y <- prAsUnit(y)

  txt_padding <- unit(4, "mm")
  spacer <- list(x = unit(2, "mm"),
                 y = unit(5, "mm"))

  base_txt_height <- prConvTxt2Height(label) + 2
  add_height <- max(prConvTxt2Height(label_left), prConvTxt2Height(label_right))
  if (missing(height))
    height <- unit(base_txt_height + add_height, "mm") + spacer$y + txt_padding + txt_padding

  main_label <- NULL
  if (!missing(label)) {
    main_label <- grobTree(gList(roundrectGrob(gp=box_highlight_gp),
                                 textGrob(label = label, x = prGetX4Txt(just, txt_padding), y = .5,
                                          just = just)),
                           vp = viewport(height=unit(base_txt_height + 2, "mm"), y = 1, just="top"))
  }

  sublabel <- list()
  if (!missing(label_left)){
    sublabel <- c(sublabel,
                  list(textGrob(label = label_left, x = .5, y = 1,
                           just = "center", vjust = 1,
                           vp = viewport(x = prop/2, width=prop))))
  }

  if (!missing(label_right)){
    sublabel <- c(sublabel,
                  list(textGrob(label = label_right, x = .5, y = 1,
                                just = "center", vjust = 1,
                                vp = viewport(x = prop + (1-prop)-(1-prop)/2, width=1-prop))))
  }

  if (length(sublabel) == 0) {
    sublabel <- NULL
  }else{
    sublabel <- do.call(gList, sublabel)
  }

  txt <- grobTree(
    gList(
      main_label,
      grobTree(
        sublabel,
        vp = viewport(y = 0, just = "bottom",
                           height = unit(1, "npc") - unit(base_txt_height, "mm") - spacer$y))),
    vp = viewport(height = unit(1, "npc") - txt_padding - txt_padding,
                  width = unit(1, "npc") - txt_padding - txt_padding,
                  clip="on"))


  # Calculate the width of the grob
  base_width <-
    max(prCnvrtX(grobWidth(textGrob(txt))),
        (prCnvrtX(grobWidth(textGrob(label_left))) +
           prCnvrtX(spacer$x)+
           prCnvrtX(grobWidth(textGrob(label_right))))
    )

  # Due to the proportions we may need to force a larger window
  min_left <- prCnvrtX(grobWidth(textGrob(label_left))) +
    prCnvrtX(txt_padding) +
    prCnvrtX(spacer$x)
  min_right <- prCnvrtX(grobWidth(textGrob(label_right))) +
    prCnvrtX(txt_padding) +
    prCnvrtX(spacer$x)
  if (base_width * prop < min_left) {
    base_width <- min_left / prop
  }
  if (base_width * (1 - prop) < min_right) {
    base_width <- min_right / (1-prop)
  }

  if (missing(width))
    width <- unit(base_width, "mm") + txt_padding + txt_padding

  half_height <- unit(prCnvrtY(height)/2, "mm")
  half_width <- unit(prCnvrtX(width)/2, "mm")

  gl <- grobTree(roundrectGrob(gp=box_left_gp,
                               width=width, x=0, just="left",
                               vp=viewport(x=0, just="left", width=prop, clip="on")),
                 roundrectGrob(gp=box_right_gp,
                               width=width, x=1, just="right",
                               vp=viewport(x=1, just="right", width=1-prop, clip="on")),
                 txt,
                 vp = viewport(x = x, y = y, width = width, height = height, just = bjust))
  x <- prAdjustXPos(bjust, x, width)
  y <- prAdjustYPos(bjust, y, height)

  structure(gl,
            class=c("box", class(gl)),
            coords = list(
              left = x - half_width,
              right = x + half_width,
              bottom = y - half_height,
              top = y + half_height,
              x = x,
              left_x = x - half_width +
                unit(prCnvrtX(width)*prop/2, "mm"),
              right_x = x - half_width +
                unit(prCnvrtX(width)*prop +
                       prCnvrtX(width)*(1-prop)/2,
                     "mm"),
              y = y)
            )
}

#' Connect boxes with an arrow
#'
#' The function creates a grob that links two boxes together. It looks for
#' which side it should attach the arrow to in order to avoid
#'
#' @param start The start box
#' @param end The end box
#' @param type How the boxes are stacked. The \code{L} alternative generates a
#'  straight line up/down and then turns to righT/left for connecting with the end.
#' @param subelmnt If we have a split box we can specify the right/left x as the
#'  connector point.
#' @param lty_gp The \code{\link[grid]{gpar}} for the line
#'
#' @return grob with an arrow
#' @export
#'
#' @importFrom checkmate assert_class
#' @family box-functions
#' @rdname connect
#' @example inst/examples/connectGrob_example.R
connectGrob <- function(
  start,
  end,
  type = c("vertical", "horizontal", "L"),
  subelmnt = c("right", "left"),
  lty_gp = gpar(fill="black"))
{
  assert_class(start, "box")
  assert_class(end, "box")
  assert_class(lty_gp, "gpar")

  # We use the coordinates provided with the boxes
  start <- attr(start, "coords")
  end <- attr(end, "coords")

  type = match.arg(type)
  if (missing(subelmnt)) {
    subelmnt <- ""
  } else {
    subelmnt <- sprintf("%s_", match.arg(subelmnt))
  }
  getX4elmnt <- function(elmnt, side = c("left", "right", "x")){
    side = match.arg(side)
    if (side == "x" && !is.null(elmnt[[sprintf("%s%s", subelmnt, side)]])) {
      return (elmnt[[sprintf("%s%s", subelmnt, side)]])
    }else{
      return (elmnt[[side]])
    }
  }
  line <- list()
  cnvrt <- function(val)
    convertHeight(val, unitTo = "mm", valueOnly = TRUE)
  if (type == "L") {
    line$y <- unit.c(start$bottom, end$y, end$y)
    if (cnvrt(getX4elmnt(start, "x")) < cnvrt(getX4elmnt(end, "x"))) {
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$left)
    }else{
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$right)
    }
  }else if (type == "vertical") {
    line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(end, "x"))
    if (cnvrt(start$y) < cnvrt(end$y)) {
      line$y <- unit.c(start$top, end$bottom)
    }else{
      line$y <- unit.c(start$bottom, end$top)
    }
  }else{
    line$y <- unit.c(start$y, end$y)
    if (cnvrt(getX4elmnt(start, "x")) < cnvrt(getX4elmnt(end, "x"))) {
      line$x <- unit.c(start$right, end$left)
    }else{
      line$x <- unit.c(start$left, end$right)
    }
  }

  lg <- linesGrob(x = line$x,
                  y = line$y,
                  gp = gpar(fill="black"),
                  arrow = arrow(ends = "last", type = "closed"))
  structure(lg,
            class = c("connect_boxes", class(lg)))
}

#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param x The grob to print/plot
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname connect
#' @export
print.connect_boxes <- function(x, ...)
{
  grid.draw(x, ...)
}

#' @rdname connect
#' @export
plot.connect_boxes <- print.connect_boxes


prAsUnit <- function(val){
  if (is.unit(val))
    return(val)

  return(unit(val, "npc"))
}
prCnvrtY <- function(val)
  convertHeight(val, unitTo = "mm", valueOnly = TRUE)
prCnvrtX <- function(val)
  convertWidth(val, unitTo = "mm", valueOnly = TRUE)
prAdjustXPos <- function(bjust, x, width) {
  width <- prCnvrtX(width)
  if (any(grepl("left", bjust))) {
    x <- x + unit(width/2, "mm")
  } else if (any(grepl("right", bjust))) {
    x <- x - unit(width/2, "mm")
  } else if (is.numeric(bjust)) {
    x <- x + unit(prCnvrtX(width)*(0.5 - bjust[1]), "mm")
  }
  return (x)
}
prAdjustYPos <- function(bjust, y, height) {
  height <- prCnvrtX(height)
  if (any(grepl("top", bjust))) {
    y <- y - unit(height/2, "mm")
  } else if (any(grepl("bottom", bjust))) {
    y <- y + unit(height/2, "mm")
  } else if (is.numeric(bjust)) {
    y <- y + unit(prCnvrtX(height)*(0.5 - bjust[1]), "mm")
  }
  return (y)
}
prGetX4Txt <- function(just, txt_padding) {
  x <- .5
  if (just == "left"){
    x <- txt_padding
  }else if (just == "right"){
    x <- unit(1, "npc") - txt_padding
  }
  return(x)
}
prConvTxt2Height <- function(str){
  if (missing(str))
    return(0)

  length(strsplit(as.character(str), "\n")[[1]]) %>%
    unit("lines") %>%
    prCnvrtY
}
