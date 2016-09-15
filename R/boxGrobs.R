#' Create a grid box with text
#'
#'
#'
#' @param label
#' @param y
#' @param x
#' @param just
#' @param bjust
#' @param fill
#'
#' @return
#' @export
#'
#' @rdname box
#' @importFrom grid
#' @examples
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
  height <- grobHeight(txt) + txt_padding + txt_padding
  width <- grobWidth(txt) + txt_padding + txt_padding


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
            left = x - half_width,
            right = x + half_width,
            bottom = y - half_height,
            top = y + half_height,
            x = x,
            y = y)
}

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
  length(strsplit(as.character(str), "\n")[[1]]) %>%
    unit("lines") %>%
    prCnvrtY
}

#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @rdname box
#' @export
print.box <- function(x, ...)
{
  grid.draw(x, ...)
}

#' @rdname box
#' @export
plot.box <- print.box


boxSplitGrob <- function (label,
                          label_left = "",
                          label_right = "",
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
                          box_gp = gpar(fill=NA),
                          box_left_gp = gpar(fill="#81BFD4", col = NA),
                          box_right_gp = gpar(fill="#D8F0D1", col = NA),
                          box_highlight_gp = gpar(fill="#ffffff55", col=NA)) {

  assert(
    checkString(label),
    checkNumeric(label)
  )
  assert(
    checkString(label_left),
    checkNumeric(label_left)
  )
  assert(
    checkString(label_right),
    checkNumeric(label_right)
  )
  assert_unit(y)
  assert_unit(x)
  assert_unit(width)
  assert_unit(height)
  assert_just(just)
  assert_just(bjust)
  assert_class(txt_gp, "gpar")
  assert_class(txt_left_gp, "gpar")
  assert_class(txt_right_gp, "gpar")
  assert_class(box_gp, "gpar")
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
  height <- unit(base_txt_height + add_height, "mm") + spacer$y + txt_padding + txt_padding

  txt <- grobTree(
    gList(roundrectGrob(gp=box_highlight_gp,
                        vp=viewport(height = unit(base_txt_height + 2, "mm"), y = 1, just="top")),
          textGrob(label = label, x = prGetX4Txt(just, txt_padding), y = .5,
                   just = just,
                   vp = viewport(height=unit(base_txt_height + 2, "mm"), y = 1, just="top")),
          textGrob(label = label_left, x = .5, y = 1,
                   just = "center", vjust = 1,
                   vp = viewport(x = prop/2, width=prop,
                                 y = 0,
                                 just = "bottom",
                                 height = unit(1, "npc") - unit(base_txt_height, "mm") - spacer$y)),
          textGrob(label = label_right, x = .5, y = 1,
                   just = "center", vjust = 1,
                   vp = viewport(x = prop + (1-prop)-(1-prop)/2, width=1-prop,
                                 y = 0,
                                 just = "bottom",
                                 height = unit(1, "npc") - unit(base_txt_height, "mm") - spacer$y))),
    vp = viewport(height = unit(1, "npc") - txt_padding - txt_padding,
                  width = unit(1, "npc") - txt_padding - txt_padding,
                  clip="on"))


  # Calculate the width of the grob
  base_width <-
    max(prCnvrtX(grobWidth(txt)),
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
  if (base_width * prop < min_right) {
    base_width <- min_right / prop
  }

  width <- unit(base_width, "mm") + txt_padding + txt_padding

  half_height <- unit(prCnvrtY(height)/2, "mm")
  half_width <- unit(prCnvrtX(width)/2, "mm")

  radius = unit(2, "mm")
  gl <- grobTree(roundrectGrob(x = prop/2, width=prop, gp=box_left_gp, r=radius),
                 rectGrob(x = 3*prop/4, width=2*prop/4, gp=box_left_gp),
                 roundrectGrob(x = prop + (1-prop)/2, width=1-prop, gp=box_right_gp, r=radius),
                 rectGrob(x = prop + prop/4, width=2*prop/4, gp=box_right_gp),
                 roundrectGrob(x=.5, y=.5,
                               gp = box_gp,
                               width = unit(1, "npc"),
                               height= unit(1, "npc"),
                               r=radius,
                               just = bjust),
                 txt,
                 vp = viewport(x = x, y = y, width = width, height = height))

  structure(gl,
            class=c("box", class(gl)),
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
}

connectBoxes <- function(
  start,
  end,
  type = c("vertical", "horizontal", "L"),
  subelmnt = "")
{
  type = match.arg(type)
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

  grid.lines(x = line$x,
             y = line$y,
             gp = gpar(fill="black"),
             arrow = arrow(ends = "last", type = "closed"))
}

