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
#' @param txt_gp The \code{\link[grid]{gpar}} style to apply to the text. Set \code{boxPropGrobTxt} option
#'  if you want to customize all the boxes at once.
#' @param txt_left_gp The \code{\link[grid]{gpar}} style to apply to the left text. Set
#' \code{boxPropGrobLeftTxt} option if you want to customize all the boxes at once.
#' @param txt_right_gp The \code{\link[grid]{gpar}} style to apply to the right text. Set
#' \code{boxPropGrobRightTxt} option if you want to customize all the boxes at once.
#' @param box_left_gp The \code{\link[grid]{gpar}} style to apply to the left box. Set
#' \code{boxPropGrobLeft} option if you want to customize all the boxes at once.
#' @param box_right_gp The \code{\link[grid]{gpar}} style to apply to the right box. Set
#' \code{boxPropGrobRight} option if you want to customize all the boxes at once.
#' @param box_highlight_gp The \code{\link[grid]{gpar}} style to apply to the background
#' of the main label. Set \code{boxPropGrobHighlight} option if you want to customize
#' all the boxes at once.
#'
#' @return A box grob
#' @export
#'
#' @importFrom checkmate assert_class assert checkString checkNumeric assert_number
#' @family flowchart components
#' @examples
#' library(grid)
#' grid.newpage()
#' boxPropGrob("Main label", "Left text", "Right text", prop = .3)
boxPropGrob <- function(label,
                        label_left,
                        label_right,
                        prop,
                        y = unit(.5, "npc"),
                        x = unit(.5, "npc"),
                        width,
                        height,
                        just = "center",
                        bjust = "center",
                        txt_gp = getOption("boxPropGrobTxt",
                          default = gpar(color = "black")
                        ),
                        txt_left_gp = getOption("boxPropGrobLeftTxt",
                          default = gpar(col = "black")
                        ),
                        txt_right_gp = getOption("boxPropGrobRightTxt",
                          default = gpar(col = "black")
                        ),
                        box_left_gp = getOption("boxPropGrobLeft",
                          default = gpar(fill = "#E6E8EF")
                        ),
                        box_right_gp = getOption("boxPropGrobRight",
                          default = gpar(fill = "#FFFDF6")
                        ),
                        box_highlight_gp = getOption("boxPropGrobHighlight",
                          default = gpar(fill = "#ffffff55", col = NA)
                        ),
                        name = NULL) {
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
  spacer <- list(
    x = unit(2, "mm"),
    y = unit(5, "mm")
  )

  label_measures <- list(width = 0,
                         height = 0)

  main_label <- NULL
  if (!missing(label)) {
    tg <- textGrob(
      label = label,
      x = prGetX4Txt(just, txt_padding), y = .5,
      just = just,
      name = "label",
      gp = txt_gp
    )
    label_measures$height <- (prCnvrtX(grobHeight(tg)) + 2)
    label_measures$width <- prCnvrtX(grobWidth(tg))

    main_label <- grobTree(
      name = "main_label",
      gList(roundrectGrob(gp = box_highlight_gp), tg),
      vp = viewport(height = unit(label_measures$height, "mm"),
                    y = 1,
                    just = "top")
    )

  }

  sublabel <- list()
  if (!missing(label_left)) {
    sublabel$left <- prBuildSubLabel(label = label_left,
                                     prop = prop,
                                     txt_gp = txt_left_gp,
                                     side = "left")
  }

  if (!missing(label_right)) {
    sublabel$right <- prBuildSubLabel(label = label_right,
                                      prop = prop,
                                      txt_gp = txt_right_gp,
                                      side = "right")
  }

  space_between_label_and_sublabel <- unit(0, "mm")
  if (missing(height)) {
    add_height <- 0
    if (length(sublabel) > 0) {
      add_height <- max(sapply(sublabel, function(x) attr(x, "height")))
    }

    if (label_measures$height > 0 && add_height > 0) {
      space_between_label_and_sublabel <- spacer$y
    }

    height <- unit(label_measures$height + add_height, "mm") + space_between_label_and_sublabel + txt_padding + txt_padding
  }

  if (length(sublabel) == 0) {
    sublabel <- NULL
  } else {
    sublabel <- do.call(gList, sublabel)
  }

  txt <- grobTree(
    gList(
      main_label,
      grobTree(
        sublabel,
        name = "sublabel",
        vp = viewport(
          y = 0, just = "bottom",
          height = unit(1, "npc") - unit(label_measures$height, "mm") - space_between_label_and_sublabel
        )
      )
    ),
    vp = viewport(
      height = unit(1, "npc") - txt_padding - txt_padding,
      width = unit(1, "npc") - txt_padding - txt_padding,
      clip = "on"
    ),
    name = name,
    cl = "boxProp"
  )


  # Calculate the width of the grob
  if (missing(width)) {
    total_sublabel_width <- 0
    if (length(sublabel) > 0) {
      total_sublabel_width <- sum(sapply(sublabel, function(x) attr(x, "width"))) +
        prCnvrtX(spacer$x)
    }

    base_width <- max(label_measures$width, total_sublabel_width  )

    # Due to the proportions we may need to force a larger window
    get_min_width <- function(sl) {
      if (is.null(sl)) {
        return(0)
      }

      attr(sl, "width") +
        prCnvrtX(txt_padding) +
        prCnvrtX(spacer$x)
    }

    min_left <- get_min_width(sublabel$left)
    if (base_width * prop < min_left) {
      base_width <- min_left / prop
    }

    min_right <- get_min_width(sublabel$right)
    if (base_width * (1 - prop) < min_right) {
      base_width <- min_right / (1 - prop)
    }

    width <- unit(base_width, "mm") + txt_padding + txt_padding
  }


  vp_args <- list(
    x = x,
    y = y,
    width = width,
    height = height,
    just = bjust
  )

  gl <- grobTree(
    roundrectGrob(
      gp = box_left_gp,
      width = width, x = 0, just = "left",
      vp = viewport(x = 0, just = "left", width = prop, clip = "on")
    ),
    roundrectGrob(
      gp = box_right_gp,
      width = width, x = 1, just = "right",
      vp = viewport(x = 1, just = "right", width = 1 - prop, clip = "on")
    ),
    txt,
    vp = do.call(viewport, vp_args),
    cl = "box"
  )

  xtr_coordinate_fns <- list(
    left_x = function(x, width, half_width) x - half_width + unit(prCnvrtX(width) * prop / 2, "mm"),
    right_x = function(x, width, half_width) {
      x - half_width +
        unit(prCnvrtX(width) * prop + prCnvrtX(width) * (1 - prop) / 2, "mm")
    },
    prop_x = function(x, width, half_width) {
      x - half_width + unit(prCnvrtX(width) * prop, "mm")
    }
  )

  structure(gl,
    coords = prCreateBoxCoordinates(viewport_data = vp_args, extra_coordinate_functions = xtr_coordinate_fns),
    extra_coordinate_functions = xtr_coordinate_fns,
    viewport_data = vp_args
  )
}


#' Add a sub-label to \code{boxPropGrob}
#'
#' @param prop The proportion
#' @param label The text of the label
#' @param txt_gp The style as defined by \code{\link[grid]{gpar}()}
#' @param side The side that the label belongs to
#' @return A \code{textGrob} with he additional attributes \code{width} and \code{height}.
prBuildSubLabel <- function(label,
                            prop,
                            txt_gp,
                            side = c("left", "right")) {
  side <- match.arg(side)
  if (side == "left") {
    x = prop / 2
    width = prop
  } else {
    x <- prop + (1 - prop) / 2
    width <- 1 - prop
  }

  vp <- viewport(x = x, width = width,
                 y = 1,
                 just = c('centre', 'top'),
                 name = paste0("vp_label_", side))
  tg <- textGrob(
    label = label,
    x = .5, y = 0.5,
    just = "center",
    vp = vp,
    name = paste0("label_", side),
    gp = txt_gp
  )
  structure(tg,
            height = prCnvrtY(grobHeight(tg) + unit(.5, units = "lines")),
            width = prCnvrtX(grobWidth(tg) + unit(.5, units = "lines")))
}