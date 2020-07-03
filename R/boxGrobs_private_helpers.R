prAsUnit <- function(val) {
  if (is.unit(val)) {
    return(val)
  }

  return(unit(val, "npc"))
}

prCnvrtY <- function(val) {
  convertHeight(val, unitTo = "mm", valueOnly = TRUE)
}

prCnvrtX <- function(val) {
  convertWidth(val, unitTo = "mm", valueOnly = TRUE)
}

prAdjustXPos <- function(bjust, x, width) {
  width <- prCnvrtX(width)
  if (any(grepl("left", bjust))) {
    x <- x + unit(width / 2, "mm")
  } else if (any(grepl("right", bjust))) {
    x <- x - unit(width / 2, "mm")
  } else if (is.numeric(bjust)) {
    x <- x + unit(width * (0.5 - bjust[1]), "mm")
  }
  return(x)
}

prAdjustYPos <- function(bjust, y, height) {
  height <- prCnvrtY(height)
  if (any(grepl("top", bjust))) {
    y <- y - unit(height / 2, "mm")
  } else if (any(grepl("bottom", bjust))) {
    y <- y + unit(height / 2, "mm")
  } else if (is.numeric(bjust) && length(bjust) == 2) {
    y <- y + unit(height * (0.5 - bjust[2]), "mm")
  }
  return(y)
}

prGetX4Txt <- function(just, txt_padding) {
  x <- .5
  if (just == "left") {
    x <- txt_padding
  } else if (just == "right") {
    x <- unit(1, "npc") - txt_padding
  }
  return(x)
}

prConvTxt2Height <- function(str) {
  if (missing(str)) {
    return(0)
  }

  length(strsplit(as.character(str), "\n")[[1]]) %>%
    unit("lines") %>%
    prCnvrtY()
}