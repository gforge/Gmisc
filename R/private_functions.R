# This file contains all the helper functions that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################

#' Get statistics according to the type
#'
#' A simple function applied by the \code{\link{getDescriptionStatsBy}}
#' for the total column.
#'
#' @return A matrix or a vector depending on the settings
#'
#' @inheritParams getDescriptionStatsBy
#' @keywords internal
#' @importFrom stats na.omit
prGetStatistics <- function(x,
                            show_perc = FALSE,
                            html = TRUE,
                            digits = 1,
                            digits.nonzero = NA,
                            numbers_first = TRUE,
                            useNA = c("ifany", "no", "always"),
                            useNA.digits = digits,
                            show_all_values = FALSE,
                            continuous_fn = describeMean,
                            factor_fn = describeFactors,
                            prop_fn = factor_fn,
                            percentage_sign = TRUE) {
  # All the describe functions have the same interface
  # so it is useful to gather all the arguments here
  describe_args <-
    list(
      x = x,
      html = html,
      digits = digits,
      digits.nonzero = digits.nonzero,
      number_first = numbers_first,
      useNA = useNA,
      useNA.digits = useNA.digits,
      percentage_sign = percentage_sign
    )

  if (is.factor(x) ||
    is.logical(x) ||
    is.character(x)) {
    if ((is.factor(x) &&
      length(levels(x)) == 2) ||
      (!is.factor(x) &&
        length(unique(na.omit(x))) == 2)) {
      if (show_perc) {
        total_table <- fastDoCall(prop_fn, describe_args)
      } else {
        total_table <- table(x, useNA = useNA)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE) {
          total_table <- total_table[names(total_table) %in%
            c(levels(as.factor(x))[1], "Missing")]
        }
      }
    } else {
      if (show_perc) {
        total_table <- fastDoCall(factor_fn, describe_args)
      } else {
        total_table <- table(x, useNA = useNA) %>%
          txtInt()
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  } else {
    total_table <- fastDoCall(continuous_fn, describe_args)

    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE) {
      total_table[2] <- sum(is.na(x))
    }
  }
  return(total_table)
}

#' A helper function for the description stats
#'
#' @param x The variable of interest with the levels
#' @param default_ref The default reference, either first,
#'  the level name or a number within the levels. If left out
#'  it defaults to the first value.
#' @return \code{integer} The level number of interest
#'
#' @keywords internal
prDescGetAndValidateDefaultRef <- function(x, default_ref) {
  if (missing(default_ref)) {
    default_ref <- 1
  } else if (is.character(default_ref)) {
    if (default_ref %in% levels(x)) {
      default_ref <- which(default_ref == levels(x))
    } else {
      stop(
        "You have provided an invalid default reference, '",
        default_ref, "' can not be found among: ", paste(levels(x), collapse = ", ")
      )
    }
  } else if (!default_ref %in% 1:length(levels(x))) {
    stop(
      "You have provided an invalid default reference,",
      " it is ", default_ref, " while it should be between 1 and ", length(levels(x)),
      " as this is only used for factors."
    )
  }

  return(default_ref)
}

#' Pushes viewport with margins
#'
#' A \code{\link[grid]{grid.layout}} object is used to
#' generate the margins. A second viewport selecting the
#' mid-row/col is used to create the effect of margins
#'
#' @param bottom The margin object, either in npc or a \code{\link[grid]{unit}} object,
#'  or a vector of 4 if you want the same margins allover.
#' @param left The margin object, either in npc or a \code{\link[grid]{unit}} object.
#' @param top The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param right The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param name The name of the last viewport
#' @return \code{2} - the number of pushed viewports
#'
#' @keywords internal
prPushMarginViewport <- function(bottom, left, top, right, name = "margin") {
  if (!is.unit(bottom)) {
    bottom <- unit(bottom, "npc")
  }

  if (missing(left) &&
    missing(top) &&
    missing(right)) {
    if (length(bottom) == 4) {
      left <- bottom[2]
      top <- bottom[3]
      right <- bottom[4]
      bottom <- bottom[1]
    } else {
      left <- bottom[1]
      top <- bottom[1]
      right <- bottom[1]
    }
  }

  if (!is.unit(top)) {
    top <- unit(top, "npc")
  }

  if (!is.unit(left)) {
    left <- unit(left, "npc")
  }

  if (!is.unit(right)) {
    right <- unit(right, "npc")
  }

  layout_name <- sprintf("margin_grid_%s", name)

  gl <- grid.layout(
    nrow = 3, ncol = 3,
    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
    widths = unit.c(left, unit(1, "npc") - left - right, right)
  )

  pushViewport(viewport(layout = gl, name = layout_name))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2, name = name))
  return(2)
}

#' Adds a title to the plot
#'
#' Adds the title and generates a new
#' main viewport below the title
#'
#' @param title The title as accepted by \code{\link[grid:grid.text]{textGrob}}
#' @param base_cex The base cex used for the plot
#' @param cex_mult The multiplier of the base - i.e. the increase of the
#'  text size for the title as compared to the general
#' @param fontface The type of fontface
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value
#'
#' @keywords internal
prGridPlotTitle <- function(title,
                            base_cex,
                            cex_mult = 1.2,
                            fontface = "bold",
                            space_below = NULL) {
  titleGrob <- textGrob(title,
    just = "center",
    gp = gpar(
      fontface = fontface,
      cex = base_cex * cex_mult
    )
  )

  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly = TRUE) * 1.5, "mm")
  if (is.null(space_below)) {
    space_below <- unit(convertUnit(gh, "mm", valueOnly = TRUE) / 3, "mm")
  } else if (!is.unit(space_below)) {
    space_below <- unit(space_below, "npc")
  }

  gl <- grid.layout(
    nrow = 3, ncol = 1,
    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh)
  )

  pushViewport(viewport(layout = gl, name = "title_layout"))
  pushViewport(viewport(layout.pos.row = 1, name = "title"))
  grid.draw(titleGrob)
  upViewport()

  pushViewport(viewport(layout.pos.row = 3, name = "main"))
}

#' Just a simple acces to the gp$cex parameter
#'
#' @param x The text-grob of interest
#' @return \code{numeric} The cex value, 1 if no cex was present
#' @keywords internal
prGetTextGrobCex <- function(x) {
  cex <- 1
  if (!is.null(x$gp$cex)) {
    cex <- x$gp$cex
  }

  return(cex)
}

#' Collapses a vector for throwing errors
#'
#' The function collapses a vector into an output useful when throwing
#' errors, e.g. 1:3 becomes '1', '2', '3'
#'
#' @param x The vector
prPasteVec <- function(x) {
  sprintf("'%s'", paste(x, collapse = "', '"))
}
