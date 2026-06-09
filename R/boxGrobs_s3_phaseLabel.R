#' Add a phase label to a flowchart stage (S3)
#'
#' Adds a label box that sits just above a *stage* and is centred between that
#' stage's arms, drawn on top of the surrounding boxes. This is the typical
#' CONSORT "phase" banner such as *Allocation*, *Follow-up* and *Analysis* that
#' spans the randomisation arms.
#'
#' Unlike [`insert`], the label is positioned relative to the stage it labels
#' (`reference`), not relative to a neighbouring element, and it is marked to be
#' drawn on top (see `on_top` in [`insert`]) so it stays visible even where it
#' overlaps the stage boxes.
#'
#' @param x A `Gmisc_list_of_boxes` (or a `list` of boxes, which is converted).
#' @param reference The name or index of the **stage to label**. The stage may be
#'  a single [`boxGrob`] or a `list` of arm boxes.
#' @param label The label, either a string (wrapped with [`boxGrob`]) or a
#'  pre-built [`boxGrob`]/[`boxPropGrob`].
#' @param ... Passed on to [`boxGrob`] when `label` is a string (e.g. `box_gp`,
#'  `txt_gp`).
#' @param width Optional fixed label width as a [`unit`][grid::unit] (a numeric is
#'  treated as millimetres). When `NULL` (default) the width is derived from the
#'  stage: for a two-arm stage the label spans the gap between the arms plus
#'  `overlap` of each inner top corner; for a single- or 3+-arm stage the label
#'  spans the full width of the stage (a banner across all arms).
#' @param overlap How far the label overlaps the stage, as a fraction
#'  (default `0.07`, i.e. ~7%). It controls the vertical dip below the stage's top
#'  edge and, for the derived two-arm width, the horizontal corner lap.
#' @param name Name for the inserted label element. Defaults to
#'  `paste0(reference, "_label")`.
#'
#' @return The updated `Gmisc_list_of_boxes` with the label added after the
#'  referenced stage.
#' @seealso [`insert`] for the general `on_top` overlay mechanism this builds on.
#' @export
#' @family flowchart components
#' @examples
#' library(grid)
#' grid.newpage()
#'
#' flowchart(
#'   rando = boxGrob("Randomised\nN = 100", x = .5, y = .8),
#'   arms = list(
#'     boxGrob("Intervention\nn = 50", x = .3, y = .4),
#'     boxGrob("Control\nn = 50", x = .7, y = .4)
#'   )
#' ) |>
#'   phaseLabel("arms", "Allocation", box_gp = gpar(fill = "#c8daf7")) |>
#'   connect("rando", "arms", type = "N") |>
#'   print()
phaseLabel <- function(x, ...) {
  UseMethod("phaseLabel")
}

#' @export
#' @rdname phaseLabel
phaseLabel.default <- function(x, ...) {
  if (is.list(x) && !inherits(x, "box")) {
    return(phaseLabel(prConvertListToBoxList(x), ...))
  }
  stop("phaseLabel() requires a list of boxes (Gmisc_list_of_boxes).")
}

#' @export
#' @rdname phaseLabel
phaseLabel.Gmisc_list_of_boxes <- function(x, reference, label, ..., width = NULL, overlap = 0.07, name = NULL) {
  # Resolve the referenced stage to a list index
  idx <- if (is.character(reference)) match(reference, names(x)) else as.integer(reference)
  if (length(idx) != 1 || is.na(idx) || idx < 1 || idx > length(x)) {
    stop("phaseLabel(): could not find stage '", reference, "' in the flowchart.", call. = FALSE)
  }
  stage <- x[[idx]]

  # Stage geometry (bounding box of all arms) and arm count
  rc <- prConvert2Coords(stage)
  n_arms <- if (inherits(stage, "box") || !is.list(stage)) 1L else length(stage)

  # Build the label box
  lab <- if (inherits(label, "box")) label else boxGrob(label, ...)

  to_npc_x <- function(u) convertX(u, "npc", valueOnly = TRUE)
  to_npc_y <- function(u) convertY(u, "npc", valueOnly = TRUE)
  to_npc_w <- function(u) convertWidth(u, "npc", valueOnly = TRUE)
  to_npc_h <- function(u) convertHeight(u, "npc", valueOnly = TRUE)

  auto_w <- to_npc_w(coords(lab)$width)

  # Determine the target width (in npc)
  if (!is.null(width)) {
    if (is.numeric(width)) width <- unit(width, "mm")
    if (!is.unit(width)) stop("`width` must be a unit or numeric.", call. = FALSE)
    target_w <- to_npc_w(width)
  } else if (n_arms == 2) {
    # Narrow label spanning the central gap plus a corner lap on each inner edge
    c1 <- prConvert2Coords(stage[[1]])
    c2 <- prConvert2Coords(stage[[2]])
    x1 <- to_npc_x(c1$x)
    x2 <- to_npc_x(c2$x)
    left_coords  <- if (x1 <= x2) c1 else c2
    right_coords <- if (x1 <= x2) c2 else c1
    gap <- to_npc_x(right_coords$left) - to_npc_x(left_coords$right)
    arm_w <- to_npc_w(left_coords$width)
    target_w <- max(gap + 2 * overlap * arm_w, auto_w)
  } else {
    # Banner spanning the full stage width
    target_w <- max(to_npc_w(rc$width), auto_w)
  }

  # Apply the width (no-op if it already matches the box)
  lab <- prSetBoxDimensions(lab, width = unit(target_w, "npc"))
  attr(lab, "draw_on_top") <- TRUE

  # Position: centred on the stage, bottom edge dipping `overlap` into the top
  dip <- overlap * to_npc_h(rc$height)
  half_h <- to_npc_h(coords(lab)$half_height)
  new_y <- to_npc_y(rc$top) - dip + half_h
  lab <- moveBox(lab, x = rc$x, y = unit(new_y, "npc"), space = "absolute", just = "center")

  # Name and append after the stage (without repositioning, unlike insert())
  if (is.null(name)) {
    ref_name <- if (is.character(reference)) reference else names(x)[idx]
    name <- if (is.null(ref_name) || !nzchar(ref_name)) paste0("phase_", idx) else paste0(ref_name, "_label")
  }
  to_ins <- list(lab)
  names(to_ins) <- name
  append(x, to_ins, after = idx)
}
