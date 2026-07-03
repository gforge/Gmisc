#' Connect boxes (S3)
#'
#' A convenient way to connect boxes in a `Gmisc_list_of_boxes` or
#' simple `list` context, designed for piping (`|>`).
#'
#' @param x A `list` of boxes (will be converted to `Gmisc_list_of_boxes` if needed).
#' @param from The name (string), index, list of selectors, or `stringr::regex()`
#'   selector for the start box in `x`. Multiple values allowed.
#' @param to The name (string), index, list of selectors, or `stringr::regex()`
#'   selector for the end box in `x`. Multiple values allowed.
#' @param ... Arguments passed on to [`connectGrob`].
#'
#' @return The original list `x` (upgraded to `Gmisc_list_of_boxes`) with a new
#'   connection appended to its `"connections"` attribute. When printed, these
#'   connections are drawn.
#'
#' @seealso [`connectGrob`]
#' @export
#' @family flowchart components
connect <- function(x, ...) UseMethod("connect")

#' @export
#' @rdname connect
connect.default <- function(x, ...) {
  # If it's a list (but not yet class-extended), treat it as a box list
  if (is.list(x) && !inherits(x, "box")) {
    return(connect(prConvertListToBoxList(x), ...))
  }
  stop("connect() requires a list of boxes (Gmisc_list_of_boxes).")
}

#' @export
#' @rdname connect
connect.Gmisc_list_of_boxes <- function(x, from = NULL, to = NULL, ...) {
  args <- list(...)

  if (is.null(from) || is.null(to)) {
    stop("You must provide both 'from' and 'to' arguments.")
  }

  # Helper to resolve box objects from x using names/indices/selectors
  resolve_box <- function(ref) {
    if (inherits(ref, "stringr_regex")) {
      paths <- prResolveSubelementSelector(ref, x)
      return(lapply(paths, function(path) {
        el <- get_list_element_by_path(x, path)
        if (is.null(el)) {
          stop("Could not find box named: ", paste(path, collapse = " -> "))
        }
        el
      }))
    } else if (is.list(ref) && !inherits(ref, "box")) {
      if (prIsBoxList(ref)) {
        return(ref)
      }
      return(unlist(lapply(ref, resolve_box), recursive = FALSE))
    } else if (is.character(ref)) {
      if (all(ref %in% names(x))) {
        return(x[ref])
      }

      resolved <- lapply(ref, function(r) {
        if (r %in% names(x)) {
          return(x[[r]])
        }

        if (grepl("$", r, fixed = TRUE)) {
          path <- strsplit(r, "$", fixed = TRUE)[[1]]
          el <- get_list_element_by_path(x, path)
          if (!is.null(el)) {
            return(el)
          }
        }

        stop("Could not find box named: ", r)
      })
      return(resolved)
    } else if (is.numeric(ref)) {
      return(x[ref])
    } else if (inherits(ref, "box") || (is.list(ref) && all(vapply(ref, inherits, logical(1), "box")))) {
      # Use directly if passed as object
      return(ref)
    }
    stop("Invalid from/to selector: must be name, index, or box object.")
  }

  is_list_of_box_lists <- function(obj) {
    is.list(obj) &&
      length(obj) > 0 &&
      !inherits(obj, "box") &&
      all(vapply(obj, prIsBoxList, logical(1)))
  }

  connect_side_fan_in <- function(starts, end) {
    get_arg <- function(name, default) {
      if (!is.null(args[[name]])) args[[name]] else default
    }

    lty_gp <- get_arg("lty_gp", getOption("connectGrob", default = gpar(fill = "black")))
    arrow_obj <- get_arg("arrow_obj", getOption("connectGrobArrow", default = arrow(ends = "last", type = "closed")))
    arrow_size <- get_arg("arrow_size", NULL)
    side <- match.arg(get_arg("side", "auto"), c("auto", "left", "right"))
    end_side <- match.arg(get_arg("end_side", "auto"), c("auto", "left", "right"))
    side_route <- match.arg(get_arg("side_route", "outside"), c("outside", "edge"))
    side_offset <- get_arg("side_offset", unit(5, "mm"))
    smooth <- get_arg("smooth", FALSE)
    corner_radius <- get_arg("corner_radius", unit(3, "mm"))
    if (is.numeric(side_offset)) {
      side_offset <- unit(side_offset, "mm")
    }
    if (!inherits(side_offset, "unit")) {
      stop("`side_offset` must be a unit or numeric.", call. = FALSE)
    }

    if (!is.null(arrow_size)) {
      ends_map <- c("1" = "first", "2" = "last", "3" = "both")
      type_map <- c("1" = "open", "2" = "closed")
      arrow_obj <- arrow(
        ends = ends_map[as.character(arrow_obj$ends)],
        type = type_map[as.character(arrow_obj$type)],
        angle = arrow_obj$angle,
        length = unit(arrow_size, "mm")
      )
    }

    s_coords <- lapply(starts, coords)
    e <- coords(end)
    starts_left <- mean(vapply(s_coords, function(s) prConvertWidthToMm(s$x), numeric(1))) <
      prConvertWidthToMm(e$x)

    exit_side <- if (side == "right" || (side == "auto" && !starts_left)) "right" else "left"
    entry_side <- if (end_side == "left" || (end_side == "auto" && starts_left)) "left" else "right"

    exit_xs <- lapply(s_coords, `[[`, exit_side)
    bus_x <- if (exit_side == "right") {
      exit_xs[[which.max(vapply(exit_xs, prConvertWidthToMm, numeric(1)))]]
    } else {
      exit_xs[[which.min(vapply(exit_xs, prConvertWidthToMm, numeric(1)))]]
    }
    if (side_route == "outside") {
      bus_x <- if (exit_side == "right") {
        bus_x + side_offset
      } else {
        bus_x - side_offset
      }
    }
    entry_x <- e[[entry_side]]

    stubs <- lapply(seq_along(s_coords), function(i) {
      prRenderLine(
        x = unit.c(exit_xs[[i]], bus_x),
        y = unit.c(s_coords[[i]]$y, s_coords[[i]]$y),
        smooth = FALSE,
        corner_radius = corner_radius,
        gp = lty_gp,
        arrow = NULL
      )
    })

    y_mm <- vapply(s_coords, function(s) prConvertHeightToMm(s$y), numeric(1))
    end_y_mm <- prConvertHeightToMm(e$y)
    bus_start_y <- if (end_y_mm < mean(y_mm)) {
      s_coords[[which.max(y_mm)]]$y
    } else {
      s_coords[[which.min(y_mm)]]$y
    }

    spine <- prRenderLine(
      x = unit.c(bus_x, bus_x),
      y = unit.c(bus_start_y, e$y),
      smooth = FALSE,
      corner_radius = corner_radius,
      gp = lty_gp,
      arrow = NULL
    )
    final <- prRenderLine(
      x = unit.c(bus_x, entry_x),
      y = unit.c(e$y, e$y),
      smooth = smooth,
      corner_radius = corner_radius,
      gp = lty_gp,
      arrow = arrow_obj
    )

    line <- list(
      x = unit.c(bus_x, bus_x, entry_x),
      y = unit.c(bus_start_y, e$y, e$y)
    )
    gt <- grid::grobTree(do.call(grid::gList, c(stubs, list(spine, final))))
    structure(gt, line = line, class = c("connect_boxes", class(gt)))
  }

  connect_side_fan_out <- function(start, ends) {
    get_arg <- function(name, default) {
      if (!is.null(args[[name]])) args[[name]] else default
    }

    lty_gp <- get_arg("lty_gp", getOption("connectGrob", default = gpar(fill = "black")))
    arrow_obj <- get_arg("arrow_obj", getOption("connectGrobArrow", default = arrow(ends = "last", type = "closed")))
    arrow_size <- get_arg("arrow_size", NULL)
    side <- match.arg(get_arg("side", "auto"), c("auto", "left", "right"))
    end_side <- match.arg(get_arg("end_side", "auto"), c("auto", "left", "right"))
    side_route <- match.arg(get_arg("side_route", "outside"), c("outside", "edge"))
    side_offset <- get_arg("side_offset", unit(5, "mm"))
    smooth <- get_arg("smooth", FALSE)
    corner_radius <- get_arg("corner_radius", unit(3, "mm"))
    label <- get_arg("label", NULL)
    label_gp <- get_arg("label_gp", grid::gpar(cex = 0.9))
    label_bg_gp <- get_arg("label_bg_gp", grid::gpar(fill = "white", col = NA, alpha = 0.85))
    label_pad <- get_arg("label_pad", unit(1.5, "mm"))
    label_offset <- get_arg("label_offset", unit(0, "mm"))

    if (is.numeric(side_offset)) {
      side_offset <- unit(side_offset, "mm")
    }
    if (!inherits(side_offset, "unit")) {
      stop("`side_offset` must be a unit or numeric.", call. = FALSE)
    }
    if (!is.null(label)) assert_class(label_gp, "gpar")
    if (is.numeric(label_pad)) label_pad <- unit(label_pad, "mm")
    if (is.numeric(label_offset)) label_offset <- unit(label_offset, "mm")

    if (!is.null(arrow_size)) {
      ends_map <- c("1" = "first", "2" = "last", "3" = "both")
      type_map <- c("1" = "open", "2" = "closed")
      arrow_obj <- arrow(
        ends = ends_map[as.character(arrow_obj$ends)],
        type = type_map[as.character(arrow_obj$type)],
        angle = arrow_obj$angle,
        length = unit(arrow_size, "mm")
      )
    }

    s <- coords(start)
    e_coords <- lapply(ends, coords)
    ends_right <- mean(vapply(e_coords, function(e) prConvertWidthToMm(e$x), numeric(1))) >
      prConvertWidthToMm(s$x)

    exit_side <- if (side == "right" || (side == "auto" && ends_right)) "right" else "left"
    entry_side <- if (end_side == "left" || (end_side == "auto" && ends_right)) "left" else "right"

    exit_x <- s[[exit_side]]
    bus_x <- exit_x
    if (side_route == "outside") {
      bus_x <- if (exit_side == "right") {
        bus_x + side_offset
      } else {
        bus_x - side_offset
      }
    }

    stub <- prRenderLine(
      x = unit.c(exit_x, bus_x),
      y = unit.c(s$y, s$y),
      smooth = FALSE,
      corner_radius = corner_radius,
      gp = lty_gp,
      arrow = NULL
    )

    y_mm <- vapply(e_coords, function(e) prConvertHeightToMm(e$y), numeric(1))
    start_y_mm <- prConvertHeightToMm(s$y)
    bus_end_y <- if (mean(y_mm) < start_y_mm) {
      e_coords[[which.min(y_mm)]]$y
    } else {
      e_coords[[which.max(y_mm)]]$y
    }

    spine <- prRenderLine(
      x = unit.c(bus_x, bus_x),
      y = unit.c(s$y, bus_end_y),
      smooth = FALSE,
      corner_radius = corner_radius,
      gp = lty_gp,
      arrow = NULL
    )

    finals <- lapply(e_coords, function(e) {
      prRenderLine(
        x = unit.c(bus_x, e[[entry_side]]),
        y = unit.c(e$y, e$y),
        smooth = smooth,
        corner_radius = corner_radius,
        gp = lty_gp,
        arrow = arrow_obj
      )
    })

    line <- list(
      x = unit.c(exit_x, bus_x, bus_x, e_coords[[1]][[entry_side]]),
      y = unit.c(s$y, s$y, bus_end_y, e_coords[[1]]$y)
    )
    label_grobs <- list()
    if (!is.null(label)) {
      label_x <- bus_x
      label_y <- s$y + label_offset
      tg <- grid::textGrob(label, x = label_x, y = label_y, just = "center", gp = label_gp)
      w <- grid::grobWidth(tg) + label_pad
      h <- grid::grobHeight(tg) + label_pad
      bg <- grid::roundrectGrob(
        x = label_x, y = label_y,
        width = w, height = h,
        r = unit(1, "mm"),
        gp = label_bg_gp
      )
      label_grobs <- list(bg, tg)
    }
    gt <- grid::grobTree(do.call(grid::gList, c(list(stub, spine), finals, label_grobs)))
    structure(gt, line = line, class = c("connect_boxes", class(gt)))
  }

  connect_armwise <- function(starts, ends) {
    arm_count <- length(ends)
    bad <- which(vapply(starts, length, integer(1)) != arm_count)
    if (length(bad) > 0) {
      stop(
        "When grouped 'from' values connect to a grouped 'to' value, each group must have the same number of boxes as 'to'.",
        call. = FALSE
      )
    }

    if (identical(args$type, "side")) {
      cg <- lapply(seq_len(arm_count), function(i) {
        connect_side_fan_in(lapply(starts, `[[`, i), ends[[i]])
      })
    } else {
      cg <- unlist(lapply(starts, function(start_group) {
        mapply(function(s, e) {
          do.call(connectGrob, c(list(start = s, end = e), args))
        }, start_group, ends, SIMPLIFY = FALSE)
      }), recursive = FALSE)
    }
    class(cg) <- c("connect_boxes_list", "list")
    cg
  }

  connect_armwise_to_groups <- function(starts, end_groups) {
    arm_count <- length(starts)
    bad <- which(vapply(end_groups, length, integer(1)) != arm_count)
    if (length(bad) > 0) {
      stop(
        "When grouped 'to' values connect from a grouped 'from' value, each group must have the same number of boxes as 'from'.",
        call. = FALSE
      )
    }

    cg <- unlist(lapply(end_groups, function(end_group) {
      mapply(function(s, e) {
        do.call(connectGrob, c(list(start = s, end = e), args))
      }, starts, end_group, SIMPLIFY = FALSE)
    }), recursive = FALSE)
    class(cg) <- c("connect_boxes_list", "list")
    cg
  }

  start_boxes <- resolve_box(from)
  end_boxes <- resolve_box(to)

  if (length(start_boxes) == 1) start_boxes <- start_boxes[[1]]
  if (length(end_boxes) == 1) end_boxes <- end_boxes[[1]]

  # Support grouped-source to grouped-target mapping for CONSORT-style
  # return arrows, e.g. connect(regex("^ex"), "analysis", type = "side").
  if (is_list_of_box_lists(start_boxes) && prIsBoxList(end_boxes)) {
    cg <- connect_armwise(start_boxes, end_boxes)
  } else if (prIsBoxList(start_boxes) && is_list_of_box_lists(end_boxes)) {
    cg <- connect_armwise_to_groups(start_boxes, end_boxes)
  } else if (inherits(start_boxes, "box") && prIsBoxList(end_boxes) && identical(args$type, "side")) {
    cg <- connect_side_fan_out(start_boxes, end_boxes)
  } else if (prIsBoxList(start_boxes) && inherits(end_boxes, "box") && identical(args$type, "side")) {
    # Many-to-one side fan-in, e.g. connect(list("ex1", "ex2"), "analysis", type = "side").
    # Routes the shared bus to the requested side (honoring side/end_side and the
    # side_route/offset) instead of letting each line pick the closest side.
    cg <- connect_side_fan_in(start_boxes, end_boxes)
  # Support pairwise list-to-list mapping in the S3 flowchart API.
  # This keeps connectGrob() many-to-many unsupported while making
  # connect("groups", "groups2") behave as users expect.
  } else if (prIsBoxList(start_boxes) && prIsBoxList(end_boxes)) {
    if (length(start_boxes) != length(end_boxes)) {
      stop("When both 'from' and 'to' resolve to lists of boxes, they must have the same length.", call. = FALSE)
    }

    cg <- mapply(function(s, e) {
      do.call(connectGrob, c(list(start = s, end = e), args))
    }, start_boxes, end_boxes, SIMPLIFY = FALSE)
    class(cg) <- c("connect_boxes_list", "list")
  } else {
    # Create the connection grob
    call_args <- c(list(start = start_boxes, end = end_boxes), args)
    cg <- do.call(connectGrob, call_args)
  }

  # Append to attributes
  current_conns <- attr(x, "connections")
  if (is.null(current_conns)) current_conns <- list()

  current_conns <- c(current_conns, list(cg))

  attr(x, "connections") <- current_conns
  x
}
