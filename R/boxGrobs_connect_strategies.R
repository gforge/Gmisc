#' Connector Strategy Dispatcher
#'
#' @param start The start object(s)
#' @param end The end object(s)
#' @param type The connector type
#' @return A strategy object
#' @keywords internal
#' @noRd
prGetConnectorStrategy <- function(start, end, type) {
    is_start_list <- prIsBoxList(start)
    is_end_list <- prIsBoxList(end)

    if (is_start_list && is_end_list) {
        stop("Both 'start' and 'end' cannot be lists (not implemented).", call. = FALSE)
    }

    config <- if (is_start_list) {
        "ManyToOne"
    } else if (is_end_list) {
        "OneToMany"
    } else {
        "OneToOne"
    }

    # Map types to camelCase for class names
    type_map <- c(
        "vertical" = "Vertical",
        "horizontal" = "Horizontal",
        "L" = "L",
        "-" = "Dash",
        "Z" = "Z",
        "N" = "N",
        "fan_in_top" = "FanInTop",
        "fan_in_center" = "FanInCenter"
    )

    type_name <- type_map[type]
    if (is.na(type_name)) type_name <- "Default"

    class_name <- paste0(config, type_name, "ConnectorStrategy")

    structure(
        list(
            config = config,
            type = type
        ),
        class = c(class_name, "ConnectorStrategy")
    )
}

#' Calculate the connector grob(s) for a given strategy
#'
#' @param strategy The strategy object
#' @param ... Arguments passed to the strategy implementation
#' @return A single grob or a list of grobs
#' @keywords internal
#' @noRd
prCalculateConnector <- function(strategy, ...) {
    UseMethod("prCalculateConnector")
}

#' Default fallback for connector strategies
#' @noRd
prCalculateConnector.ConnectorStrategy <- function(strategy, ...) {
    # If no specific strategy for the type+config combination exists,
    # we fall back to the config-level default handlers.
    args <- list(...)
    if (strategy$config == "ManyToOne") {
        return(prConnectManyToOneStandard(
            starts = args$start,
            end = args$end,
            type = strategy$type,
            subelmnt = args$subelmnt,
            lty_gp = args$lty_gp,
            arrow_obj = args$arrow_obj
        ))
    }

    if (strategy$config == "OneToMany") {
        return(prConnectOneToManyStandard(
            start = args$start,
            ends = args$end,
            type = strategy$type,
            subelmnt = args$subelmnt,
            lty_gp = args$lty_gp,
            arrow_obj = args$arrow_obj
        ))
    }

    # OneToOne default
    prConnect1(
        start = args$start,
        end = args$end,
        type = strategy$type,
        subelmnt = args$subelmnt,
        lty_gp = args$lty_gp,
        arrow_obj = args$arrow_obj,
        label = args$label,
        label_gp = args$label_gp,
        label_bg_gp = args$label_bg_gp,
        label_pos = args$label_pos,
        label_offset = args$label_offset,
        label_pad = args$label_pad
    )
}

#' Specific strategy for Many-to-One Fan-in Center
#' @noRd
prCalculateConnector.ManyToOneFanInCenterConnectorStrategy <- function(strategy, ...) {
    args <- list(...)
    prConnectManyToOneFanCenter(
        starts = args$start,
        end = args$end,
        subelmnt = args$subelmnt,
        lty_gp = args$lty_gp,
        arrow_obj = args$arrow_obj,
        margin = args$margin,
        split_pad = args$split_pad
    )
}

#' Specific strategy for Many-to-One Fan-in Top
#' @noRd
prCalculateConnector.ManyToOneFanInTopConnectorStrategy <- function(strategy, ...) {
    args <- list(...)
    prConnectManyToOneFanTop(
        starts = args$start,
        end = args$end,
        subelmnt = args$subelmnt,
        lty_gp = args$lty_gp,
        arrow_obj = args$arrow_obj,
        margin = args$margin,
        split_pad = args$split_pad
    )
}

#' Specific strategy for Many-to-One N
#' @noRd
prCalculateConnector.ManyToOneNConnectorStrategy <- function(strategy, ...) {
    args <- list(...)
    prConnectManyToOneN(
        starts = args$start,
        end = args$end,
        type = strategy$type,
        subelmnt = args$subelmnt,
        lty_gp = args$lty_gp,
        arrow_obj = args$arrow_obj,
        split_pad = args$split_pad
    )
}

#' Specific strategy for One-to-Many N
#' @noRd
prCalculateConnector.OneToManyNConnectorStrategy <- function(strategy, ...) {
    args <- list(...)
    prConnectOneToManyN(
        start = args$start,
        ends = args$end,
        type = strategy$type,
        subelmnt = args$subelmnt,
        lty_gp = args$lty_gp,
        arrow_obj = args$arrow_obj,
        split_pad = args$split_pad
    )
}
