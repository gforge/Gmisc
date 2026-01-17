# Helpers to resolve and validate .subelement targets for align* functions

# Find a target element by path in either the top-level list or the first nested
# container, returning a list with elements: target, container_is_first (logical)
prFindSubelementTarget <- function(boxes2align, path) {
    target <- get_list_element_by_path(boxes2align, path)
    container_is_first <- FALSE

    # Fallback 1: path may refer to named element at top-level with subpath
    if (is.null(target) && length(path) >= 2 && is.list(boxes2align) && !is.null(names(boxes2align)) && path[1] %in% names(boxes2align)) {
        candidate <- boxes2align[[path[1]]]
        if (!is.null(candidate) && is.list(candidate)) {
            target2 <- get_list_element_by_path(candidate, path[-1])
            if (!is.null(target2)) {
                return(list(target = target2, container_is_first = FALSE))
            }
        }
    }

    # Fallback 2: try inside the common nested first element (maintain old behavior)
    if (is.null(target) && length(boxes2align) > 0 && is.list(boxes2align[[1]]) && !inherits(boxes2align[[1]], "box")) {
        target2 <- get_list_element_by_path(boxes2align[[1]], path)
        if (!is.null(target2)) {
            return(list(target = target2, container_is_first = TRUE))
        }
    }

    list(target = target, container_is_first = container_is_first)
}
