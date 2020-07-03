#' Outputs an object
#'
#' Manually viewing a list object can be tricky where the natural print
#' can be hard to work through. The config format *yaml* is increadibly
#' dense and useful not only for writing configs but also viewing them
#' which `yamlDump` helps with.
#'
#' @param x An object that \code{\link[yaml]{as.yaml}} accepts
#' @return \code{void}
#'
#' @examples
#' some_fancy_list <- list(complex = list(some_data = 1:3,
#'                                        other_data = list(name = "Max")),
#'                         simple = "awesome overview")
#' yamlDump(some_fancy_list)
#' #complex:
#' #  some_data:
#' #  - 1
#' #  - 2
#' #  - 3
#' #  other_data:
#' #    name: Max
#' #simple: awesome overview
#'
#' @importFrom yaml as.yaml
#' @export
yamlDump <- function(x) {
  as.yaml(x) %>% cat
}