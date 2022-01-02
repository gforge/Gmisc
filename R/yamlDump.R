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
#' # If you got a character json you can also input it directly
#' # and the function will automatically convert it to a list
#' yamlDump('{"a":{"b":["1"]}}')
#'
#' @importFrom yaml as.yaml
#' @export
yamlDump <- function(x) {
  UseMethod("yamlDump")
}

#' @exportS3Method
yamlDump.default <- function(x) {
  as.yaml(x) %>% cat
}

#' @exportS3Method
yamlDump.character <- function(x) {
  safeLoadPkg("jsonlite")
  yamlDump(jsonlite::fromJSON(x, simplifyVector = FALSE))
}

#' @exportS3Method
yamlDump.json <- function(x) {
  yamlDump.character(x)
}
