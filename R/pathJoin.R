#' A path join function
#'
#' This function joins strings into a valid path. It is a simple version of python's 
#' \code{os.path.join} and fixes simple problems such as having/not having trailing /
#' in each section.
#' 
#' @param ... A set of strings to join
#' @return \code{string} A string with the merged path
#'
#' @examples
#' pathJoin('my_base_path/helpers', 'superfunction.R')
#' # 'my_base_path/helpers/superfunction.R'
#'
#' @importFrom stringr str_replace
#' @export
pathJoin <- function(...) {
  paths <- list(...) %>% unlist
  paths %>%
    map(~str_replace(.x, "/$", "")) %>%
    do.call(file.path, .) %>%
    str_replace("//", "/")
}
