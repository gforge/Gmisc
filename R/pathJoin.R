#' A path join function
#'
#' This function joins strings into a valid path. It is a simple version of python's
#' \code{os.path.join} and fixes simple problems such as having/not having trailing /
#' in each section.
#'
#' @param ... A set of strings to join. Each may be a single string or a vector.
#'  If you provide vectors they can either be all of the same length
#'  or where there are two lengths where one is equal to 1.
#' @return \code{string} A string with the merged path
#'
#' @examples
#' pathJoin("my_base_path/helpers", "superfunction.R")
#' # 'my_base_path/helpers/superfunction.R'
#'
#' base_dir <- "/home/tester/images"
#' out <- data.frame(filename = c("file1.png", "file2.png", "file3.png")) |>
#'   dplyr::mutate(full_path = pathJoin(base_dir, filename))
#'
#' @importFrom stringr str_replace
#' @export
pathJoin <- function(...) {
  paths <- list(...)
  if (length(paths) == 1) {
    paths <- as.list(paths[[1]])
  }

  lengths <- sapply(paths, length, USE.NAMES = FALSE)
  if (length(unique(lengths)) > 1) {
    if (length(unique(lengths)) > 2) {
      stop("You can only have two lengths for joining paths")
    }
    if (!(1 %in% lengths)) {
      stop("If using different lengths at least one of the vectors must have the length of 1")
    }
  }

  clean_paths <- paths %>%
    lapply(function(x) {
      if (is.character(x)) {
        return(dropTrailingSlash(x))
      }
      lapply(x, dropTrailingSlash)
    })

  if (!all(lengths == lengths[1])) {
    ml <- max(lengths)
    for (i in 1:length(clean_paths)) {
      clean_paths[[i]] <- rep(clean_paths[[i]], length.out = ml)
    }
  }

  clean_paths |>
    data.frame() |>
    apply(MARGIN = 1, FUN = \(x) do.call(file.path, as.list(x))) |>
    str_replace("/[/]+", .Platform$file.sep)
}


dropTrailingSlash <- function(x) {
  str_replace(x, "[\\/]$", "")
}
