#' Detects if knitr is running
#'
#' The function tries to deduce if the environment contains functions
#' that indicate that knitr is running and compiling. Previously all
#' that was necessary was to detect the package:knitr string within
#' the environment but with recent changes a more complicated detection
#' algorithm has been designed.
#'
#' @return \code{TRUE/FALSE} depending if knitting
#' @export
#' @examples
#' isKnitting()
#'
#' @author Max Gordon
isKnitting <- function(){
  if ("package:knitr" %in% search() ||
        length(ls(pattern = "metadata")) == 1 ||
        (is.function(options()$device) &&
           length(ls(envir = environment(options()$device), pattern="^knit$")) == 1)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}