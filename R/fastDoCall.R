#' An alternative to the internal \code{do.call}
#'
#' The \code{\link[base]{do.call}} can be somewhat slow, especially when working
#' with large objects. This function is based upon the suggestions from Hadley
#' Wickham on the R mailing list, see \href{http://r.789695.n4.nabble.com/call-do-call-expression-too-big-td3574335.html}{here}.
#' Also thanks to \emph{Tommy} at StackOverflow for
#' \href{https://stackoverflow.com/questions/10022436/do-call-in-combination-with}{suggesting}
#' how to handle double and triple colon operators, \code{::}, further
#' enhancing the function.
#'
#' @inheritParams base::do.call
#'
#' @section Note:
#'
#' While the function attempts to do most of what \code{\link[base]{do.call}}
#' can it has limitations. It can currently not parse the example code from the
#' original function: \code{do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)}
#' and the functionality of \code{quote} has not been thoroughly tested.
#'
#' @example inst/examples/fastDoCall_ex.R
#' @export
fastDoCall <- function(what, args, quote = FALSE, envir = parent.frame()) {
  if (quote) {
    args <- lapply(args, enquote)
  }

  if (is.null(names(args)) ||
    is.data.frame(args)) {
    argn <- args
    args <- list()
  } else {
    # Add all the named arguments
    argn <- lapply(names(args)[names(args) != ""], as.name)
    names(argn) <- names(args)[names(args) != ""]
    # Add the unnamed arguments
    argn <- c(argn, args[names(args) == ""])
    args <- args[names(args) != ""]
  }

  if ("character" %in% class(what)) {
    if (is.character(what)) {
      fn <- strsplit(what, "[:]{2,3}")[[1]]
      what <- if (length(fn) == 1) {
        get(fn[[1]], envir = envir, mode = "function")
      } else {
        get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
      }
    }
    call <- as.call(c(list(what), argn))
  } else if ("function" %in% class(what)) {
    f_name <- deparse(substitute(what))
    call <- as.call(c(list(as.name(f_name)), argn))
    args[[f_name]] <- what
  } else if ("name" %in% class(what)) {
    call <- as.call(c(list(what, argn)))
  }

  eval(call,
    envir = args,
    enclos = envir
  )
}