#' An alternative to the internal \code{do.call}
#' 
#' The \code{\link[base]{do.call}} can be somewhat slow, especially when working
#' with large objects. This function is based upon the suggestions from Hadley
#' Wickham on the R mailing list, see \href{here}{http://r.789695.n4.nabble.com/call-do-call-expression-too-big-td3574335.html}
#' 
#' @inheritParams base::do.call
#' 
#' @section Note:
#' 
#' While the function attempts to do most of what \code{\link[base]{do.call}}
#' can it has limitations. It can currently not parse the example code from the
#' original function: \code{do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)}
#' and the funcitonality of \code{quote} has not been thoroughly tested.
#' 
#' @example inst/examples/fastDoCall_ex.R
#' @export
fastDoCall <- function(what, args, quote = FALSE, envir = parent.frame()){
  if (quote) 
    args <- lapply(args, enquote)
  
  if (is.null(names(args))){
    argn <- args
    args <- list()
  }else{
    # Add all the named arguments
    argn <- lapply(names(args)[names(args) != ""], as.name) 
    names(argn) <- names(args)[names(args) != ""]
    # Add the unnamed arguments
    argn <- c(argn, args[names(args) == ""])
    args <- args[names(args) != ""]  
  }
  
  if (class(what) == "character"){
    call <- as.call(c(list(as.name(what)), argn)) 
  }else if (class(what) == "function"){
    f_name <- deparse(substitute(what))
    call <- as.call(c(list(as.name(f_name)), argn))
    args[[f_name]] <- what
  }else if (class(what) == "name"){
    call <- as.call(c(list(what, argn)))
  }
  
  eval(call, 
       envir = args, 
       enclos = envir)
}