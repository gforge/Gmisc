#' Prepares a matrix for htmlTable from a list
#'
#' By putting all the output from the \code{\link{getDescriptionStatsBy}}
#' into a list, naming each element that we want in an rgroup we can
#' automatically merge everything and create an object ready for the
#' htmlTable.
#'
#' @section The \code{rgroup} value:
#'
#' The value for the rgroup is by default the name of the list element. If you have
#' passed a list without a name for that particular element or if you have passed a
#' matrix it will look for a label set by the \pkg{Hmisc}\code{::\link[Hmisc]{label}} function.
#' For those elements that have only one row no rgroup is set, and the naming sequence
#' is the same as above but with an additional \code{\link[base]{rownames}} if the previous
#' two turn out empty. All this behaviour is exemplified in the example.
#'
#' The \code{rgroup} value can be overridden by simply specifying a custom rgroup when
#' calling the \code{\link{htmlTable}} function.
#'
#' @section The \code{colnames} of the matrix:
#'
#' The function chooses the \code{\link[base]{colnames}} from the first element in
#' the \code{tlist}.
#'
#' @param ... One or more elements coming from \code{\link{getDescriptionStatsBy}}.
#'  You can also provide pure output from the \code{\link{getDescriptionStatsBy}} function
#'  and have the function merge this together with the \code{...} argument.
#'  \emph{Note} that all elements myst have the same \code{by} argument or you
#'  will not be able to merge it into a list.
#' @return \code{matrix} Returns a matrix object of class descList
#' @export
#' @example inst/examples/getDescriptionStatsBy_example.R
#' @family table functions
mergeDesc <- function(...){
  tlist <- list()
  dots <- list(...)
  if (length(dots) > 0){
    for (add_lst in dots){
      if (!is.list(add_lst))
        add_lst <- list(add_lst)
      tlist <- append(tlist,
                      add_lst)
    }
  }

  mx <- NULL
  rgroup <- n.rgroup <- c()
  for(i in 1:length(tlist)){
    n <- names(tlist)[i]
    if (is.null(n) || n == ""){
      if (label(tlist[[i]]) != ""){
        n <- label(tlist[[i]])
      }else if (nrow(tlist[[i]]) == 1){
        n <- rownames(tlist[[i]])
      }else{
        n <- ""
      }
    }

    mx <- rbind(mx,
                tlist[[i]])
    if (nrow(tlist[[i]]) > 1){
      rgname <- n
      rgno <- nrow(tlist[[i]])
    }else{
      rownames(mx)[NROW(mx)] <- n
      rgname <- ""
      rgno <- 1
    }

    if (rgno != ""){
      rgroup <- c(rgroup,
                  rgname)
      n.rgroup <- c(n.rgroup,
                    rgno)
    }else{
      if (length(rgroup) == 0){
        rgroup <- ""
        n.rgroup <- rgno
      }else{
        if (tail(rgroup, 1) == rgname){
          n.rgroup[length(n.rgroup)] <-
            n.rgroup[length(n.rgroup)] + rgno
        }else{
          rgroup <- c(rgroup,
                      rgname)
          n.rgroup <- c(n.rgroup,
                        rgno)
        }
      }
    }
  }

  colnames(mx) <- colnames(tlist[[1]])

  return(structure(mx,
                   rgroup = rgroup,
                   n.rgroup = n.rgroup,
                   class = c("descMrg", class(mx))))
}


#' A wrapper around the \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}} function
#'
#' Calls the \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}} after extracting the
#' \code{rgroup}/\code{n.rgroup} arguments.
#'
#' @param x The \code{\link{mergeDesc}} object
#' @param ... Passed onto \code{\link[htmlTable]{htmlTable}}/\code{\link[Hmisc]{latex}}
#'
#' @rdname htmlTable_latex
#' @importFrom Hmisc latex
#' @keywords internal
#' @export
latex.descMrg <- function(x, ...)
{
  dots <- list(...)
  if (!"rgroup" %in% names(dots)){
    return(NextMethod(generic = NULL, object = x,
                      rgroup = attr(x, "rgroup"),
                      n.rgroup = attr(x, "n.rgroup"),
                      ...))
  }

  return(NextMethod(generic = NULL, object = x,
                    ...))
}


#' @rdname htmlTable_latex
#' @export
#' @import htmlTable
htmlTable.descMrg <- function(x, ...)
{
  dots <- list(...)
  if (!"rgroup" %in% names(dots)){
    return(NextMethod(generic = NULL, object = x,
                      rgroup = attr(x, "rgroup"),
                      n.rgroup = attr(x, "n.rgroup"),
                      ...))
  }

  return(NextMethod(generic = NULL, object = x,
                    ...))
}