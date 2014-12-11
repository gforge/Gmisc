#' Prepares a matrix for htmlTable from a list
#'
#' By putting all the output from the \code{\link{getDescriptionStatsBy}}
#' into a list, naming each element that we want in an rgroup we can
#' automatically merge everything and create an object ready for the
#' htmlTable.
#'
#' @param tlist A list where each element comes from \code{\link{getDescriptionStatsBy}}.
#'  You can also provide pure output from the \code{\link{getDescriptionStatsBy}} function
#'  and have the function merge this together with the \code{...} argument.
#'  \emph{Note} that all elements myst have the same \code{by} argument or you
#'  will not be able to merge it into a list.
#' @param ... Additional lists/mtrx to be merged with the \code{tlist}
#' @return \code{matrix} Returns a matrix object of class descList
#' @export
#' @example inst/examples/getDescriptionStatsBy_example.R
descMerger <- function(tlist, ...){
  if (!is.list(tlist))
    tlist <- list(tlist)
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