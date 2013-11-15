#' A simpler latex output of the latex.anova.rms 
#' 
#' The original problem is that the anova default function
#' output is very detailed and cause a complaint in Sweave/knitr that
#' \\hbox is overfull. It basically changes capitalized TOTAL,
#' TOTAL INTERACTION and TOTAL NONLINEAR INTERACTION into lower
#' case letters. It also deletes the (Factor + Higher Order Factors).
#'   
#' @param anova_output An object from the \code{\link{anova}()} function 
#' @param subregexps A 2 column matrix with sub() regular expressions
#'   to search for and their substitutions. The regular expression
#'   should be located in column 1 and the substitution in column
#'   2.
#' @param digits Number of digits in using the round
#' @param rowlabel The label of the rows
#' @param pval_threshold The threshold before setting "<", default is < 0.0001 
#' @param ... Passed on to latex() or htmlTable
#' @return void See the latex() function 
#' 
#' @example examples/simpleRmsAnova_example.R
#' 
#' @rdname SimpleRmsAnova
#' @author max
#' @export
simpleRmsAnova <- function(anova_output, subregexps = NA, digits=4, pval_threshold = 10^-4, rowlabel="Variable", ...){
  
  if (!inherits(anova_output, "anova.rms"))
    if (inherits(anova_output, "rms")){
      anova_output <- anova(anova_output) 
    }else{
      stop("You must provide either an rms-regression object or an anova rms output for this to work")
    }
  
  rownames <- names(attr(anova_output, "which"))
  if (is.matrix(subregexps) && NCOL(subregexps) == 2){
    for (i in 1:NROW(subregexps))
      rownames <- sub(subregexps[i, 1], subregexps[i, 1], rownames)
  }
  rownames <- sub("TOTAL", "Total", rownames)
  rownames <- sub("INTERACTION", "interaction", rownames)
  rownames <- sub("NONLINEAR", "nonlinear", rownames)
  rownames <- sub("\\(Factor\\+Higher Order Factors\\)", "", rownames)
  
  mtrx <- as.matrix(anova_output)
  pvals <- mtrx[,ncol(mtrx)]
  mtrx <- mtrx[,-ncol(mtrx)]
  # The digits differ per column and we need
  # to handle NA:s
  mtrx <- apply(mtrx, MARGIN=2, 
    FUN=function(x, digits) {
      ret <- c()
      for(val in x){
        if (is.numeric(val) == FALSE){
          ret <- append(ret, val)
        }else if (is.na(val)){
          ret <- append(ret, "")
        }else if (round(val, digits) == 0){
          ret <- append(ret, sprintf(sprintf("%%.%df", digits), 0))
        }else{
          ret <- append(ret, format(val, digits=digits))
        }
      }
      return(ret)
    },
    digits=digits)
  pvals <- ifelse(pvals < pval_threshold,  
    sprintf("< %s", format(pval_threshold, scientific=FALSE)),
    formatC(pvals, digits=2))
  mtrx <- cbind(mtrx, pvals)
  number_of_total_rows <- length(grep("TOTAL", names(attr(anova_output, "which"))))
  attr(mtrx, "title") <- rownames
  attr(mtrx, "n.rgroup") <- c(NROW(mtrx)-number_of_total_rows, number_of_total_rows)
  attr(mtrx, "rgroup") <- c("Variables", "Total")
  attr(mtrx, "rowlabel") <- rowlabel
  attr(mtrx, "other") <- list(...)
  class(mtrx) <- c("simpleRmsAnova", class(mtrx))
  return(mtrx)
}

#' @param x The output object from the SimpleRmsAnova function 
#' @rdname SimpleRmsAnova
#' @method print simpleRmsAnova
#' @param html If HTML output through the htmlTable should be used 
#'   instead of traditional latex() function
#' @S3method print simpleRmsAnova
print.simpleRmsAnova <- function(x, html=TRUE, ...){
  dots <- list(...)
  html = TRUE
  if ("html" %in% names(dots)){
    html <- dots[["html"]]
    dots[["html"]] <- NULL
    
  }else if (length(attr(x, "html")) > 0){
    html <- attr(x, "html")
    attr(x, "html") <- NULL
  }
  
  if (html){
    rownames <- sub("^ ", "&nbsp;&nbsp;", rownames(x))
  }else{
    rownames <- sub("^ ", "\\\\hspace{3 mm}", rownames(x))
  }
  
  call_list <- list(x = x, 
    rowname=rownames, 
    n.rgroup=attr(x, "n.rgroup"), 
    rgroup=attr(x, "rgroup"),
    rowlabel=attr(x, "rowlabel"))
  
  if (length(attr(x, "other")) > 0){
    other <- attr(x, "other")
    for (option in names(other))
      if (nchar(option) > 0) call_list[option] <- other[[option]]
  }
  
  if (length(dots) > 0){
    for (option in names(dots))
      if (nchar(option) > 0) call_list[option] <- dots[[option]]
  }

  if (html) call_list[["x"]] <- gsub("<", "&lt;", call_list[["x"]])
  
  do.call(ifelse(html, "htmlTable", "latex"), call_list)
}
