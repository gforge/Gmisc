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
#' @param html If HTML output through the htmlTable should be used 
#'   instead of traditional latex() function
#' @param digits Number of digits in using the round
#' @param rowlabel The label of the rows
#' @param pval_threshold The threshold before setting "<", default is < 0.0001 
#' @param ... Passed on to latex() or htmlTable
#' @return void See the latex() function 
#' 
#' @example examples/simpleRmsAnovaLatex_example.R
#' 
#' @author max
#' @export
simpleRmsAnovaLatex <- function(anova_output, subregexps = NA, html=FALSE, digits=4, pval_threshold = 10^-4, rowlabel="Variable", ...){
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
    sprintf(ifelse(html, "&lt; %s", "< %s"), format(pval_threshold, scientific=FALSE)),
    format(pvals, digits=2))
  mtrx <- cbind(mtrx, pvals)
  number_of_total_rows <- length(grep("TOTAL", names(attr(anova_output, "which")))) 
  if (html){
    rownames <- sub("^ ", "&nbsp;&nbsp;", rownames)
    htmlTable(mtrx, title=rownames, n.rgroup=c(NROW(mtrx)-number_of_total_rows, number_of_total_rows), 
      rgroup=c("Variables", "Total"), rowlabel=rowlabel,...)
  }else{
    rownames <- sub("^ ", "\\\\hspace{3 mm}", latexTranslate(rownames))
    latex(mtrx, rowname=rownames, n.rgroup=c(NROW(mtrx)-number_of_total_rows, number_of_total_rows), 
      rgroup=c("Variables", "Total"), rowlabel=rowlabel, ...)
  }
}
