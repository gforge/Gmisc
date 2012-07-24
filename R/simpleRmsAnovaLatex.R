#' A simpler latex output of the latex.anova.rms 
#' 
#' The original problem is that the anova default function
#' output is very detailed and cause a complaint in Sweave that
#' \\hbox is overfull. It basically changes capitalized TOTAL,
#' TOTAL INTERACTION and TOTAL NONLINEAR INTERACTION into lower
#' case letters. It also deletes the (Factor + Higher Order Factors).
#'   
#' @param anova_output An object from the anova() function 
#' @param subregexps A 2 column matrix with sub() regular expressions
#'   to search for and their substitutions. The regular expression
#'   should be located in column 1 and the substitution in column
#'   2.
#' @param html If HTML output through the htmlTable should be used 
#'   instead of traditional latex() function
#' @param ... Passed on to latex() or htmlTable
#' @return void See the latex() function 
#' 
#' @example examples/simpleRmsAnovaLatex_example.R
#' 
#' @author max
#' @export
simpleRmsAnovaLatex <- function(anova_output, subregexps = NA, html=FALSE, ...){
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
  if (html){
    rownames <- sub("^ ", "&nbsp;&nbsp;", rownames)
    htmlTable(mtrx, rowname=rownames, n.rgroup=c(NROW(mtrx)-4, 4), rgroup=c("Variables", "Total"), ...)
  }else{
    rownames <- sub("^ ", "\\\\hspace{3 mm}", latexTranslate(rownames))
    latex(mtrx, rowname=rownames, n.rgroup=c(NROW(mtrx)-4, 4), rgroup=c("Variables", "Total"), ...)
  }
}