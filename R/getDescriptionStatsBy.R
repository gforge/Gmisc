#' A function that returns a description statistic that can be used
#' for creating a publication "table 1" when you want it by groups.
#' The function identifies if the variable is a continuous, binary 
#' or a factored variable. The format is inspired by NEJM, Lancet &
#' BMJ.
#' 
#' @param x The variable that you want the statistics for
#' @param by The variable that you want to split into different 
#'  columns
#' @param vars If this statistic is for a continuous or a binary 
#'  statistic it's easy to append the current statistic to the
#'  previous
#' @param digits The number of decimals used 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param NEJMstyle Adds - no (\%) at the end to proportions
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @param statistics Add statistics, fisher test for proportions and Wilcoxon
#'  for continuous variables
#' @return Returns a vector if vars wasn't specified and it's a
#'  continuous or binary statistic. If vars was a matrix then it
#'  appends the result to the end of that matrix. If the x variable
#'  is a factor then it does not append and you get a warning.
#' 
#' @example examples/getDescriptionStatsBy_example.R
#' 
#' @author max
#' @export
getDescriptionStatsBy <- function(x, by, vars=FALSE, digits=1, html = FALSE, 
  NEJMstyle = FALSE, numbers_first = TRUE, statistics=FALSE){
  
  getFormattedPval <- function(p_val){
    if (p_val < 10^-4)
      return("< 0.0001")
    return(format(p_val, digits=-floor(log10(p_val))+1))
  }
  if (is.numeric(x) ||
    (is.factor(x) && length(levels(x)) == 2)){
    if (is.factor(x)){
      t <- by(x, by, FUN=describe_prop, html=html, digits=digits, number_first=numbers_first)
      name <- sprintf("%s %s", capitalize(levels(x)[1]), tolower(label(x)))
      if (NEJMstyle) {
      	if (html) {
      		name <- sprintf("%s - no (%%)", name)
      	} else {
          name <- sprintf("%s - no (\\%%)", name)
      	}
      }
      pval <- getFormattedPval(fisher.test(x, by)$p.value)
    }else{
      t <- by(x, by, FUN=describe_mean, html=html, digits=digits)
      name <- label(x)
      pval <- getFormattedPval(wilcox.test(x ~ by)$p.value)
    }
    if (is.matrix(vars)){
      rn <- rownames(vars)
      rn <- c(rn, name)
      results <- unlist(t)
      if (NCOL(vars) == length(unique(by)) + 1 + statistics*1){
        if (units(x) != "") results <- append(results, sprintf("$%s$",units(x)))
        else results <- append(results, "-")
      }
      
      if (statistics){
        results <- append(results, pval)
      }
      vars <- rbind(vars, results)
      rownames(vars) <- rn
    }else{
      results <- unlist(t)
      if (units(x) != ""){
        results <- append(results, sprintf("$%s$",units(x)))
      }
      if (statistics){
        results <- append(results, pval)
      }
      vars <- matrix(results, ncol=length(results))
      rownames(vars) <- name
    }
  }else{
    if (is.matrix(vars))
      warning("The function cannot add a factor variable to a previous matrix")
    
    t <- by(x, by, FUN=describe_factors, html=html, digits=digits, number_first=numbers_first)
    vars <- matrix(unlist(t), ncol=length(unique(by)))
    label(vars) <- label(x)
    rownames(vars) <- names(table(x))
    if (statistics){
      # This is a quick fix in case of large dataset
      workspace = 10^5
      if (length(x)*length(levels(x)) > 10^4)
        workspace = 10^7
      pval <- getFormattedPval(fisher.test(x, by, workspace=workspace)$p.value)
      vars <- cbind(vars, c(pval, rep("", nrow(vars)-1)))
    }
  }
  
  return (vars)
}