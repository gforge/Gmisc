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
#' @return Returns a vector if vars wasn't specified and it's a
#'  continuous or binary statistic. If vars was a matrix then it
#'  appends the result to the end of that matrix. If the x variable
#'  is a factor then it does not append and you get a warning.
#' 
#' @example examples/getDescriptionStatsBy_example.R
#' 
#' @author max
#' @export
getDescriptionStatsBy <- function(x, by, vars=FALSE, digits=1, html = FALSE, NEJMstyle = FALSE){
  describe_mean <- function(x){
    if (html)
      ret <- sprintf(sprintf("%%.%df &plusmn;(%%3.%df)", digits, digits), 
          mean(x, na.rm=T), sd(x, na.rm=T))
    else
      ret <- sprintf(sprintf("$%%.%df \\pm(%%3.%df)$", digits, digits), 
        mean(x, na.rm=T), sd(x, na.rm=T))
    
    return (ret)
  }
  
  describe_prop <- function(x) {
    no <- sum(x == levels(x)[1], na.rm=T)
    percent <- 100*no/length(x[is.na(x)==F])
    no <- ifelse(no >= 10^4, format(no, big.mark=" "), format(no))
    if (html)
      ret <- sprintf(sprintf("%%s (%%.%df %%%%)", digits), no, percent)
    else
      ret <- sprintf(sprintf("%%s (%%.%df \\%%%%)", digits), no, percent)
    return (ret)
  }
  
  describe_factors <- function(x) {
    values <- ifelse(table(x) >= 10^4, format(table(x), big.mark=" "), format(table(x)))
    if (html)
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df %%%%)", digits), 
          values, 
          table(x)/sum(table(x))*100), ncol=1)
    else
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df \\%%%%)", digits), 
          values,
          table(x)/sum(table(x))*100), ncol=1)
    return(ret)
  }
  
  if (is.numeric(x) ||
    (is.factor(x) && length(levels(x)) == 2)){
    if (is.factor(x)){
      t <- by(x, by, FUN=describe_prop)
      name <- sprintf("%s %s", capitalize(levels(x)[1]), tolower(label(x)))
      if (NEJMstyle) {
      	if (html) {
      		name <- sprintf("%s - no (%%)", name)
      	} else {
          name <- sprintf("%s - no (\\%%)", name)
      	}
      }
    }else{
      t <- by(x, by, FUN=describe_mean)
      name <- label(x)
    }
    if (is.matrix(vars)){
      rn <- rownames(vars)
      rn <- c(rn, name)
      results <- unlist(t)
      if (NCOL(vars) == length(unique(by)) + 1){
        if (units(x) != "") results <- append(results, sprintf("$%s$",units(x)))
        else results <- append(results, "-")
      }
      vars <- rbind(vars, results)
      rownames(vars) <- rn
    }else{
      results <- unlist(t)
      if (units(x) != ""){
        results <- append(results, sprintf("$%s$",units(x)))
      }
      vars <- matrix(results, ncol=length(results))
      rownames(vars) <- name
    }
  }else{
    if (is.matrix(vars))
      warning("The function cannot add a factor variable to a previous matrix")
    
    t <- by(x, by, FUN=describe_factors)
    vars <- matrix(unlist(t), ncol=length(unique(by)))
    label(vars) <- label(x)
    rownames(vars) <- names(table(x))
  }
  
  return (vars)
}