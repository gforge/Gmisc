#' A function that returns a description statistic that can be used
#' for creating a publication "table 1" when you want it by groups.
#' The function identifies if the variable is a continuous, binary 
#' or a factored variable.
#' 
#' @param x The variable that you want the statistics for
#' @param by The variable that you want to split into different 
#'  columns
#' @param vars If this statistic is for a continuous or a binary 
#'  statistic it's easy to append the current statistic to the
#'  previous
#' @param digits The number of decimals used 
#' @return Returns a vector if vars wasn't specified and it's a
#'  continuous or binary statistic. If vars was a matrix then it
#'  appends the result to the end of that matrix. If the x variable
#'  is a factor then it does not append and you get a warning.
#' 
#' @example examples/getDescriptionStatsBy_example.R
#' 
#' @author max
#' @export
getDescriptionStatsBy <- function(x, by, vars=FALSE, digits=1){
  describe_mean <- function(x) sprintf(sprintf("%%.%df (%%3.%df)", digits, digits), 
      mean(x, na.rm=T), sd(x, na.rm=T))
  describe_prop <- function(x) sprintf(sprintf("%%.%df (\\%%%% %%s)", digits), 
      sum(x == levels(x)[1], na.rm=T)/length(x[is.na(x)==F])*100,
      levels(x)[1])
  describe_factors <- function(x) matrix(
      sprintf(sprintf("%%i (%%.%df \\%%%%)", digits), 
        table(x), 
        table(x)/sum(table(x))*100), ncol=1)
  if (is.numeric(x) ||
    (is.factor(x) && length(levels(x)) == 2)){
    if (is.factor(x)){
      t <- by(x, by, FUN=describe_prop)
    }else{
      t <- by(x, by, FUN=describe_mean)
    }
    if (is.matrix(vars)){
      rn <- rownames(vars)
      rn <- c(rn, label(x))
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
      rownames(vars) <- label(x)
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