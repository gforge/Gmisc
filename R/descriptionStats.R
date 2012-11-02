#' A function that returns a description mean that contains
#' the standard deviation
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_mean(1:10)
#' 
#' @author max
#' @export
describe_mean <- function(x, html=FALSE, digits=1){
  if (html)
    ret <- sprintf(sprintf("%%.%df &plusmn;(%%3.%df)", digits, digits), 
        mean(x, na.rm=T), sd(x, na.rm=T))
  else
    ret <- sprintf(sprintf("$%%.%df \\pm(%%3.%df)$", digits, digits), 
      mean(x, na.rm=T), sd(x, na.rm=T))
  
  return (ret)
}
  
#' A function that returns a description proportion that contains
#' the number and the percentage
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used for the percentage 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_prop(factor(sample(50, x=c("A","B"), replace=TRUE)))
#'  
#' @author max
#' @export
describe_prop <- function(x, html=FALSE, digits=1, number_first = TRUE){
  if (is.factor(x) == FALSE)
    x <- factor(x)
  
  no <- sum(x == levels(x)[1], na.rm=T)
  percent <- 100*no/length(x[is.na(x)==F])
  no <- ifelse(no >= 10^4, format(no, big.mark=" "), format(no))
  if (html){
    if (number_first)
      ret <- sprintf(sprintf("%%s (%%.%df %%%%)", digits), no, percent)
    else
      ret <- sprintf(sprintf("%%.%df %%%% (%%s)", digits), percent, no)
  } else {
    if (number_first)
      ret <- sprintf(sprintf("%%s (%%.%df \\%%%%)", digits), no, percent)
    else
      ret <- sprintf(sprintf("%%.%df \\%%%% (%%s)", digits), percent, no)
  }
  return (ret)
}


#' A function that returns a description proportion that contains
#' the number and the percentage
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used for the percentage 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_factors(sample(50, x=c("A","B", "C"), replace=TRUE))
#' 
#' @author max
#' @export
describe_factors <- function(x, html=FALSE, digits=1, number_first = TRUE) {
  values <- ifelse(table(x) >= 10^4, format(table(x), big.mark=" ", trim=TRUE), format(table(x), trim=TRUE))
  if (html){
    if (number_first)
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df %%%%)", digits), 
          values, 
          table(x)/sum(table(x))*100), ncol=1)
    else
      ret <- matrix(
        sprintf(sprintf("%%.%df %%%% (%%s)", digits), 
          table(x)/sum(table(x))*100, 
          values), ncol=1)
  } else {
    if (number_first)
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df \\%%%%)", digits), 
          values,
          table(x)/sum(table(x))*100), ncol=1)
    else
      ret <- matrix(
        sprintf(sprintf("%%.%df \\%%%% (%%s)", digits),
          table(x)/sum(table(x))*100, 
          values), ncol=1)
    
  }
  return(ret)
}
