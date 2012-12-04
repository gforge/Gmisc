#' A function that returns a description mean that contains
#' the standard deviation
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#'  This is only used together with the show_missing variable.
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_mean(1:10)
#' describe_mean(c(1:10, NA), show_missing=TRUE)
#' 
#' @author max
#' @export
describe_mean <- function(x, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE){
  if (html)
    ret <- sprintf(sprintf("%%.%df &plusmn;(%%3.%df)", digits, digits), 
      mean(x, na.rm=T), sd(x, na.rm=T))
  else
    ret <- sprintf(sprintf("$%%.%df \\pm(%%3.%df)$", digits, digits), 
      mean(x, na.rm=T), sd(x, na.rm=T))
  
  if (show_missing & sum(is.na(x))>0){
    ret <- rbind(ret, describe_prop(is.na(x) == FALSE, number_first = number_first, digits = digits, html = html))
    rownames(ret) <- c("Mean (SD)", "Missing")
  }
  
  return (ret)
}

#' A function that returns a description median that contains
#' the interquartile range or the full range
#' 
#' @param x The variable that you want the statistics for
#' @param iqr If interquartile range should be used
#' @param digits The number of decimals used 
#' @param html If HTML compatible output shoudl be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#'  This is only used together with the show_missing variable.
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_median(1:10)
#' describe_median(c(1:10, NA), show_missing=TRUE)
#' 
#' @author max
#' @export
describe_median <- function(x, iqr=TRUE, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE){
  if (iqr)
    range_quantiles = c(1/4, 3/4)
  else
    range_quantiles = c(0, 1)
  
  ret <- sprintf(sprintf("%%.%df (%%3.%df - %%3.%df)", digits, digits, digits),
    median(x, na.rm=T), 
    quantile(x, probs=range_quantiles[1], na.rm=T),
    quantile(x, probs=range_quantiles[2], na.rm=T))
  
  if (show_missing & sum(is.na(x))>0){
    ret <- rbind(ret, describe_prop(is.na(x) == FALSE, number_first = number_first, digits = digits, html = html))
    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing")
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
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_prop(factor(sample(50, x=c("A","B"), replace=TRUE)))
#' describe_prop(factor(sample(50, x=c("A","B", NA), replace=TRUE)), show_missing=TRUE)
#'  
#' @author max
#' @export
describe_prop <- function(x, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE){
  if (is.factor(x) == FALSE)
    x <- factor(x)
  
  no <- sum(x == levels(x)[1], na.rm=T)
  if (show_missing)
    percent <- 100*no/length(x)
  else
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

  if (show_missing & sum(is.na(x))>0){
    ret <- rbind(ret, describe_prop(is.na(x) == FALSE, number_first = number_first, digits = digits, html = html))
    rownames(ret) <- c(
      sprintf(
        ifelse(number_first, 
          ifelse(html, "%s (%%)", "%s (\\%%)"),
          ifelse(html, "%s %% (no)", "%s \\%% (no)")), 
        levels(x)[1]), 
      "Missing")
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
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @examples
#' describe_factors(sample(50, x=c("A","B", "C"), replace=TRUE))
#' describe_factors(sample(50, x=c("A","B", "C", NA), replace=TRUE), show_missing=TRUE)
#' 
#' @author max
#' @export
describe_factors <- function(x, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE) {
  table_results <- table(x, useNA= ifelse(show_missing, "ifany", "no"))
  values <- ifelse(table_results >= 10^4, format(table_results, big.mark=" ", trim=TRUE), 
    format(table_results, trim=TRUE))
  
  percentages <- proportions <- table_results/sum(table_results)*100
  if (html){
    if (number_first)
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df %%%%)", digits), 
          values, 
          percentages), ncol=1)
    else
      ret <- matrix(
        sprintf(sprintf("%%.%df %%%% (%%s)", digits), 
          percentages, 
          values), ncol=1)
  } else {
    if (number_first)
      ret <- matrix(
        sprintf(sprintf("%%s (%%.%df \\%%%%)", digits), 
          values,
          percentages), ncol=1)
    else
      ret <- matrix(
        sprintf(sprintf("%%.%df \\%%%% (%%s)", digits),
          percentages, 
          values), ncol=1)
    
  }
  rn <- names(table_results)
  rn[is.na(rn)] <- "Missing"
  rownames(ret) <- rn
  return(ret)
}
