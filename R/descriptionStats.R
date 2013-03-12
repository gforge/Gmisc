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
#' @param horizontal_proportions Is only active if show_missing since this is 
#'  the only case of a proportion among continuous variables. This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the 
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command. Note! This calls the  
#'  \code{\link{describeFactors}} since the horizontal interpretation looses the 
#'  vertical information in the second category and is thus better 
#'  interpreted as a whole.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @seealso \code{\link{getDescriptionStatsBy}}
#' 
#' @examples
#' describeMean(1:10)
#' describeMean(c(1:10, NA), show_missing=TRUE)
#' 
#' @author max
#' @export
describeMean <- function(x, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE, horizontal_proportions=NULL){
  show_missing <- prConvertShowMissing(show_missing)

  if (html)
    plusmin_str <- "&plusmn;"
  else
    plusmin_str <- "\\pm"
  
  ret <- c(sprintf(sprintf("%%.%df (%s %%3.%df)", digits, plusmin_str, digits), 
    mean(x, na.rm=T), sd(x, na.rm=T)))
  
  # LaTeX complains if any formula isn't encapsulated within $ signs
  if (html == FALSE)
    ret <- sprintf("$%s$", ret)
  
  if (show_missing %in% c("ifany", "always") & sum(is.na(x))>0){
    missing <- describeFactors(is.na(x), number_first = number_first, digits = digits, html = html,
      horizontal_proportions = horizontal_proportions)
    ret <- rbind(ret, missing["TRUE", ])
    rownames(ret) <- c("Mean (SD)", "Missing")
  } else if (show_missing == "always"){
    percent_sign <- ifelse(html, "%", "\\%")
    empty <- sprintf(ifelse(number_first, "0 (0 %s)", "0 %s (0)"), percent_sign)
    ret <- rbind(ret, rep(empty, times=ncol(ret)))
    rownames(ret) <- c("Mean (SD)", "Missing")
  } else {
    names(ret) <- "Mean (SD)"
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
#' @param horizontal_proportions Is only active if show_missing since this is 
#'  the only case of a proportion among continuous variables. This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the 
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command. Note! This calls the  
#'  \code{\link{describeFactors}} since the horizontal interpretation looses the 
#'  vertical information in the second category and is thus better 
#'  interpreted as a whole.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @seealso \code{\link{getDescriptionStatsBy}}
#' 
#' @examples
#' describeMedian(1:10)
#' describeMedian(c(1:10, NA), show_missing=TRUE)
#' 
#' @author max
#' @export
describeMedian <- function(x, iqr=TRUE, html=FALSE, digits=1, number_first = TRUE, show_missing = FALSE, horizontal_proportions=NULL){
  show_missing <- prConvertShowMissing(show_missing)
  
  if (iqr)
    range_quantiles = c(1/4, 3/4)
  else
    range_quantiles = c(0, 1)
  
  ret <- c(sprintf(sprintf("%%.%df (%%3.%df - %%3.%df)", digits, digits, digits),
    median(x, na.rm=T), 
    quantile(x, probs=range_quantiles[1], na.rm=T),
    quantile(x, probs=range_quantiles[2], na.rm=T)))
  
  if (show_missing %in% c("ifany", "always") & sum(is.na(x))>0){
    missing <- describeFactors(is.na(x), number_first = number_first, digits = digits, html = html, 
      horizontal_proportions = horizontal_proportions)
    ret <- rbind(ret, missing["TRUE", ])
    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing")
  } else if (show_missing == "always"){
    percent_sign <- ifelse(html, "%", "\\%")
    empty <- sprintf(ifelse(number_first, "0 (0 %s)", "0 %s (0)"), percent_sign)
    ret <- rbind(ret, rep(empty, times=ncol(ret)))
    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing")
  } else {
    names(ret) <- ifelse(iqr, "Median (IQR)", "Median (range)")
  }
  
  return (ret)
}

#' A function that returns a description proportion that contains
#' the number and the percentage
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used for the percentage 
#' @param html If HTML compatible output should be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other. This will always be converted into the describeFactor
#'  function if there is a missing row.
#' @param horizontal_proportions This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the 
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command. Note! This calls the  
#'  \code{\link{describeFactors}} since the horizontal interpretation looses the 
#'  vertical information in the second category and is thus better 
#'  interpreted as a whole.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @seealso \code{\link{getDescriptionStatsBy}}, \code{\link{describeFactors}} 
#' @examples
#' describeProp(factor(sample(50, x=c("A","B"), replace=TRUE)))
#' describeProp(factor(sample(50, x=c("A","B", NA), replace=TRUE)), show_missing=TRUE)
#'  
#' @author max
#' @export
describeProp <- function(x, 
  html=FALSE, 
  digits=1, 
  number_first = TRUE, 
  show_missing = FALSE,
  horizontal_proportions = NULL){
  show_missing <- prConvertShowMissing(show_missing)
  
  # If we're to use the horizontal proportions then
  # it's better to report the variable as a factor
  # instead of a single proportion
  if(is.null(horizontal_proportions) == FALSE | 
    show_missing == "ifany" & any(is.na(x)) |
    show_missing == "always")
    return(describeFactors(x=x, html=html, number_first = number_first, 
      show_missing = show_missing, horizontal_proportions = horizontal_proportions))
  
  if (is.factor(x) == FALSE)
    x <- factor(x)
  
  no <- sum(x == levels(x)[1], na.rm=T)

  # Don't count missing since those are treated as factors if any
  percent <- 100*no/length(x[is.na(x)==F])
  
  no <- ifelse(no >= 10^4, format(no, big.mark=" "), format(no))
  # The LaTeX treats % as comments unless it's properly escaped
  percent_sign <- ifelse (html, "%", "\\%")
  
  if (number_first)
    ret <- sprintf(sprintf("%%s (%%.%df %%s)", digits), no, percent, percent_sign)
  else
    ret <- sprintf(sprintf("%%.%df %%s (%%s)", digits), percent, percent_sign, no)
    
  return (ret)
}


#' A function that returns a description proportion that contains
#' the number and the percentage
#' 
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used for the percentage 
#' @param html If HTML compatible output should be used instead
#'  of default LaTeX
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @param show_missing This indicates if missing should be added as a separate
#'  row below all other.
#' @param horizontal_proportions This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the 
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command.
#' @return A string formatted for printing either latex by  HTML
#' 
#' @seealso \code{\link{getDescriptionStatsBy}}
#' 
#' @examples
#' set.seed(1)
#' describeFactors(sample(50, x=c("A","B", "C"), replace=TRUE))
#' describeFactors(sample(50, x=c("A","B", "C", NA), replace=TRUE), show_missing=TRUE)
#' 
#' n <- 500
#' my_var <- factor(sample(size=n, x=c("A","B", "C", NA), replace=TRUE))
#' my_exp <- rbinom(n=n, size=1, prob=0.2)
#' total <- table(my_var, useNA="ifany")
#' by(my_var,
#'    INDICES=my_exp,
#'    FUN=describeFactors,
#'    show_missing="ifany",
#'    horizontal_proportions = total)
#' 
#' @author max
#' @export
describeFactors <- function(x, html=FALSE, 
  digits=1, number_first = TRUE, 
  show_missing = FALSE, horizontal_proportions = NULL) {
  
  show_missing <- prConvertShowMissing(show_missing)
  
  # Get basic data
  table_results <- table(x, useNA= show_missing)
  values <- ifelse(table_results >= 10^4, format(table_results, big.mark=" ", trim=TRUE), 
    format(table_results, trim=TRUE))
  
  # Check if we should relate to an external total
  if (is.null(horizontal_proportions) == FALSE){
    
    # Error check the horizontal_proporitons variable
    # First check that it's a table or at least a vector
    if (is.numeric(horizontal_proportions) == FALSE ||
      (class(horizontal_proportions) != "table" &&
        is.vector(horizontal_proportions) == FALSE))
      stop(sprintf("You have not provided a proper table/vector variable for the function, the class you've given is: '%s', instead of numeric vector or table", 
          class(horizontal_proportions)))
    
    # The original table should always be longer than the subtable
    if (length(horizontal_proportions) < length(table_results))
      stop(sprintf("There is a length discrepancy between the number of groups in the given sample and the reference sample, %d < %d!",
          length(horizontal_proportions), length(table_results)))
    
    # All variables should exist in the horizontal table
    if (all(names(table_results) %in% names(horizontal_proportions)) == FALSE)
      stop(sprintf("Your results contain results that are not in the reference horizontal_proportions: %s",
          paste(names(table_results)[names(table_results) %in% names(horizontal_proportions)], collapse=", ")))
    
    # Create a equal length array with the same order
    if(length(horizontal_proportions) > length(table_results)){
      # There seems to be a few variables more in the horizontal version and 
      # then we need to fill in those values with 0
      # Initiate an empty array
      tmp <- rep(0, times=length(horizontal_proportions))
      # The array should have the names of the results
      # to be able to reference them
      names(tmp) <- names(horizontal_proportions)
      # Iterate and copy the values
      for (n in names(tmp)){
        if (n %in% names(table_results)){
          tmp[n] <- table_results[n]
        }
      }
      # Reset the table_results to our new array
      table_results <- tmp
      class(tmp) <- "table"
      # Redo the values variable
      values <- ifelse(table_results >= 10^4, format(table_results, big.mark=" ", trim=TRUE), 
        format(table_results, trim=TRUE))
    }
    
    # Initiate an empty array
    percentages <- rep(NA, times=length(horizontal_proportions))
    # The array should have the names of the results
    # to be able to reference them
    names(percentages) <- names(horizontal_proportions)
    for (n in names(horizontal_proportions)){
      # Strange things happen with NA...
      # Need to find that element in a slightly odd way
      if (is.na(n))
        percentages[is.na(names(percentages))] <- 
          table_results[is.na(names(table_results))]/horizontal_proportions[is.na(names(horizontal_proportions))]*100
      else
        percentages[n] <- table_results[n]/horizontal_proportions[n]*100
    }
  }else{
    percentages <- table_results/sum(table_results)*100
  }
  
  # The LaTeX treats %% as comments unless it's properly escaped
  percent_sign <- ifelse (html, "%", "\\%")
  
  if (number_first)
    ret <- matrix(
      sprintf(sprintf("%%s (%%.%df %%s)", digits), 
        values, 
        percentages, 
        percent_sign), ncol=1)
  else
    ret <- matrix(
      sprintf(sprintf("%%.%df %%s (%%s)", digits), 
        percentages, 
        percent_sign, 
        values), ncol=1)
  
  rn <- names(table_results)
  rn[is.na(rn)] <- "Missing"
  rownames(ret) <- rn
  return(ret)
}
