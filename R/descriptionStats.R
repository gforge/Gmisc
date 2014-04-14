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
#' @param show_missing_digits The number of digits to use for the 
#'  missing percentage, defaults to the overall \code{digits}.
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
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable. Note, this is 
#'  only used when combined with the missing information.
#' @param plusmin_str Provide if you want anything other than the plus minus sign
#'  suited for the given output format.
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator in the \code{\link{outputInt}}
#'  function.
#' @param ... Passed on to \code{\link{describeFactors}}
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
describeMean <- function(x, 
  html=FALSE, 
  digits=1, 
  number_first = TRUE, 
  show_missing = FALSE, 
  show_missing_digits = digits,
  horizontal_proportions=NULL,
  percentage_sign = TRUE,
  plusmin_str,
  languge = "en",
  ...){
  show_missing <- prConvertShowMissing(show_missing)

  if (missing(plusmin_str))
    if (html)
      plusmin_str <- "&plusmn;"
    else
      plusmin_str <- "\\pm"
  
  ret <- c(sprintf(sprintf("%%.%df (%s%%3.%df)", digits, plusmin_str, digits), 
    mean(x, na.rm=T), sd(x, na.rm=T)))
  
  # LaTeX complains if any formula isn't encapsulated within $ signs
  if (html == FALSE)
    ret <- sprintf("$%s$", ret)
  
  if (show_missing %in% c("ifany", "always") & sum(is.na(x))>0){
    missing <- describeFactors(is.na(x), number_first = number_first, 
                               percentage_sign=percentage_sign, 
                               languge = languge,
                               digits = show_missing_digits, html = html,
                               horizontal_proportions = horizontal_proportions,
                               ...)
    ret <- rbind(ret, missing["TRUE", ])
    rownames(ret) <- c("Mean (SD)", "Missing")
  } else if (show_missing == "always"){
    if(percentage_sign == TRUE)
      percentage_sign <- ifelse (html, "%", "\\%")
    else if(is.character(percentage_sign) == FALSE)
      percentage_sign = ""

    empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"), percentage_sign)
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
#' @param show_missing_digits The number of digits to use for the 
#'  missing percentage, defaults to the overall \code{digits}.
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
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable. Note, this is 
#'  only used when combined with the missing information.
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator in the \code{\link{outputInt}}
#'  function.
#' @param ... Passed on to \code{\link{describeFactors}}
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
describeMedian <- function(x, 
  iqr=TRUE, 
  html=FALSE, 
  digits=1, 
  number_first = TRUE, 
  show_missing = FALSE, 
  show_missing_digits = digits,
  horizontal_proportions=NULL,
  percentage_sign = TRUE,
  languge = "en",
  ...){
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
    missing <- describeFactors(is.na(x), number_first = number_first, 
                               percentage_sign=percentage_sign,
                               languge = languge,
                               digits = show_missing_digits, html = html, 
                               horizontal_proportions = horizontal_proportions,
                               ...)
    ret <- rbind(ret, missing["TRUE", ])
    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing")
  } else if (show_missing == "always"){
    if(percentage_sign == TRUE)
      percentage_sign <- ifelse (html, "%", "\\%")
    else if(is.character(percentage_sign) == FALSE)
      percentage_sign = ""
    
    empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"), percentage_sign)
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
#' @param show_missing_digits The number of digits to use for the 
#'  missing percentage, defaults to the overall \code{digits}.
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
#' @param default_ref If you use proportions with only one variable
#'  it can be useful to set the reference level that is of interest to show. This can 
#'  wither be "First", level name or level number.  
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable.
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator in the \code{\link{outputInt}}
#'  function.
#' @param ... Passed on to \code{\link{outputInt}}
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
  show_missing_digits = digits,
  horizontal_proportions = NULL,
  default_ref = "First",
  percentage_sign = TRUE,
  languge = "en"){
  show_missing <- prConvertShowMissing(show_missing)
  
  default_ref <- prGetAndValidateDefaultRef(x, default_ref)
  
  # If we're to use the horizontal proportions then
  # it's better to report the variable as a factor
  # instead of a single proportion.
  # When we have missing it also gets more difficult
  # to just report one percentage as it suddenly uncertain
  # for what percentage the number applies to
  if(is.null(horizontal_proportions) == FALSE || 
       (show_missing == "ifany" && 
          any(is.na(x))) ||
       show_missing == "always")
    return(describeFactors(x=x, 
                           html=html, 
                           number_first = number_first, 
                           digits = digits,
                           percentage_sign=percentage_sign,
                           languge = languge,
                           show_missing = show_missing, 
                           show_missing_digits = show_missing_digits,
                           horizontal_proportions = horizontal_proportions,
                           ...))
  
  if (is.factor(x) == FALSE)
    x <- factor(x)
  
  no <- sum(x == levels(x)[default_ref], na.rm=T)

  # Don't count missing since those are treated as factors if any
  percent <- 100*no/length(x[is.na(x)==FALSE])
  
  no <- outputInt(no, languge=languge, html=html, ...=...)
  
  # The LaTeX treats % as comments unless it's properly escaped
  if(percentage_sign == TRUE)
    percentage_sign <- ifelse (html, "%", "\\%")
  else if(is.character(percentage_sign) == FALSE)
    percentage_sign = ""
  
  if (number_first)
    ret <- sprintf(sprintf("%%s (%%.%df%%s)", digits), no, percent, percentage_sign)
  else
    ret <- sprintf(sprintf("%%.%df%%s (%%s)", digits), percent, percentage_sign, no)
    
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
#' @param show_missing_digits The number of digits to use for the 
#'  missing percentage, defaults to the overall \code{digits}.
#' @param horizontal_proportions This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the 
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command.
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable.
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator in the \code{\link{outputInt}}
#'  function.
#' @param ... Passed on to \code{\link{outputInt}}
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
describeFactors <- function(x, 
  html=FALSE, 
  digits=1, 
  number_first = TRUE, 
  show_missing = FALSE, 
  show_missing_digits = digits,
  horizontal_proportions = NULL,
  percentage_sign = TRUE,
  languge = "en",
  ...) {
  
  show_missing <- prConvertShowMissing(show_missing)
  
  # Get basic data
  table_results <- table(x, useNA= show_missing)
  
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
  
  # Format the values
  values <- sapply(table_results, outputInt, languge=languge, html=html, ...=...)
  
  # The LaTeX treats %% as comments unless it's properly escaped
  if(percentage_sign == TRUE)
    percentage_sign <- ifelse (html, "%", "\\%")
  else if(is.character(percentage_sign) == FALSE)
    percentage_sign = ""

  # The is.na(...) is a little overkill
  if (number_first)
    ret <- matrix(
      sprintf(ifelse(is.na(names(table_results)),
                     sprintf("%%s (%%.%df%%s)", show_missing_digits), 
                     sprintf("%%s (%%.%df%%s)", digits)),
              values, 
              percentages, 
              percentage_sign), ncol=1)
  else
    ret <- matrix(
      sprintf(ifelse(is.na(names(table_results)),
                     sprintf("%%.%df%%s (%%s)", show_missing_digits), 
                     sprintf("%%.%df%%s (%%s)", digits)),
              percentages, 
              percentage_sign, 
              values), ncol=1)
  
  rn <- names(table_results)
  rn[is.na(rn)] <- "Missing"
  rownames(ret) <- rn
  return(ret)
}
