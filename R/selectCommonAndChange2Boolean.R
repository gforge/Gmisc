#' A function for selecting covariates and changing the score to TRUE/FALSE
#'
#' This function uses the select frequent scores and then changes the scores
#' that are frequent into values of TRUE/FALSE. The scores values are usually
#' not that interesting unless you merge them into a combined score and therefore
#' this functions converts the score into a boolean value.
#'  
#' @param score A matrix/df with all the columns being scores that are checked  
#' @param censored This is the censored/outcome vector. If a score occurs 
#'   not so frequently but it is _very_ strongly related to the outcome the
#'   you have a less strict selection. This selection is the min_with_occurrences
#' @param min The minimum of times that the score may occur and still be interesting.
#'   Note this is NOT a percentage since it may be interesting to have very low 
#'   percentages if your material is big.
#' @param min_with_occurences A smaller number than min since this gives an option
#'   to select more rare indicators if they're strongly associated with the 
#'   outcome
#' @param censored_indicator The indicator for the censored vector that indicates
#'   that an event/outcome has occurred 
#' @return Returns the selected columns from the score variable
#'
#' @example examples/selectCommonAndChange2Boolean_example.R
#'  
#' @author max
#' @export
selectCommonAndChange2Boolean <- function(score, 
  censored, 
  min=300, 
  min_with_occurences=10,
  censored_indicator="reoperation")
{
  keepers <- prSelectFrequentVariables(score, 
    censored = censored, 
    min, 
    min_with_occurences)
  score <- score[keepers]
  for (i in 1:length(score)){
    score[,i] <- score[,i] > 0
  }
  return(score)
}