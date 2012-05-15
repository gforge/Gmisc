#' A functions for selecting the scores that have a certain frequency
#' 
#' Used to remove for instance HIV from Charlsons, Elixhausers and other
#' rare scores that probably don't have any predictive value due to their 
#' rareness. 
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
#' @returnType vector
#' @return Returns a vector with the column names that are to be kept, colnames(score)[keepers]
#' 
#' @author max
#' @export
select_frequent_scores <- function(score, 
		censored, 
		min,  
		min_with_occurences = NULL,
		censored_indicator = "reoperation"){
	if (is.null(min_with_occurences)){
		min_with_occurences <- min
	}else if (min > min_with_occurences){
        warning("Your min_with occurences is smaller than the min - this must be a misstake")
        min = min_with_occurences
    }
    
    cat("\n\n* Begin selecting common scores *")
    keepers <- c()
    for(i in 1:length(score)){
        occurences <- sum(score[,i] > 0)
        if (occurences >= min_with_occurences){
            events <- length(censored[score[,i] > 0 & censored=="reoperation"])
            if (occurences > min | events > min_with_occurences){
                keepers <- append(keepers, i)
                # Normalize score to values 0 or 1
                score[,i] <- (score[,i] > 0)*1
            }else{
                cat("\nRemoving:", 
                        colnames(score)[i],
                        "occurences=",
                        occurences, "re-operations=", events)
            }
        }else{
            cat("\nRemoving:", 
                    colnames(score)[i],
                    "occurences=", occurences)
        }
    }
    
    cat("\n* End selecting common scores *")
    
    return(keepers)
}

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
#' @returnType matrix 
#' @return Returns the selected columns from the score variable
#' 
#' @author max
#' @export
select_and_change_to_boolean <- function(score, 
		censored, 
		min=300, 
		min_with_occurences=10,
		censored_indicator="reoperation")
{
  keepers <- select_frequent_scores(score, 
		  censored = censored, 
		  min, 
		  min_with_occurences)
  score <- score[keepers]
  for (i in 1:length(score)){
      score[,i] <- score[,i] > 0
  }
  return(score)
}


#' Counts the number of occurrences of a certain score
#' 
#' Used mostly for diagnostics
#' 
#' @param score Matrix with all the scores 
#' @returnType Void
#' 
#' @author max
#' @export
print_count_common_scores <- function(score){
    cat("\n\n* Begin counting common scores *")
    for(i in 1:length(score)){
        cat("\nFound for row:", 
            colnames(score)[i],
            length(score[,i][score[,i] > 0]),
            "hits")
    }
    
    cat("\n* End selecting common scores *")
}


#' Scruccas code that is a helper for factoring for the cmprsk package
#' 
#' The cmprsk package isn't fully evolved and this function helps it by
#' creating dummy variables.
#' 
#' @param x The variable, a vector (column) 
#' @param baseline The baseline to reference
#' @returnType matrix
#' @return Returns a matrix with all the dummy variables
#' 
#' @author max
#' @export
scrucca_factor2ind <- function(x, baseline)
{
# Given a factor variable x, create an indicator matrix of dimension 
# length(x) x (nlevels(x)-1) dropping the column corresponding to the 
# baseline level (by default the first level is used as baseline).
# Example:
# > x = gl(4, 2, labels = LETTERS[1:4])
# > factor2ind(x)
# > factor2ind(x, "C")
  xname <- deparse(substitute(x))
  n <- length(x)
  x <- as.factor(x)
  if(!missing(baseline)) x <- relevel(x, baseline)
  X <- matrix(0, n, length(levels(x)))
  X[(1:n) + n*(unclass(x)-1)] <- 1
  dimnames(X) <- list(names(x), paste(xname, levels(x), sep = ":"))
  return(X[,-1,drop=FALSE])
}

#' Prints the cph output
#' 
#' This is a different way of outputting the cph outcome
#' from the rms package. I needed something a little simpler
#' than the general summary and therefore created this function.
#' 
#' @param fit The cph return object 
#' @param round How many integers to display
#' @returnType matrix
#' @return Returns a matrix with columns HR, 2.5%, 97.5 % and Pr(>|Z|)
#' 
#' @author max
#' @export
output_cph <- function (fit, round = TRUE){
    beta <- fit$coefficients
    se <- sqrt(diag(fit$var))
    Z <- fit$coefficients/se
    p_vals <- 1 - pchisq(Z^2, 1)
    ci <- exp(confint(fit))
    ci[,2][ci[,2] > 10^4] <- Inf
    if (round == TRUE){
        data <- cbind(round(exp(beta), 3), round(ci, 3), round(p_vals, 4))
    }else{
        data <- cbind(exp(beta), ci, p_vals)
    }
    
    colnames(data) <- c("HR", "2.5 %", "97,5 %", "Pr(>|Z|)")
    
    # Dump the splines
    splines <- rownames(data)[grep("[^']'$", rownames(data))]
    for(i in 1:length(splines)){
        # Clear the '
        splines[i] <- substr(rownames(data)[grep("[^']'$", rownames(data))][i], 1, nchar(rownames(data)[grep("[^']'$", rownames(data))][i])-1)
    }
    
    spline_grep <- paste("(", paste(splines, collapse="|"), ")", sep="")
    drop_rows <- grep(spline_grep, rownames(data))
    if (length(drop_rows) > 0){
        data <- data[-drop_rows, ]
    }
    
    return(data)
}


#' Not sure when I use this
#' @param cox A model fit
#' @returnType matrix
#' @return A matrix with columns beta, confidence interval and p-value 
#' 
#' @author max
calc_4_table <- function(cox){
    # Prepare the columns
    beta <- coef(cox)
    se   <- sqrt(diag(cox$var))
    p    <- 1 - pchisq((beta/se)^2, 1)
    CI   <- round(confint(cox), 3)
    
    return(cbind("exp(B)" = round(exp(beta), 2), CI, p=round(p, 3)))
}