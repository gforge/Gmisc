# This file contains all the helper funcitons that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################


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
#' @return Returns a vector with the column names that are to be kept, colnames(score)[keepers]
#' 
#' @author max
prSelectFrequentVariables <- function(score, 
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

#' Not sure when I use this
#' 
#' @param cox A model fit
#' @return A matrix with columns beta, confidence interval and p-value 
#' 
#' @author max
prCalc4Table <- function(cox){
  # Prepare the columns
  beta <- coef(cox)
  se   <- sqrt(diag(cox$var))
  p    <- 1 - pchisq((beta/se)^2, 1)
  CI   <- round(confint(cox), 3)
  
  return(cbind("exp(B)" = round(exp(beta), 2), CI, p=round(p, 3)))
}


#' Gets the boundaries for a survival fit
#' 
#' @param fit A survival model of either competing risk regression or cox regression type 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @author max
prGetFpDataFromSurvivalFit <- function (fit, 
  conf.int = 0.95,
  exp      = TRUE){
  # Get the p-value, I use the method in the
  # print.cph -> prModFit from the rms package
  Z <- coef(fit)/sqrt(diag(fit$var))
  p_val <- signif(1 - pchisq(Z^2, 1), 5)
  order <- rep(-1, length(beta))
  ci <- confint(fit, level=conf.int)
  
  if (exp){
    ret_matrix <- cbind(
      beta=exp(coef(fit)),
      p_val=p_val,
      low=exp(ci[,1]),
      high=exp(ci[,2]),
      order=order)
  }else{
    ret_matrix <- cbind(
      beta=coef(fit),
      p_val=p_val,
      low=ci[,1],
      high=ci[,2],
      order=order)
  }
  
  # Set the names of the rows
  rownames(ret_matrix) <- names(fit$coef)
  
  
  return(ret_matrix)
}

#' Gets the boundaries for a GLM fit that is poisson or quasipoisson based
#' 
#' @param glm.fit A regression model 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @author max
prGetFpDataFromGlmFit <- function(glm.fit, 
  conf.int = 0.95,
  exp      = TRUE){
  warning("The GLM part has not been properly tested. Please check that it seems right")
  
  summary_glm <- summary.glm(glm.fit)
  
  # Extract the summary values of interest
  summary_se <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Std. Error"]
  if ("quasipoisson" %in% glm.fit$family){
    summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|t|)"]
  }else if ("poisson" %in% glm.fit$family){
    summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|z|)"]
  }else{
    stop("Type of analysis not prepared!")
  }
  
  order = rep(-1, length(glm.fit$coefficients))
  ci <- confint(glm.fit, level=conf.int)
  
  if (exp){
    ret_matrix <- cbind(
      beta=exp(coef(glm.fit)),
      p_val=summary_p_val,
      low=exp(ci[,1]),
      high=exp(ci[,2]),
      order=order)
  }else{
    ret_matrix <- cbind(
      beta=coef(glm.fit),
      p_val=summary_p_val,
      low=ci[,1],
      high=ci[,2],
      order=order)
  }
  
  # Set the names of the rows
  rownames(ret_matrix) <- names(glm.fit$coefficients)
  
  # Remove the intercept
  ret_matrix <- ret_matrix[names(glm.fit$coefficients) != "(Intercept)", ]
  
  return(ret_matrix)
}


#' Gets the confidence interval, p-values,
#' coefficients from a survival object
#' 
#' @param model_fit A regression fit from CRR, coxph, cph object 
#' @param conf.int The interval of interest 0-1, see levels in confint()
#' @param exp If the value should be in exponential form (default)
#' @return A matrix with the columns: 
#' \item{beta}{The estimated coefficient}
#' \item{p_val}{P-value}
#' \item{low}{The lower confidence interval}
#' \item{high}{The upper confidence interval}
#' \item{order}{A column that later can be used in ordering}
#' 
#' @author max
prGetFpDataFromFit <- function(model_fit, 
  conf.int = 0.95,
  exp = TRUE){
  # Get the estimates, confidence intervals and the p_values
  if (any(class(model_fit) %in% "coxph") || 
    any(class(model_fit) %in% "crr")){
    sd <- prGetFpDataFromSurvivalFit(fit = model_fit, conf.int = conf.int, exp = exp)
  } else if (any(class(model_fit) %in% "glm")){
    sd <- prGetFpDataFromGlmFit(glm.fit = model_fit, conf.int = conf.int, exp = exp)
  } else {
    stop(paste("Unknown fit class type:", class(model_fit)))
  }
  
  return(sd)
}