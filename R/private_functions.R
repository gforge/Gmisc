# This file contains all the helper funcitons that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################


#' Get statistics according to the type
#' 
#' A simple function applied by the \code{\link{getDescriptionStatsBy}}
#' for the total column. This function is also used by \code{\link{orintCrudeAndAdjustedModel}}
#' in case of a basic linear regression is asked for a raw stat column
#'  
#' @param x The variable that we want the statistics for 
#' @param show_perc If this is a factor/proportion variable then we
#'  might want to show the percentages 
#' @param html If the output should be in html or LaTeX formatting
#' @param digits Number of decimal digits
#' @param numbers_first If number is to be prior to the percentage
#' @param show_missing If missing should be included 
#' @param continuous_function A function for describing continuous variables
#'  defaults to \code{\link{describeMean}} 
#' @param prop_function A function for describing proportions, defaults to
#'  the factor function
#' @param factor_function A function for describing factors, defaults to
#'  \code{\link{describeFactor}}
#' @return A matrix or a vector depending on the settings
#' 
#' @author max
#' @export
prGetStatistics <- function(x, 
  show_perc = FALSE, 
  html = TRUE, 
  digits = 1, 
  numbers_first = TRUE, 
  show_missing = TRUE, 
  continuous_function = describeMean, 
  factor_function = describeFactors,
  prop_function = factor_function){
  if (is.factor(x)){
    if (total_col_show_perc)
      total_table <- factor_function(x, 
        html=html, 
        digits=digits,
        number_first=numbers_first, 
        show_missing = show_missing)
    else{
      total_table <- table(x[is.na(by) == FALSE], useNA=show_missing)
      names(total_table)[is.na(names(total_table))] <- "Missing"
    }
    
  }else{
    total_table <- continuous_function(x[is.na(by) == FALSE], 
      html=html, digits=digits, 
      number_first=numbers_first, 
      show_missing = show_missing)
    
    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      total_col_show_perc == FALSE)
      total_table[2] <- sum(is.na(x))
  }
  return(total_table)
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

#' A functuon for converting a show_missing variable
#' 
#' The variable is suppose to be directly compatible with
#' table(..., useNA=show_missing). It throughs an error
#' if not compatible
#' 
#' @param show_missing Boolean or "no", "ifany", "always" 
#' @return string 
#' 
#' @author max
prConvertShowMissing <- function(show_missing){
  if (show_missing == FALSE || show_missing == "no")
    show_missing <- "no"
  else if (show_missing == TRUE)
    show_missing <- "ifany"
  
  if (show_missing %nin% c("no", "ifany", "always"))
    stop(sprintf("You have set an invalid option for show_missing variable, '%s' ,it should be boolean or one of the options: no, ifany or always.", show_missing))
  
  return(show_missing)
}
