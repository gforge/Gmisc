#' This function helps with printing regression models
#' 
#' This function is used for getting the adjusted and unadjusted values
#' for a regression model. It takes a full model and walks through each
#' variable, removes in the regression all variables except one then
#' reruns that variable to get the unadjusted value. This functions not
#' intended for direct use, it's better to use \code{\link{printCrudeAndAdjustedModel}}
#' that utilizes this function.
#' 
#' This function saves a lot of time creating tables since it compiles a fully
#' unadjusted list of all your used covariates.
#' 
#' If the model is an exponential poisson/logit/cox regression model then it automatically
#' reports the exp() values instead of the original values
#' 
#' The function skips by default all spline variables since this becomes very complicated
#' and there is no simple \deqn{\beta}{beta} to display. For the same reason it skips
#' any interaction variables since it's probably better to display these as a contrast table. 
#' 
#' Note that the rms regression has a separate function that uses the rms:::summaryrms function
#' that returns a matrix that is then pruned.
#' 
#' @param fit The regression model
#' @param level The confidence interval level
#' @param remove_interaction_vars Removes the interaction terms as this makes no sense in the output
#' @param ... Not used
#' @return Returns a matrix with the columns: 
#'   \code{c("Crude", "2.5 \%", "97.5 \%", "Adjusted", "2.5 \%", "97.5 \%")}.
#'   The row order is not changed from the original fit. The percentages can vary depending
#'   on the set level.
#'
#' @seealso \code{\link{printCrudeAndAdjustedModel}}
#' 
#' @example examples/getCrudeAndAdjustedModelData_example.R
#' 
#' @importFrom stringr str_split
#' 
#' @rdname getCrudeAndAdjustedModelData
#' @author max
#' @export
getCrudeAndAdjustedModelData <- function(fit, level, ...)
  UseMethod("getCrudeAndAdjustedModelData")

#' @author max
#' @rdname getCrudeAndAdjustedModelData
#' @method getCrudeAndAdjustedModelData default
#' @S3method getCrudeAndAdjustedModelData default
getCrudeAndAdjustedModelData.default <- function(fit, level=.95, remove_interaction_vars = TRUE, ...){
  # Just a prettifier for the output an alternative could be:
  # paste(round(x[,1],1), " (95% CI ", min(round(x[,2:3])), "-", max(round(x[,2:3])), ")", sep="") 
  get_coef_and_ci <- function(fit, skip_intercept=FALSE){
    # Get the coefficients
    my_coefficients <- coef(fit)
    coef_names <- names(my_coefficients)
    
    ci <- suppressMessages(confint(fit, level=level))
    
    if (skip_intercept){
      intercept <- grep("[iI]ntercept", coef_names)
      if (length(intercept) > 0){
        my_coefficients <- my_coefficients[-intercept]
        ci <- ci[-intercept,]
        coef_names <- coef_names[-intercept]
      }
    }
    
    # Use the exp() if logit or cox regression
    if ("coxph" %in% class(fit) ||
      (!is.null(fit$family$link) &&
        fit$family$link %in% c("logit", "log"))){
      my_coefficients <- exp(my_coefficients)
      ci <- exp(ci)
    }
    
    if (length(my_coefficients) > 1)
      ret_val <- cbind(my_coefficients, ci)
    else
      ret_val <- matrix(c(my_coefficients, ci), nrow=1)
    
    colnames(ret_val) <- c("", 
      sprintf("%.1f%%", 100*(1-level)/2),
      sprintf("%.1f%%", 100*(level + (1-level)/2)))
    rownames(ret_val) <- coef_names
    return(ret_val)
  }
  
  var_names <- prGetModelVariables(fit, 
      remove_interaction_vars = remove_interaction_vars,
      add_intercept = TRUE)
  if (length(var_names) == 0)
    stop("You have no variables that can be displayed as adjusted/unadjusted",
      " since they all are part of an interaction, spline or strata.")
  
  # Get the adjusted variables
  adjusted <- get_coef_and_ci(fit)
  # When using splines, rcs in cox regression this shows a little different
  
  keep <- c()
  for (name in var_names){
    matches <- grep(name, rownames(adjusted), fixed=TRUE)
    if(length(matches) > 0)
      keep <- append(keep, matches)
  }
  if (length(keep) == 0)
    stop("Error when trying to extract the variable names",
      " from the adjusted values. These names: ", paste(var_names, collapse=", "),
      "\n seem not to exist within the rownames of: ", paste(rownames(adjusted), collapse=", "))
  
  adjusted <- adjusted[keep, ,drop=FALSE]
  
  unadjusted <- c()
  for(variable in var_names){
    if (!grepl("[iI]ntercept", variable)){
      # Run the same fit but with only one variable
      fit_only1 <- update(fit, paste(".~", variable))
      
      # Get the coefficients processed with some advanced
      # round part()
      new_vars <- get_coef_and_ci(fit_only1, skip_intercept = TRUE)
      
      # Add them to the previous
      unadjusted <- rbind(unadjusted, new_vars)
    }else{
      # Run the same fit but without any variables
      fit_only1 <- update(fit, ".~ 1")
      
      # Get the coefficients
      new_vars <- get_coef_and_ci(fit_only1, skip_intercept = FALSE)
      
      # Add
      unadjusted <- rbind(new_vars, unadjusted)
      
      # Change name
      rownames(unadjusted)[1] <- "Intercept"
    }
  }
  
  # If just one variable it's not a proper matrix
  if (is.null(dim(adjusted))){
    both <- matrix(c(unadjusted, adjusted), nrow=1)
  }else{
    both <- cbind(unadjusted, adjusted)
  }
  
  levels_str <- c(sprintf("%.1f %%", 100*(1-level)/2),
    sprintf("%.1f %%", 100*(level + (1-level)/2)))
  
  colnames(both) <- c("Crude", levels_str,
    "Adjusted", levels_str)
  return(both)
}

