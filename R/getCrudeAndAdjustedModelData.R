#' This function helps with printing regression models
#' 
#' This function is used for printing the adjusted and unadjusted values
#' for a regression model. It takes a full model and walks through each
#' variable, removes in the regression all variables except one then
#' reruns that variable to get the unadjusted value.
#' 
#' This function saves a lot of time creating tables since it compiles a fully
#' unadjusted list of all your used covariates.
#' 
#' If the model is an exponential logit/cox regression model then it automatically
#' reports the exp() values instead of the original values
#' 
#' The function skips by default all spline variables since this becomes very complicated
#' and there is no simple \deqn{\beta}{beta} to display. For the same reason it skips
#' any interaction variables since it's probably better to display these as a contrast table. 
#' 
#' @param fit The regression model
#' @param digits Number of digits you want in your estimates
#' @param max A number that specifies if any values should be abbreviated above this value,
#'   for instance a value of 1000 would give a value of > 1000 for a value of 1001. This gives
#'   a prettier table when you have very wide confidence intervals.
#' @return Returns a matrix with the columns c("Crude", "95% CI", "Adjusted", "95% CI").
#'   The rows are not changed from the original fit.
#' 
#' @example examples/getCrudeAndAdjustedModelData_example.R
#' 
#' @author max
#' @export
getCrudeAndAdjustedModelData <- function(fit, digits=2, max=Inf){
  # Just a prettifier for the output an alternative could be:
  # paste(round(x[,1],1), " (95% CI ", min(round(x[,2:3])), "-", max(round(x[,2:3])), ")", sep="")  
  get_coef_and_ci <- function(fit, skip_intercept=FALSE){
    # Just to make sure that it gives 1.0 and 
    # not 1 if digits = 1, in cases where a 
    # adding another decimal that is used
    # since everyone is so hyped about p-val < 0.05
    add_zero_to_var <- function(x){
      return(sprintf(sprintf("%%0.%df", digits), x))
    }
    
    # Get coefficients and conf. interval
    my_coefficients <- coef(fit)
    ci <- confint(fit)
    coef_names <- names(my_coefficients)
    
    if (skip_intercept){
      intercept <- grep("Intercept", coef_names)
      if (length(intercept)){
        my_coefficients <- my_coefficients[-intercept]
        ci <- ci[-intercept,]
        coef_names <- coef_names[-intercept]
      }
    }
    
    # Use the exp() if logit or cox regression
    if ("coxph" %in% class(fit) ||
      ("glm" %in% class(fit) &&
        fit$family$link == "logit")){
      my_coefficients <- exp(my_coefficients)
      ci <- exp(ci)
    }
    
    confidence_string <- sprintf("%%0.%df to %%0.%df", digits, digits)
    if (length(my_coefficients) > 1){
      my_coefficients <- tapply(my_coefficients, 1:length(my_coefficients), FUN = add_zero_to_var)
      
      # Set upper bound for nicer output
      if (is.infinite(max) == FALSE){
        if (floor(max) == max)
          ci[ci[,2] > max, 2] <- sprintf("> %d", max)
        else
          ci[ci[,2] > max, 2] <- sprintf("> %f", max)
      }
    
      ci <- sprintf(confidence_string, ci[,1], ci[,2])
    }else{
      my_coefficients <- add_zero_to_var(my_coefficients)
      
      if (is.infinite(max) == FALSE &&  
          ci[2] > max){
        if (floor(max) == max)
          ci[2] <- sprintf("> %d", max)
        else
          ci[2] <- sprintf("> %f", max)
      }
      
      ci <- sprintf(confidence_string, ci[1], ci[2])
    }
    
    ret_val <- cbind(my_coefficients, ci)
    colnames(ret_val) <- c("", "2.5% to 97.5%")
    rownames(ret_val) <- coef_names
    return(ret_val)
  }
  
  # Extract all the term names
  all.terms <- terms(fit)	
  var_names <- attr(all.terms, 'term.labels')
  
  # Skip variables consisting of
  # functions such as spline, strata variables
  regex_for_unwanted_vars <- "^(ns|rcs|bs|pspline)[(]"
  skip_variables <- grep(regex_for_unwanted_vars, var_names)
  skip_variables_names <- c()
  if (length(skip_variables) > 0){
    for (vn in skip_variables){
      match <- gregexpr("\\w+\\((?<var_name>\\w+)", var_names[vn], perl = TRUE)[[1]]
      if (match[1] >= 0){
        st <- attr(match, "capture.start")[1, ]
        skip_variables_names <- append(skip_variables_names, substring(var_names[vn], st, st + attr(match, "capture.length")[1, ] - 1))
      }
    }
  }
  
  # Skips the interaction terms since they should
  # be displayed in a different fashion
  raw_int_terms <- grep("[[:alnum:]]+:[[:alnum:]]+", var_names)
  if (length(raw_int_terms) > 0){
    require("stringr")
    for (i in raw_int_terms){
      for (name in str_split(var_names[i], ":")[[1]]){
        skip_variables_names <- append(skip_variables_names, name)
        skip_variables <- union(skip_variables, grep(name, var_names))
      }
    }

  }
  
  skip_variables <- union(skip_variables, grep("^strat[a]{0,1}\\(", var_names))
  
  # Get the adjusted variables
  adjusted <- get_coef_and_ci(fit)
  # When using splines, rcs in cox regression this shows a little different
  
  # Remove all the splines, rcs etc
  rn <- rownames(adjusted)
  remove <- grep("(\'{1,}|[[][0-9]+[]]|[)][0-9]+)$", rn)
  for (name in skip_variables_names){
    m <- grep(name, rn)
    if (length(m) > 0)
      remove <- union(remove, m)
  }
  
  # Remove the unwanted rows if any found
  if (length(remove) > 0){
    adjusted <- adjusted[-remove, ]
  }
  
  unadjusted <- c()
  if (length(skip_variables) > 0){
    variables_to_check <- var_names[-skip_variables]
  }else{
    variables_to_check <- var_names
  }
  for(variable in variables_to_check){
    interaction_variable <- length(grep(":", variable)) > 0
    
    # If it's an interaction variable the
    # interacting parts have to be included  
    if (interaction_variable){
      variable <- paste(paste(unlist(strsplit(variable, ":")), sep="", collapse=" + "), variable, sep=" + ")
    }
    
    # Run the same fit but with only one variable
    fit_only1 <- update(fit, paste(".~", variable))
    
    # Get the coefficients processed with some advanced 
    # round part()
    new_vars <- get_coef_and_ci(fit_only1, skip_intercept = TRUE)
    
    # If interaction then we should only keep the 
    # interaction part - the other variables are
    # always included by default and need therefore
    # to be removed
    if (interaction_variable){
      new_vars <- new_vars[grep("[*:]", rownames(new_vars)),]
    }
    
    # Add them to the previous
    unadjusted <- rbind(unadjusted, new_vars)
  }
  
  # If regression contains Intercept 
  # this is meaningless for the comparison
  # of adjusted and unadjusted values 
  if (length(grep("Intercept", rownames(adjusted))) > 0)
  {
    unadjusted <- rbind(c("-", "-"), unadjusted)
    rownames(unadjusted)[1] <- "Intercept"
  }
  
  # If just one variable it's not a proper matrix
  if (is.null(dim(adjusted))){
    both <- matrix(c(unadjusted, adjusted), nrow=1)
  }else{
    both <- cbind(unadjusted, adjusted)
  }
  colnames(both) <- c("Crude", "95% CI", "Adjusted", "95% CI") 
  return(both)
}