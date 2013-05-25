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
#' If the model is an exponential logit/cox regression model then it automatically
#' reports the exp() values instead of the original values
#' 
#' The function skips by default all spline variables since this becomes very complicated
#' and there is no simple \deqn{\beta}{beta} to display. For the same reason it skips
#' any interaction variables since it's probably better to display these as a contrast table. 
#' 
#' @param fit The regression model
#' @return Returns a matrix with the columns c("Crude", "95\% CI", "Adjusted", "95\% CI").
#'   The rows are not changed from the original fit.
#'
#' @seealso \code{\link{printCrudeAndAdjustedModel}}
#' 
#' @example examples/getCrudeAndAdjustedModelData_example.R
#' 
#' @import boot
#' 
#' @author max
#' @export
getCrudeAndAdjustedModelData <- function(fit){
  
  if ("Glm" %in% class(fit))
    stop("The rms::Glm does not work properly with this function, please use regular glm instead")
  
  # Just a prettifier for the output an alternative could be:
  # paste(round(x[,1],1), " (95% CI ", min(round(x[,2:3])), "-", max(round(x[,2:3])), ")", sep="") 
  get_coef_and_ci <- function(fit, skip_intercept=FALSE){
    # Get the coefficients
    my_coefficients <- coef(fit)
    coef_names <- names(my_coefficients)
    
    # Confint isn't implemented for the rms package
    # we therefore need to do a work-around
    if ("rms" %in% class(fit) && "ols" %nin% class(fit)){
      # The summary function has to be invoked to get this to work
      # in the confint version and the summary.rms doesn't return
      # a profile.glm() compatible list, it returns a matrix
      # It works though OK for regular ordinary least square regression
      if ("lrm" %in% class(fit)){
        warning("Using the confint.default, i.e. Wald statistics, and it may not be optimal but profile CI is not implemented for the lrm, an option could be to change to the Glm with the family=binomial")
        ci <- confint.default(fit)
      }else if ("Glm" %in% class(fit)){
        # The summary.rms is causing issues when profiling,
        # it works fine when summary.glm is issued, we need to
        # change class for that
        class(fit) <- class(fit)["rms" != class(fit)]
        ci <- confint(fit)
      }else if ("cph" %in% class(fit)){
        # I haven't found any large issues with this call
        ci <- confint(fit)
      }else{
        # Untested option
        # try confint for better or worse
        warning("The function has not been tested with this type of regression object, may not work properly.")
        ci <- confint(fit)
      }
    }else{
      ci <- confint(fit)
    }
    
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
    
    if (length(my_coefficients) > 1)
      ret_val <- cbind(my_coefficients, ci)
    else
      ret_val <- matrix(c(my_coefficients, ci), nrow=1)
    
    colnames(ret_val) <- c("", "2.5%", "97.5%")
    rownames(ret_val) <- coef_names
    return(ret_val)
  }
  
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
  if (length(variables_to_check) == 0)
    stop("You have no variables that can be displayed as adjusted/unadjusted since they all are part of an interaction, spline or strata")
  
  for(variable in variables_to_check){
    interaction_variable <- length(grep(":", variable)) > 0
    
    # If it's an interaction variable the
    # interacting parts have to be included 
    if (interaction_variable){
      variable <- paste(paste(unlist(strsplit(variable, ":")), sep="", collapse=" + "), variable, sep=" + ")
    }
    
    # Run the same fit but with only one variable
    fit_only1 <- update(fit, paste(".~", variable))
    if ("boot.coef" %in% names(fit)){
      # Redo bootstrap if this was a bootstrapped
      # rms model by rerunning the bootcov part
      fit_only1 <- bootcov(fit_only1, B=fit$B, coef.reps="boot.Coef" %in% names(fit))
    }
    
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
    if ("ols" %in% class(fit)){
      # The simple .~1 doesn't work for me with the ols and I've
      # therefore created this workaround
      if ("boot.coef" %in% names(fit)){
        if ("y" %nin% names(fit)){
          warning("If you want the intercept on a bootstrapped ols() model then you need to specify y=TRUE or else you get NA in the unadjusted estimates")
          new_vars <- rep(NA, times=3)          
        }else{
          require("boot") || stop("`boot' package not found - it's needed for the unadjusted estimate of the intercept")
          ci <- boot.ci(boot(fit$y, function(x, i) mean(x[i], na.rm=TRUE), R=fit$B, stype="i"), 
            type=ifelse("boot.Coef" %in% names(fit), "perc", "norm"))
          new_vars <- c(mean(fit$y, na.rm=TRUE), ci$percent[4:5])
        }
      }else{
        new_vars <- c(mean(fit$y, na.rm=TRUE),
          mean(fit$y) + c(-1, 1)*sd(fit$y)/sqrt(sum(is.na(fit$y)==FALSE))*qnorm(.05/2))
      }

    }else{
      # Run the same fit but with only one variable
      fit_only1 <- update(fit, ".~ 1")
      if ("boot.coef" %in% names(fit)){
        # Redo bootstrap if this was a bootstrapped
        # rms model by rerunning the bootcov part
        fit_only1 <- bootcov(fit_only1, B=fit$B, coef.reps="boot.Coef" %in% names(fit))
      }
      
      # Get the coefficients processed with some advanced
      # round part()
      new_vars <- get_coef_and_ci(fit_only1, skip_intercept = FALSE)
    }
    
    unadjusted <- rbind(new_vars, unadjusted)
    rownames(unadjusted)[1] <- "Intercept"
  }
  
  # If just one variable it's not a proper matrix
  if (is.null(dim(adjusted))){
    both <- matrix(c(unadjusted, adjusted), nrow=1)
  }else{
    both <- cbind(unadjusted, adjusted)
  }
  colnames(both) <- c("Crude", "2.5 %", "97.5 %", "Adjusted", "2.5 %", "97.5 %")
  return(both)
}
