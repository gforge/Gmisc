#' @author max
#' @rdname getCrudeAndAdjustedModelData
#' @method getCrudeAndAdjustedModelData rms
#' @S3method getCrudeAndAdjustedModelData rms
getCrudeAndAdjustedModelData.rms <- function(fit, level=.95, remove_interaction_vars = TRUE, ...){
  
  # The skip intercept is not an option as the summary doesn't include the intercept
  # for the rms regression outputs
  get_coef_and_ci <- function(fit, vn, data){
    # We need to standardize the summary call so that it
    # uses the first level as reference since this is not
    # always the case for the summary function
    # Furthermore, we need to set continuous variables to
    # 0 and 1 as this is not certain to be the case
    scall <- list(object=fit,
      vnames="names",
      conf.int=level,
      est.all=FALSE)
    # Select reference for each summary call
    for(name in vn){
      if (is.factor(data[[name]])){
        scall[[name]] = levels(data[[name]])[1]
      }else if(is.logical(data[[name]])){
        # Logical should always have false as the
        # reference category
        scall[[name]] = FALSE
      }else if (length(unique(data[[name]])) == 2){
        freq <- table(data[[name]])
        scall[[name]] = sort(names(freq))[1]
      }else{
        # Perhaps a little overkill but it seems better to
        # set it to one step inside the range than just 0 vs 1
        scall[[name]] = c(median(data[[name]], na.rm=TRUE), 
          median(data[[name]], na.rm=TRUE) + 1)
      }
    }
    # Call the antilog if this is a binomial logit
    if (!is.null(fit$family$link) &&
      fit$family$link == "logit")
      scall["antilog"] = TRUE
    
    s <- suppressMessages(do.call(summary, scall))
    
    # Use the hazard ratios, the odds ratios or the antilog if
    # the function needs to be exponentiated
    r_no <- grepl("^ ((Hazard|Odds) Ratio|Antilog)", rownames(s))
    if (any(r_no)){
      rn <- rownames(s)[which(r_no)-1]
      s <- s[r_no, , drop=FALSE]
      rownames(s) <- rn
    }

    # Clean out the variable name for factors
    # rownames(s) <- gsub("^[^-]+- ([^:]+).+", "\\1", rownames(s))
    
    # Select the columns of interest
    ret_val <- s[, grep("(Effect|Lower|Upper)", colnames(s)), drop=FALSE]
    
    colnames(ret_val) <- c("", 
      sprintf("%.1f%%", 100*(1-level)/2),
      sprintf("%.1f%%", 100*(level + (1-level)/2)))
    return(ret_val)
  }
  

  var_names <- prGetModelVariables(fit, remove_interaction_vars = remove_interaction_vars)
  if (length(var_names) == 0)
    stop("You have no variables that can be displayed as adjusted/unadjusted",
      " since they all are part of an interaction, spline or strata.")

  df <- prGetModelData(fit, check_subset = TRUE)
  
  # Get the adjusted variables
  adjusted <- get_coef_and_ci(fit, vn=var_names, data=df)
  
  unadjusted <- c()
  
  for(variable in var_names){
    
    # Run the same fit but with only one variable
    fit_only1 <- update(fit, paste(".~", variable))
    if ("boot.coef" %in% names(fit)){
      # Redo bootstrap if this was a bootstrapped
      # rms model by rerunning the bootcov part
      fit_only1 <- bootcov(fit_only1, B=fit$B, coef.reps="boot.Coef" %in% names(fit))
    }else if (!is.null(attr(fit, "robust"))){
      fit_only1 <- robcov_alt(fit_only1, type=attr(fit, "robust"))
    }else if (!is.null(fit$orig.var)){
      warning("Using the regular robcov function instead of the robcov_alt is not called for.",
        " If you get this message and you haven't used robcov then you may have a bug.", 
        " Try to set the fit$org.var <- NULL and see if it works better.")
      fit_only1 <- robcov(fit_only1)
    }
    
    # Get the coefficients processed with some advanced
    # round part()
    new_vars <- get_coef_and_ci(fit_only1, vn=variable, data=df)
    
    # Add them to the previous
    unadjusted <- rbind(unadjusted, new_vars)
  }
  
  # Fix if ordering got mixed up because the summary call
  if (any(rownames(adjusted) != rownames(unadjusted))){
    ua_order <- c()
    a_order <- c()
    for(variable in var_names){
      ua_order <- c(ua_order, 
                    grep(variable, rownames(unadjusted), fixed=TRUE))
      a_order <- c(a_order, 
                   grep(variable, rownames(adjusted), fixed=TRUE))
    }
    if (length(ua_order) != length(a_order)){
      stop ("An error happend when fetching the",
            "adjusted and unadjusted variables")
    }

    # Now reorder the matrices so they match
    unadjusted <- unadjusted[ua_order,]
    adjusted <- adjusted[a_order,]
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