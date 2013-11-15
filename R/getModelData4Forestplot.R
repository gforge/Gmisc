#' Get model data
#' 
#' A helper function for \code{\link{forestplotCombineRegrObj}}
#' 
#' @param regr.obj A list with all the fits that have variables that are to 
#'   be identified through the regular expression 
#' @param variablesOfInterest.regexp A regular expression identifying the variables
#'   that are of interest of comparing. For instance it can be "(score|index|measure)"
#'   that finds scores in different models that should be compared.
#' @param exp Report in exponential form. Default true since the function was built for 
#'   use with survival models.
#' @param add_first_as_ref If you want that the first variable should be reference for 
#'   that group of variables. The ref is a variable with the estimate 1 or 0 depending
#'   if exp() and the confidence interval 0.
#' @param ref_labels If add_first_as_ref is TRUE then this vector is used for the model
#'   fits. 
#' 
#' @example examples/forestplotCombineRegrObj_example.R
#' @author max
getModelData4Forestplot<- function(regr.obj, 
  exp, 
  variablesOfInterest.regexp,
  ref_labels,
  add_first_as_ref){
  models_fit_fp_data <- list()
  for(i in 1:length(regr.obj)){
    bound <- prGetFpDataFromFit(regr.obj[[i]],
      conf.int = 0.95,
      exp = exp)
    models_fit_fp_data <- append(models_fit_fp_data, list(bound))
    if (is.null(names(regr.obj)) == FALSE &&
        names(regr.obj)[[i]] != ""){
      names(models_fit_fp_data)[[i]] <- names(regr.obj)[[i]] 
    }
  }
  
  # Find the variables that belong to the score
  # the other variables should not be sorted
  # and a blank space is to appear between to separate them
  for(i in 1:length(models_fit_fp_data)){
    frame_names <- rownames(models_fit_fp_data[[i]])
    score_variables <- frame_names %in% 
      grep(variablesOfInterest.regexp, 
        frame_names, 
        value=TRUE)
    models_fit_fp_data[[i]] <- models_fit_fp_data[[i]][score_variables, ,drop=FALSE]
    if (add_first_as_ref){
      rn <- rownames(models_fit_fp_data[[i]])
      if (exp){
        models_fit_fp_data[[i]] <- rbind(
          c(beta=1,
            p_val=NA,
            low=1,
            high=1,
            order=-1),
          models_fit_fp_data[[i]])
      }else{
        models_fit_fp_data[[i]] <- rbind(
          c(beta=0,
            p_val=NA,
            low=0,
            high=0,
            order=-1),
          models_fit_fp_data[[i]])
      }
      if (is.null(ref_labels) == FALSE)
        name <- rep(ref_labels, length=length(models_fit_fp_data))[i]
      else
        name <- "Reference"
      rownames(models_fit_fp_data[[i]]) <- append(name, rn)
    }
  }
  
  return (models_fit_fp_data)
}
