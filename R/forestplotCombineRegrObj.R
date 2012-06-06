#' Compares different scores in different regression objects.
#' 
#' @param regr.obj A list with all the fits that have variables that are to 
#'   be identified through the regular expression 
#' @param variablesOfInterest.regexp A regular expression identifying the variables
#'   that are of interest of comparing. For instance it can be "(score|index|measure)"
#'   that finds scores in different models that should be compared.
#' @param plot_title The title of the plot. 
#' @param clip If the plot confidence bands should be clipped
#' @param col.zero The color of the vertical zero-effect line
#' @param reference.names Additional reference names to be added to each model
#' @param rowname.fn A function that takes a rowname and sees if it needs
#'   beautifying. The function has only one parameter the coefficients name and should
#'   return a string or expression.  
#' @param box.default.size Override the default box size based on precision
#' @param xlab The label of the x-axis
#' @param xlog If TRUE, x-axis tick marks are exponentiated
#' @param estimate.txt The text of the estimate, usually HR for hazard ratio, OR for 
#'  odds ratio
#' @param zero Indicates what is zero effect. For survival/logistic fits the zero is
#'   1 while in most other cases it's 0.
#' @param exp Report in exponential form. Default true since the function was built for 
#'   use with survival models.
#' @param ... Passed to forestplot2 
#' @return void 
#' 
#' @example examples/forestplotCombineRegrObj_example.R
#' 
#' @author max
#' @export
forestplotCombineRegrObj <- function( 
  regr.obj,
  variablesOfInterest.regexp,
  plot_title       = NULL,
  clip             = c(-Inf, Inf), 
  col.zero         = "lightgray",
  reference.names  = NULL,
  rowname.fn       = NULL,
  box.default.size = .5,
  xlab             = NULL,
  xlog             = FALSE,
  estimate.txt     = NULL,
  zero             = NULL,
  exp              = TRUE,
  ...)
{   
  if (length(regr.obj) < 2)
    stop("This function combines several fits so please provide more than one model_fits")
  
  if (is.object(regr.obj) == TRUE)
    stop("The model_fits need to be a list of fits")
  
  # Initiate some standard values if the user
  # hasn't supplied any
  if (is.null(xlab) ||
    is.null(estimate.txt)){
    if (isFitCoxPH(regr.obj[[1]])){
      if (is.null(xlab))
        xlab = "Hazard Ratio"
      
      if (is.null(estimate.txt))
        estimate.txt = "HR"
    }else if(isFitLogit(regr.obj[[1]])){
      if (is.null(xlab))
        xlab = "Odds Ratio"
      
      if (is.null(estimate.txt))
        estimate.txt = "OR"
    }
  }
  
  if (is.null(zero)){
    if (isFitCoxPH(regr.obj[[1]]) ||
      isFitLogit(regr.obj[[1]]))
      zero = 1
    else
      zero = 0
  }
  
  models_fit_fp_data = list()
  for(i in 1:length(regr.obj)){
    bound <- prGetFpDataFromFit(regr.obj[[i]],
      conf.int = 0.95,
      exp = exp)
    models_fit_fp_data <- append(models_fit_fp_data, list(bound))
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
  }
  
  t.clr <- meta.colors(box="royalblue",
    lines="darkblue", 
    summary="royalblue", 
    zero=col.zero)
  
  rn <- list()
  t.coef <- c(NA)
  t.low <- c(NA)
  t.high <- c(NA)
  
  for(i in 1:length(models_fit_fp_data)){
    if (length(rn) > 0){
      rn <- append(rn, NA)
      t.coef <- append(t.coef, NA)
      t.low <- append(t.low, NA)
      t.high <- append(t.high, NA)
    }
    if (length(reference.names) > 1 || is.null(reference.names) == FALSE){
      rn <- append(rn, reference.names[i])
      t.coef <- append(t.coef, NA)
      t.low <- append(t.low, NA)
      t.high <- append(t.high, NA)
    }
    raw_row_names <- as.list(rownames(models_fit_fp_data[[i]]))
    if (is.function(rowname.fn)){
      for(c in 1:length(raw_row_names))
        raw_row_names[[c]] <- rowname.fn(raw_row_names[[c]])
    }
    rn <- append(rn, raw_row_names)
    t.coef <- append(t.coef, models_fit_fp_data[[i]][, "beta"])
    t.low <- append(t.low, models_fit_fp_data[[i]][, "low"])
    t.high <- append(t.high, models_fit_fp_data[[i]][, "high"])
  }
  
  xticks <- getXTicks(low = t.low, 
    high = t.high, 
    clip = clip, 
    exp = exp)
  
  rn <- append("Variable", rn)
  
  col2 <- list(estimate.txt)
  # TODO: should probably use options("digits") but
  # it defaults to 7 why this might be more annoying
  # than helpful
  for (i in 2:length(t.coef))
    col2 <- append(col2, ifelse(is.na(t.coef[i]), "", sprintf(" %.2f ", t.coef[i])))
  
  rn <- list(
    rn,
    col2)
  
  b_size <- rep(box.default.size, length(t.coef))
  is.summary <- c(TRUE, rep(FALSE, length(t.coef)-1))
  
  forestplot2(rn, 
    xlim       = c(0,10),
    mean       = t.coef, 
    lower      = t.low, 
    upper      = t.high,
    col        = t.clr,
    clip       = clip,
    boxsize    = b_size,
    xticks     = xticks,
    is.summary = is.summary,
    zero       = zero,
    xlab       = xlab,
    xlog       = xlog,
    ...)
  
  if (is.null(plot_title) == FALSE)
    title(main=plot_title)
}