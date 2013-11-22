# This file contains all the helper funcitons that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################

#' Get model outcome
#' 
#' Uses the model to extract the outcome variable. Throws
#' error if unable to find the outcome.
#' 
#' @param model The fitted model.1
#' @param check_subset Check if the model has been subsetted and 
#'  if so subset the outcome variable too.
#' @return vector
#' 
#' @author max
prExtractOutcomeFromModel <- function(model, check_subset = TRUE){
  outcome_formula <- formula(model)[[2]]
  outcome <- NULL
  
  if (length(all.vars(outcome_formula)) != 1){
    # We can probably figure this one out if we have
    # the Surv() call to work with
    if (grepl("^Surv", outcome_formula)[1]){
      ls <- as.list(outcome_formula)
      if ("event" %in% names(ls))
        outcome_var_name <- ls$event
      else if(sum("" == names(ls)) >= 4)
        outcome_var_name <- ls[[which("" ==  names(ls))[4]]]
      else if(sum("" == names(ls)) <= 3)
        outcome_var_name <- ls[[tail(which("" ==  names(ls)), 1)]]
      else
        stop("Could not figure out in your Surv() call what parameter to pick for the descriptive column")
    }else{
      stop(paste("You seem to have some kind of complex outcome:", 
          as.character(outcome_formula),
          "\n In order for this function to work in a predictive way",
          "you can only have one outcome - sorry"))
    }
  }else if (length(outcome_formula) > 1){
    # A complex outcome formula
    if (is.null(model$call$data))
      stop("You need to use the regression model together with the data= option",
        " when you have complex outcomes that consist out of more than one thing or",
        " the software will get confused on how to extract that information.")
    ds <- eval(model$call$data)
    outcome <- with(ds, eval(outcome_formula))
  }else{
    outcome_var_name <- all.vars(outcome_formula)[1]
  }
  
  if (length(outcome) == 0){
    if (is.null(model$call$data)){
      outcome <- get(outcome_var_name)
    }else{
      ds <- eval(model$call$data)
      if (outcome_var_name %in% colnames(ds))
        outcome <- ds[,outcome_var_name]
      else
        outcome <- get(outcome_var_name)
    }
  }

  if  (check_subset && is.null(model$call$subset) == FALSE){
    if (is.null(model$call$data))
      stop("If you are using the subset argument then please also provide the data= argument.")
    
    ds <- eval(model$call$data)
    outcome <- outcome[with(ds, eval(model$call$subset))]
  }
  
  # I was thinking of using the following
  #     outcome <- model$y
  # but it causes issues if there are missing
  # as the predictors with missing will be included
  # while the outcome with missing are unfortunately not
  # inlcuded - hence we don't know how to match them
  
  if (is.null(outcome))
    stop(paste("Could not find outcome variable:", outcome_var_name))
  
  return(outcome)
}

#' Get model predictors
#' 
#' Uses the model to extract the dataset. Throws
#' error if unable to find the data
#' 
#' @param model The fitted model.  
#' @param check_subset Check if the model has been subsetted and 
#'  if so subset the outcome variable too.
#' @return data.frame 
#' 
#' @author max
prExtractPredictorsFromModel <- function(model, check_subset = TRUE){
  vars <- prGetModelVariables(model, remove_splines=FALSE)
  if (is.null(model$call$data)){
    # Try to create the data frame from the environment
    ds <- data.frame(get(vars[1]))
    colnames(ds) <- c(vars[1])
    if (length(vars) > 1){
      for (i in 2:length(vars)){
        ds[vars[i]] <- get(vars[i])
      }
    }
  }else{
    ds <- eval(model$call$data)
    if (any(vars %nin% colnames(ds)))
      stop(paste("Could not find all the variables in the model in the dataset specified,",
          "these were missing:",
          paste(vars[vars %nin% colnames(ds)]), collapse=", "))
    
    # The drop is needed in case we only have one variable
    ds <- ds[,vars, drop=FALSE]
  }

  # Also tried using the rms package x=TRUE functionality but it unfortunately
  # converts everything to dummy variables making it difficult for regenerating
  # the original dataset
  
  if (is.matrix(ds) & is.data.frame(ds))
    ds <- as.data.frame(ds)
  
  if (is.data.frame(ds) == FALSE)
    stop(paste("Failed to get the original data that was used for generating the model."))
  
  if (check_subset && is.null(model$call$subset) == FALSE)
    ds <- ds[with(ds, eval(model$call$subset)),]
  
  return(ds)
}

#' Get model data.frame
#' 
#' Combines the \code{\link{prExtractPredictorsFromModel}}
#' with the \code{\link{prExtractOutcomeFromModel}} to generate
#' a full model dataset
#' 
#' @param x The fitted model.  
#' @param check_subset Check if the model has been subsetted and 
#'  if so subset the outcome variable too.
#' @return data.frame 
#' 
#' TODO: Check if this cannot be replaced by model.frame()
#' @author max
prGetModelData <- function(x, check_subset = TRUE){
  data <- Gmisc:::prExtractPredictorsFromModel(x, check_subset)
  data <- cbind(data, Gmisc:::prExtractOutcomeFromModel(x, check_subset))
  colnames(data)[ncol(data)] = as.character(formula(x)[[2]])
  return(data)
}

#' Get the models variables
#'  
#' This function extract the modelled variables. Any interaction
#' terms are removed as those should already be represented by
#' the individual terms.
#'  
#' @param model A model fit
#' @param remove_splines If splines, etc. should be cleaned 
#'  from the variables as these no longer are "pure" variables
#' @param remove_interaction_vars If interaction variables are
#'  not interesting then these should be removed. Often in
#'  the case of \code{\link{printCrudeAndAdjustedModel}} it is impossible
#'  to properly show interaction variables and it's better to show
#'  these in a separate table
#' @return vector with names 
#' 
#' @importFrom stringr str_split
#' @author max
prGetModelVariables <- function(model, remove_splines = TRUE, remove_interaction_vars=FALSE){
  vars <- attr(model$terms, "term.labels")
  
  # Remove I() as these are not true variables
  # and their names can probably have lots of variants
  unwanted_vars <- grep("^I\\(.*$", vars)
  if (length(unwanted_vars) > 0){
    attr(vars, "I() removed") <- vars[unwanted_vars]
    vars <- vars[-unwanted_vars]
  }
  
  pat <- "^[[:alpha:]\\.]+[^(]+\\(.*$"
  fn_vars <- grep(pat, vars)
  if(length(fn_vars) > 0){
    if (remove_splines){
      # Remove splines and other functions
      attr(vars, "functions removed") <- vars[fn_vars]
      vars <- vars[-fn_vars]
    }else{
      # Cleane the variable names into proper names
      # the assumption here is that the real variable
      # name is the first one in the parameters
      pat <- "^[[:alpha:]\\.]+.*\\(([^,)]+).*$" 
      vars[fn_vars] <- sub(pat, "\\1", vars[fn_vars])
    }
  }

  # Remove interaction terms as these are not variables
  int_term <- "^.+:.+$"
  in_vars <- grep(int_term, vars)
  if (length(in_vars) > 0){
    if (remove_interaction_vars){
      in_vn <- unlist(str_split(vars[in_vars], ":"))
      in_vars <- unique(c(in_vars, which(vars %in% in_vn)))
    }
    attr(vars, "interactions removed") <- vars[in_vars]
    vars <- vars[-in_vars]
  }
  
  clean_vars <- unique(vars)
  attributes(clean_vars) <- attributes(vars)
  return(clean_vars)
}

#' Get statistics according to the type
#' 
#' A simple function applied by the \code{\link{getDescriptionStatsBy}}
#' for the total column. This function is also used by \code{\link{printCrudeAndAdjustedModel}}
#' in case of a basic linear regression is asked for a raw stat column
#'  
#' @param x The variable that we want the statistics for 
#' @param show_perc If this is a factor/proportion variable then we
#'  might want to show the percentages 
#' @param html If the output should be in html or LaTeX formatting
#' @param digits Number of decimal digits
#' @param numbers_first If number is to be prior to the percentage
#' @param show_missing If missing should be included 
#' @param show_all_values This is by default false as for instance if there is
#'  no missing and there is only one variable then it is most sane to only show 
#'  one option as the other one will just be a complement to the first. For instance
#'  sex - if you know gender then automatically you know the distribution of the 
#'  other sex as it's 100 \% - other \%. 
#' @param continuous_fn A function for describing continuous variables
#'  defaults to \code{\link{describeMean}} 
#' @param prop_fn A function for describing proportions, defaults to
#'  the factor function
#' @param factor_fn A function for describing factors, defaults to
#'  \code{\link{describeFactors}}
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default % if you so wish by setting this variable.
#' @return A matrix or a vector depending on the settings
#' 
#' @author max
prGetStatistics <- function(x, 
  show_perc = FALSE, 
  html = TRUE, 
  digits = 1, 
  numbers_first = TRUE, 
  show_missing = TRUE, 
  show_all_values = FALSE,
  continuous_fn = describeMean, 
  factor_fn = describeFactors,
  prop_fn = factor_fn,
  percentage_sign = percentage_sign)
{
  show_missing <- prConvertShowMissing(show_missing)
  if (is.factor(x)){
    if (length(levels(x)) == 2){
      if (show_perc)
        total_table <- prop_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE)
          total_table <- total_table[names(total_table) %in% c(levels(x)[1], "Missing")]
      }
      
    }else {
      if (show_perc)
        total_table <- factor_fn(x, 
            html=html, 
            digits=digits,
            number_first=numbers_first, 
            show_missing = show_missing,
            percentage_sign = percentage_sign)
      else{
        total_table <- table(x, useNA=show_missing)
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  }else{
    total_table <- continuous_fn(x, 
      html=html, digits=digits, 
      number_first=numbers_first, 
      show_missing = show_missing)
    
    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE)
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

#' A helper function for the description stats
#' 
#' @param x The variable of interest with the levels
#' @param default_ref The default reference, either first,
#'  the level name or a number within the levels
#' @return integer The level number of interest 
#' 
#' @author max
prGetAndValidateDefaultRef <- function(x, default_ref){
  if (default_ref == "First"){
    default_ref <- 1
  }else if (is.character(default_ref)){
    if (default_ref %in% levels(x))
      default_ref <- which(default_ref == levels(x))
    else
      stop("You have provided an invalid default reference, '", 
        default_ref, "' can not be found among: ", paste(levels(x), collapse=", "))
  }else if (!default_ref %in% 1:length(levels(x)))
    stop("You have provided an invalid default reference,",
      " it is ", default_ref, " while it should be between 1 and ", length(levels(x)),
      " as this is only used for factors.")
  
  return(default_ref)
}

#' A simple thing to keep the attributes
#' 
#' Skips the dimension as this is not intended to be copied
#' between matrices of different sizes and not rownames
#' etc between data frames
#' 
#' @param from The from object 
#' @param to The to object
#' @return object The to object  
#' 
#' @author max
prCopyAllAttribsExceptDim <- function(from, to){
  for (name in names(attributes(from))){
    # Don't overwrite attributes
    if (!name %in% names(attributes(to)))
      attr(to, name) <- attr(from, name)
  }
  return (to)
}

#' Insert a row to a matrix
#' 
#' Inserts a row and keeps the attributes \code{\link{prCopyAllAttribsExceptDim}}
#' 
#' @param m matrix.
#' @param r row number where the new row should be inserted
#' @param v optional values for the new row.
#' @param rName optional character string: the name of the new row.
#' @return a matrix with one more row than the provided matrix m.
#' 
#' @author max, Arne Henningsen
prInsertRowAndKeepAttr <- function (m, r, v = NA, rName = "") 
{
  if (!inherits(m, "matrix")) {
    stop("argument 'm' must be a matrix")
  }
  if (r == as.integer(r)) {
    r <- as.integer(r)
  }
  else {
    stop("argument 'r' must be an integer")
  }
  if (length(r) != 1) {
    stop("argument 'r' must be a scalar")
  }
  if (r < 1) {
    stop("argument 'r' must be positive")
  }
  if (r > nrow(m) + 1) {
    stop("argument 'r' must not be larger than the number of rows", 
      " of matrix 'm' plus one")
  }
  if (!is.character(rName)) {
    stop("argument 'rName' must be a character string")
  }
  if (length(rName) != 1) {
    stop("argument 'rName' must be a be a single character string")
  }
  nr <- nrow(m)
  nc <- ncol(m)
  rNames <- rownames(m)
  if (is.null(rNames) & rName != "") {
    rNames <- rep("", nr)
  }
  if (r == 1) {
    m2 <- rbind(matrix(v, ncol = nc), m)
    if (!is.null(rNames)) {
      rownames(m2) <- c(rName, rNames)
    }
  }
  else if (r == nr + 1) {
    m2 <- rbind(m, matrix(v, ncol = nc))
    if (!is.null(rNames)) {
      rownames(m2) <- c(rNames, rName)
    }
  }
  else {
    m2 <- rbind(m[1:(r - 1), , drop = FALSE], matrix(v, ncol = nc), 
      m[r:nr, , drop = FALSE])
    if (!is.null(rNames)) {
      rownames(m2) <- c(rNames[1:(r - 1)], rName, rNames[r:nr])
    }
  }
  
  return(prCopyAllAttribsExceptDim(from = m, to = m2))
}

#' Pushes viewport with margins
#' 
#' A \code{\link[grid]{grid.layout}} object is used to
#' generate the margins. A second viewport selecting the
#' mid-row/col is used to create the effect of margins
#' 
#' @param bottom The margin object, either in npc or a \code{\link[grid]{unit}} object 
#' @param left The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param top The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param right The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param name The name of the last viewport
#' @return \code{NULL} 
#' 
#' @author max
prPushMarginViewport <- function(bottom, left, top, right, name=NULL){
  if (!is.unit(bottom))
    bottom <- unit(bottom, "npc")
  
  if (!is.unit(top))
    top <- unit(top, "npc")
  
  if (!is.unit(left))
    left <- unit(left, "npc")
  
  if (!is.unit(right))
    right <- unit(right, "npc")
  
  layout_name <- NULL
  if (!is.character(name))
    layout_name <- sprintf("margin_grid_%s", name)
  
  gl <- grid.layout(nrow=3, ncol=3,
    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
    widths = unit.c(left, unit(1, "npc") - left - right, right))
  
  pushViewport(viewport(layout=gl, name=layout_name))
  pushViewport(viewport(layout.pos.row=2, layout.pos.col=2, name=name))
}

#' Adds a title to the plot
#' 
#' Adds the title and generates a new
#' main viewport below the title
#' 
#' @param title The title as accepted by \code{\link[grid]{textGrob}} 
#' @param base_cex The base cex used for the plot
#' @param cex_mult The multiplier of the base - i.e. the increase of the
#'  text size for the title as compared to the general
#' @param fontface The type of fontfacte
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value 
#' 
#' @author max
prGridPlotTitle <- function(title, 
  base_cex, cex_mult = 1.2,
  fontface = "bold",
  space_below = NULL){
  titleGrob <- textGrob(title, just="center", 
    gp=gpar(fontface = fontface,
      cex = base_cex*cex_mult))
  
  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly=TRUE)*1.5, "mm")
  if (is.null(space_below)){
    space_below <- unit(convertUnit(gh, "mm", valueOnly=TRUE)/3, "mm")
  }else if (!is.unit(space_below)){
    space_below <- unit(space_below, "npc")
  }
  
  gl <- grid.layout(nrow=3, ncol=1,
    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh))
  
  pushViewport(viewport(layout=gl, name="title_layout"))
  pushViewport(viewport(layout.pos.row=1, name="title"))
  grid.draw(titleGrob)
  upViewport()
  
  pushViewport(viewport(layout.pos.row=3, name="main"))
}