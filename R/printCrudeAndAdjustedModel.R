#' Output crude and adjusted model data
#' 
#' Prints table for a fitted object. It prints by default a latex table but can 
#' also be converted into a HTML table that should be more compatible with common
#' word processors.
#' 
#' @param model A regression model
#' @param order A vector with regular expressions for each group.
#' @param digits The number of digits to round to
#' @param max A number that specifies if any values should be abbreviated above this value,
#'   for instance a value of 1000 would give a value of $> 1000$ for a value of 1001. This gives
#'   a prettier table when you have very wide confidence intervals. 
#' @param min A number that specifies if any values should be abbreviated above this value,
#'   for instance a value of -1000 would give a value of $< -1000$ for a value of -1001. This gives
#'   a prettier table when you have very wide confidence intervals.
#' @param sprintf_ci_str A string according to \code{\link{sprintf}} to write the confidence
#'   interval where the first \%s is the lower and the second the upper. 
#' @param add_references True if it should use the dataset to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param reference_zero_effect Used with references, tells if zero effect is in exponential 
#'   form, i.e. exp(0) = 1, or in regular format, i.e. 0 = 0 (can be set to any value)
#' @param groups Only used together with regular expression for ordering and grouping. 
#'   Should be a vector with group names if you want to have groups
#'   to some of the identified order groups. If you wish to skip one just
#'   us NA for that instance.
#' @param rowname.fn A function that takes a rowname and sees if it needs
#'   beautifying. The function has only one parameter the coefficients name and should
#'   return a string or expression.  
#' @param use_labels If the rowname.fn function doesn't change the name then 
#'   the label should be used instead of the name, that is if there is a
#'   label and it isn't a factor. 
#' @param output Choose the type of output that you want returned, html, latex or raw.
#'   The raw alternative is a list with the arguments that would be sent to the latex/htmlTable
#'   functions, where x is the main content of the table.
#' @param ... Passed onto the Hmisc::\code{\link{latex}} function, \code{\link{htmlTable}}
#' @return Returns a latex formatted table
#' 
#' @import miscTools
#' 
#' @example examples/printCrudeAndAdjustedModel_example.R
#' 
#' @author max
#' @export
printCrudeAndAdjustedModel <- function(model, 
  order                 = FALSE,
  digits                = 2,
  max                   = Inf, 
  min                   = -Inf,  
  sprintf_ci_str        = "%s to %s",
  add_references        = FALSE, 
  reference_zero_effect = 1,
  groups                = NULL,
  rowname.fn            = NULL,
  use_labels            = TRUE,
  output                = c("html", "latex", "raw"),
  ...)
{
  require("miscTools") || stop("`miscTools' package not found")
  
  output <- match.arg(output)
  
  # Just to simplify the checks below
  if (length(add_references) == 1 && add_references == FALSE)
    add_references <- NULL
  
  # Convert the x that should be a model into a matrix that
  # originally was expected
  x <- getCrudeAndAdjustedModelData(fit = model)
  
  ds <- eval(model$call$data)
  
  # Set the number of digits,
  # format the confidence interval
  # and change the number of cols into
  # 4 where the upper and lower CI
  # meet in one string column
  prepareCrudeAndAdjusted <- function(x){
    is.wholenumber <-
      function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    
    # Just to make sure that it gives 1.0 and
    # not 1 if digits = 1, in cases where a
    # adding another decimal that is used
    # since everyone is so hyped about p-val < 0.05
    format_number <- function(x){
      if (length(grep("[0-9]", as.character(x))) == 0)
        return(x)
      
      # Matrix forces all values to be either char or
      # numeric and therefore we need to convert them
      # to numeric
      x <- as.numeric(x)
      
      if (is.wholenumber(x) && x > 100)
        return(as.character(x))
      
      return(sprintf(sprintf("%%0.%df", digits), x))
    }
    
    # A way to set the min/max of the confidence interval
    # according to the parameters,
    # round to appropriate number of digits and 
    # format into a string as specified in the sprintf string
    format_ci <- function(ci){
      # If there is an NA then we don't know
      # much about the data
      if (any(is.na(ci))){
        return("-")
      }
      
      upper <- max(ci)
      if (upper > max)
        upper <- sprintf(ifelse(output, "&gt; %s", "$> %s$"), format_number(max))
      else
        upper <- format_number(upper)
      
      lower <- min(ci)
      if (lower < min)
        lower <- sprintf(ifelse(output, "&gt; %s", "$ %s$"), format_number(min))
      else
        lower <- format_number(lower)
      
      return(sprintf(sprintf_ci_str, lower, upper))
    }
    
    ret <- cbind(
      tapply(x[,1], 1:NROW(x), FUN = format_number),
      apply(x[,2:3], MARGIN=1, FUN=format_ci),
      tapply(x[,4], 1:NROW(x), FUN = format_number),
      apply(x[,5:6], MARGIN=1, FUN=format_ci))
    
    colnames(ret) <- c(
      colnames(x)[1], 
      sprintf("%s to %s", colnames(x)[2], colnames(x)[3]),
      colnames(x)[4], 
      sprintf("%s to %s", colnames(x)[5], colnames(x)[6]))
    
    rownames(ret) <- rownames(x)
    
    return(ret)
  }
  
  x <- prepareCrudeAndAdjusted(x)
  
  # Get the models variables
  getModelVariables <- function(model){
    vars <- attr(model$terms, "term.labels")
    # Remove splines, and other functions
    # TODO: investigate the side effect of also removing I()
    unwanted_vars <- grep("[a-zA-Z]+[^( ]\\(", vars)
    if (length(unwanted_vars) > 0)
      vars <- vars[-unwanted_vars]
    
    return(vars)
  }
  
  # Add reference according to the model
  # this is of course for factored variables
  addReferenceFromModelData <- function(values){
    if (is.null(model[["variance.inflation.impute"]]) == FALSE)
      stop("The model seems to have been created using fit.mult.impute and unfortunately that doesn't work with the current version of this function.")
    
    if ("rms" %nin% class(model) &&
      all(class(model) == c("glm", "lm")) == FALSE &&
      length(class(model)) == 1 && class(model) %nin% c("lm", "glm"))
      stop("This is only prepared for RMS, glm and lm regressions")

    # InsertRow fails to notice that the values are a
    # matrix unless we set the class to matrix
    if (is.matrix(values))
      class(values) <- "matrix"

    vars <- getModelVariables(model)
    
    n.rgroup <- c(NROW(values))
    rgroup <- c("")
    for(vn in vars)
    {
      if (is.factor(ds[,vn])){
        # Sometimes there is a subset argument or something leading to 
        # that one of the factors isn't included and therefore I use
        # this perhaps slightly trickier identification of all the factors
        available_factors <- as.character(unique(ds[, vn][is.na(ds[, vn]) == FALSE]))
        # We need to clean from characters that might cause issues with the 
        # regular expression
        regex_clean_levels <- gsub("([()\\[\\]{}\\.])", "\\\\\\1", available_factors, perl=TRUE)
        matches <- grep(
          sprintf("^%s[=]{0,1}(%s)$", vn, 
            paste(regex_clean_levels,
              collapse="|")), 
          rownames(values))
        if (length(matches) > 0){
          # TODO: Could probably be extended to other regression models but needs testing
          used_factors <- gsub(sprintf("^%s[=]{0,1}", vn), "", rownames(values)[matches])
          
          # Fetch the reference level, would probably work just as well with a levels()[1]
          reference <- available_factors[available_factors %nin% used_factors]
          if (length(reference) > 1)
            stop(sprintf("Error occurred in looking for reference, found %d reference categories instead of expected 1, %s", 
                length(reference), 
                paste(reference, collapse=", ")))
          
          # Remove the main label as that goes into the rgroup
          rownames(values)[matches] <- used_factors
          
          # Add rgroup information
          # ... yes this got way more complicated than desired but it seems to work :-)
          lab <- ifelse (label(ds[,vn]) != "", label(ds[,vn]), vn)
          
          group_2_split <- which(cumsum(n.rgroup) >= matches[1])
          if (length(group_2_split) == 0)
            stop(sprintf("Could not find the group at match %d within %d rows", matches[1], sum(n.rgroup)))
          else
            group_2_split <- group_2_split[1]
          
          if (rgroup[group_2_split] != "")
            stop(sprintf("An error occurred when adding the group labels, the software tried to overwrite an already existing group: %s", rgroup[group_2_split]))
          
          row_prior_match <- matches[1]-1
          
          rgroup_b4 <- rgroup[1:(group_2_split-1)]
          n.rgroup_b4 <- n.rgroup[1:(group_2_split-1)]
          rgroup_after <- rgroup[(group_2_split + 1):length(rgroup)]
          n.rgroup_after <- n.rgroup[(group_2_split + 1):length(n.rgroup)]
          
          if (group_2_split == 1){
            rgroup_b4 <- NULL
            n.rgroup_b4 <- NULL
          }
          # It can be both the first and the last group
          # avoid therefore an else here
          if (group_2_split == length(rgroup)){
            rgroup_after <- NULL
            n.rgroup_after <- NULL
          }
          
          rgroup <- c(rgroup_b4, 
            "", lab, "",
            rgroup_after)
          n.rgroup <- c(n.rgroup_b4,
            row_prior_match-sum(n.rgroup_b4), # The number of rows prior to our variable
            length(used_factors) + 1, # The number of factors + the one were adding
            n.rgroup[group_2_split] - 
              (row_prior_match-sum(n.rgroup_b4)) - 
              length(used_factors),  # The remaining rows from that group, if 0 then removed below
            n.rgroup_after)
          
          
          # Remove empty group
          if (any(n.rgroup == 0)){
            rgroup <- rgroup[-which(n.rgroup == 0)]
            n.rgroup <- n.rgroup[-which(n.rgroup == 0)]
          }
          
          if (any(n.rgroup < 0))
            stop(sprintf("Sorry, the software created an invalid group length of less than 0, this occurred when adding the variable: %s (%s)", vn, lab))
          
          values <- insertRow(values, 
            matches[1], 
            rep(c(reference_zero_effect, "ref"), times=2),  
            rName=reference)
        }
               
      }
    }
    
    return(list("values" = values, "rgroup" = rgroup, "n.rgroup" = n.rgroup))
  }
  
  rgroup <- NULL
  n.rgroup <- NULL
  if (length(order) > 1){
    greps <- c()
    for (r_expr in order) {
      # Find the names that matches
      matches <- grep(r_expr, rownames(x))
      # Avoid reselecting
      new_vars <- setdiff(matches, unlist(greps))
      if (length(new_vars) > 0){
        greps <- append(greps, list(new_vars))
      }
    }
    
    reorderd_groups <- x[unlist(greps), ]
    if (any(rownames(reorderd_groups) %nin% rownames(x))){
      stop(
        sprintf("An error occurred when reordering, there are now more variables than initially found, the following new vars exist: %s",
          paste(rownames(reorderd_groups)[rownames(reorderd_groups) %nin% rownames(x)], collapse=", ")))
    }else if (any(rownames(x) %nin% rownames(reorderd_groups))){
      warning(
        sprintf("Not all variables selected from the model when re-ordering, these were not included: %s",
          paste(rownames(x)[rownames(x) %nin% rownames(reorderd_groups)], collapse=", ")))
    }
    
    if (length(add_references) == 1 && 
      add_references == TRUE){
      ret <- addReferenceFromModelData(reorderd_groups)
      reorderd_groups <- ret$values
      n.rgroup <- ret$n.rgroup
      rgroup <- ret$rgroup
    }else if (length(add_references) == length(greps)){
      line_row <- 1
      for(i in 1:length(greps)){
        # Add reference if it's not empty
        if (length(add_references) > 1 &&
          is.na(add_references[i]) == FALSE){
          reorderd_groups<- insertRow(reorderd_groups, 
            line_row, 
            rep(c(reference_zero_effect, "ref"), times=2),  
            rName=add_references[i])
          # Dosn't really matter the order since it checks the length
          greps[[i]] <- append(min(greps[[i]])-1, greps[[i]])
        }
        
        # Move to next position
        line_row <- line_row + length(greps[[i]])
      }
      
      # Add row groups according to the ordering
      if (length(groups) == length(greps) && 
        length(greps) > 0){
        rgroup <- groups[is.na(groups) == FALSE]
        n.rgroup <- c()
        l <- 0
        for(i in 1:length(greps)){
          l <- l + length(greps[[i]])
          if (is.na(groups[i]) == FALSE){
            n.rgroup <- append(n.rgroup, l)
            l <- 0
          }
        }
      }
    }
  }else{
    reorderd_groups <- x
    if (length(add_references) == 1 && 
      add_references == TRUE){
      ret <- addReferenceFromModelData(reorderd_groups)
      reorderd_groups <- ret$values
      n.rgroup <- ret$n.rgroup
      rgroup <- ret$rgroup
    }
    greps <- NULL
  }
  
  
  if (is.function(rowname.fn)){
    rn <- list()
    for (name in rownames(reorderd_groups)){
      new_name <- rowname.fn(name)
      if (use_labels && new_name == name && 
        name %in% colnames(ds) &&
        label(ds[,name]) != "")
        new_name <- label(ds[,name])
      rn <- append(rn, new_name)
      
    }
  }else{
    rn <- c()
    for (name in rownames(reorderd_groups)){
      if (use_labels &&  
        name %in% colnames(ds) &&
        label(ds[,name]) != "")
        rn <- c(rn, label(ds[,name]))
      else
        rn <- append(rn, name)
     }
    
  }
  
  coef_name <- ifelse("coxph" %in% class(model), 
    "HR", 
    ifelse("lrm" %in% class(model) |
          ("glm" %in% class(model) &&
          model$family$family == "binomial"),
      "OR",
      "Coef"))
  if (output == "html"){
    return(htmlTable(reorderd_groups, 
        headings      = sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups)), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = unlist(rn),
        n.cgroup      = c(2, 2), cgroup = c("Crude", "Adjusted"), 
        col.just      = strsplit("rcrc", "")[[1]],
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        ...))
  }else if (output == "latex"){
    return(latex(reorderd_groups, 
        colheads      = latexTranslate(sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups))), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = latexTranslate(unlist(rn)),
        n.cgroup      = c(2, 2), cgroup = c("Crude", "Adjusted"), 
        col.just      = strsplit("rcrc", "")[[1]],
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        ...))
  } else {
    return(list(x     = reorderd_groups, 
        headings      = sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups)),
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = unlist(rn),
        n.cgroup      = c(2, 2), cgroup = c("Crude", "Adjusted"), 
        col.just      = strsplit("rcrc", "")[[1]],
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup))
  }
}
