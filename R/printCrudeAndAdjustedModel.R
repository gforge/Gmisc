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
#' @param html If HTML output through the htmlTable should be used 
#'   instead of traditional latex() function
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
  html                  = FALSE,
  ...)
{
  require("miscTools") || stop("`miscTools' package not found")
  
  # Just to simplify the checks below
  if (length(add_references) && add_references == FALSE)
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
      upper <- max(ci)
      if (upper > max)
        upper <- sprintf(ifelse(html, "&gt; %s", "$> %s$"), format_number(max))
      else
        upper <- format_number(upper)
      
      lower <- min(ci)
      if (lower < min)
        lower <- sprintf(ifelse(html, "&gt; %s", "$ %s$"), format_number(min))
      else
        lower <- format_number(lower)
      
      return(sprintf(sprintf_ci_str, lower, upper))
    }
    
    ret <- cbind(
      tapply(x[,1], 1:NROW(x), FUN = format_number),
      apply(x[,2:3], MARGIN=1, FUN=format_ci),
      tapply(x[,4], 1:NROW(x), FUN = format_number),
      apply(x[,5:6], MARGIN=1, FUN=format_ci))
    
    colnames(ret) <- c(colnames(x)[1], 
      sprintf("%s to %s", colnames(x)[2], colnames(x)[3]),
      colnames(x)[3], 
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
    if ("rms" %nin% class(model) &&
      all(class(model) == c("glm", "lm")) == FALSE &&
      length(class(model)) == 1 && class(model) %nin% c("lm", "glm"))
      stop("This is only prepared for RMS, glm and lm regressions")

    # InsertRow fails to notice that the values are a
    # matrix unless we set the class to matrix
    if (is.matrix(values))
      class(values) <- "matrix"

    vars <- getModelVariables(model)
    
    n.rgroup <- c()
    rgroup <- c()
    for(vn in vars)
    {
      matches <- grep(sprintf("^%s[=]{0,1}(%s)$", vn, paste(levels(ds[, vn]), collapse="|")), rownames(values))
      if (length(matches) > 0 && is.factor(ds[,vn])){
        # TODO: Could probably be extended to other regression models but needs testing
        used_factors <- gsub(sprintf("^%s[=]{0,1}", vn), "", rownames(values)[matches])
        
        # Fetch the reference level, would probably work just as well with a levels()[1]
        reference <- levels(ds[,vn])[levels(ds[,vn]) %nin% used_factors]
        if (length(reference) > 1)
          stop(sprintf("Error occurred in looking for reference, found %d reference categories instead of expected 1", length(reference)))
        
        # Remove the main label as that goes into the rgroup
        rownames(values)[matches] <- used_factors
        
        # Add rgroup information
        lab <- ifelse (label(ds[,vn]) != "", label(ds[,vn]), vn)
        if (length(n.rgroup) > 0 && any(cumsum(n.rgroup) > matches[1])){
          rgroup <- c(rgroup[cumsum(n.rgroup) < matches[1]], lab, rgroup[cumsum(n.rgroup) >= matches[1]])
          
          # Remove the rows from the next group to remain constant number of groups
          more <- cumsum(n.rgroup) >= matches[1]
          less <- cumsum(n.rgroup) < matches[1]
          n.rgroup[more][1] <- n.rgroup[more][1] - length(used_factors)  
          n.rgroup <- c(n.rgroup[less], length(used_factors) + 1, n.rgroup[more])
          
          # Remove empty group
          if (any(n.rgroup == 0)){
            rgroup <- rgroup[-which(n.rgroup == 0)]
            n.rgroup <- n.rgroup[-which(n.rgroup == 0)]
          }
        }else{
          if (matches[1] == 1){
            rgroup <- c(lab, "")
            n.rgroup <- c(length(used_factors) + 1, NROW(values) - length(used_factors))
          }else{
            rgroup <- c("", lab)
            n.rgroup <- c(matches[1] - 1, length(used_factors) + 1)
            if (NROW(values) - sum(n.rgroup) + 1 > 0){
              rgroup <- append(rgroup, "")
              n.rgroup <- append(n.rgroup, NROW(values) - sum(n.rgroup) + 1)
            }
          }
        }
        
        values <- insertRow(values, 
            matches[1], 
            rep(c(reference_zero_effect, "ref"), times=2),  
            rName=reference)
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
    if (length(reorderd_groups) > length(x)){
      stop(sprintf("An error occurred when reordering, there are now more variables than initially found, %d > %d", length(reorderd_groups), length(x)))
    }else if (length(reorderd_groups) < length(x)){
      warning(sprintf("Not all variables selected from the model when re-ordering, %d < %d", length(reorderd_groups), length(x)))
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
  
  if (html){
    return(htmlTable(reorderd_groups, 
        headings      = sub("(Crude|Adjusted)", "Coef", colnames(reorderd_groups)), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = unlist(rn),
        n.cgroup      = c(2, 2), cgroup = c("Crude", "Adjusted"), 
        col.just      = strsplit("rcrc", "")[[1]],
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        ...))
  }else{
    return(latex(reorderd_groups, 
        colheads      = latexTranslate(sub("(Crude|Adjusted)", "Coef", colnames(reorderd_groups))), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = latexTranslate(unlist(rn)),
        n.cgroup      = c(2, 2), cgroup = c("Crude", "Adjusted"), 
        col.just      = strsplit("rcrc", "")[[1]],
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        ...))
  }
}
