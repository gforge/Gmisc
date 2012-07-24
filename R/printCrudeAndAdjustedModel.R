#' Prints table for a fitted object. It prints by default a latex table but can 
#' also be converted into a HTML table that should be more compatible with common
#' word processors.
#' 
#' @param x An output from the getCrudeAndAdjustedModelData() function or
#'   a regression model
#' @param order A vector with regular expressions for each group.
#' @param digits The number of digits to round to
#' @param add_references A vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param zero_effect Used with references, tells if zero effect is in exponential 
#'   form, i.e. exp(0) = 1, or in regular format, i.e. 0 = 0 (can be set to any value)
#' @param groups A vector with group names if you want to have groups
#'   to some of the identified order groups. If you wish to skip one just
#'   us NA for that instance.
#' @param rowname.fn A function that takes a rowname and sees if it needs
#'   beautifying. The function has only one parameter the coefficients name and should
#'   return a string or expression.  
#' @param html If HTML output through the htmlTable should be used 
#'   instead of traditional latex() function
#' @param ... Passed onto the latex function @seealso Hmisc::latex
#' @return Returns a latex formatted table
#' 
#' @import miscTools
#' 
#' @example examples/printCrudeAndAdjustedModel_example.R
#' 
#' @author max
#' @export
printCrudeAndAdjustedModel <- function(x, 
  order,
  digits         = 2,
  add_references = NULL, 
  zero_effect    = 1,
  groups         = NULL,
  rowname.fn     = NULL,
  html           = FALSE,
  ...)
{
  require("miscTools") || stop("`miscTools' package not found")
  
  # Convert the x that should be a model into a matrix that
  # originally was expected
  if(is.matrix(x) == FALSE){
    x <- getCrudeAndAdjustedModelData(fit = x, 
      digits = digits)
  }
  
  if (length(order) == 0 || 
      is.vector(order) == FALSE || 
      is.character(order) == FALSE){
    stop("You have to give a vector with regex expressions to select for latex ordering")
  }
  
  greps <- c()
  for (expression in order) {
    # Find the names that matches
    matches <- grep(expression, rownames(x))
    # Avoid reselecting
    new_vars <- setdiff(matches, unlist(greps))
    if (length(new_vars) > 0){
      greps <- append(greps, list(new_vars))
    }
  }
  
  reorderd_groups <- x[unlist(greps), ]
  if (length(add_references) == length(greps)){
    line_row <- 1
    for(i in 1:length(greps)){
      # Add reference if it's not empty
      if (is.na(add_references[i]) == FALSE){
        reorderd_groups<- insertRow(reorderd_groups, 
          line_row, 
          rep(c(zero_effect, "ref"), times=2),  
          rName=add_references[i])
        # Dosn't really matter the order since it checks the length
        greps[[i]] <- append(min(greps[[i]])-1, greps[[i]])
      }
      
      # Move to next position
      line_row <- line_row + length(greps[[i]])
    }
  }
  
  # Add row groups according to the ordering
  rgroup <- NULL
  n.rgroup <- NULL
  if (length(groups) == length(greps)){
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
  
  if (is.function(rowname.fn)){
    rn <- list()
    for (name in rownames(reorderd_groups))
      rn <- append(rn, rowname.fn(name))
  }else{
    rn <- rownames(reorderd_groups)
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
