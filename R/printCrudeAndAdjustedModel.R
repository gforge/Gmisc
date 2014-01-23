#' Output crude and adjusted model data
#' 
#' Prints table for a fitted object. It prints by default a latex table but can 
#' also be converted into a HTML table that should be more compatible with common
#' word processors.
#' 
#' A word of warning: if you call this function and you've changed any of the variables
#' used in the original call, i.e. the premises are changed, this function will not
#' remember the original values and the statistics will be faulty! 
#' 
#' @param model A regression model
#' @param order A vector with regular expressions for each group.
#' @param digits The number of digits to round to
#' @param ci_max A number that specifies if any values should be abbreviated above this value,
#'   for instance a value of 1000 would give a value of \deqn{> -1000}{> -1000} for a value of 1001. This gives
#'   a prettier table when you have very wide confidence intervals. 
#' @param ci_min A number that specifies if any values should be abbreviated above this value,
#'   for instance a value of -1000 would give a value of \deqn{< -1000}{< -1000} for a value of -1001. This gives
#'   a prettier table when you have very wide confidence intervals.
#' @param sprintf_ci_str A string according to \code{\link{sprintf}} to write the confidence
#'   interval where the first \%s is the lower and the second the upper. 
#' @param add_references True if it should use the data set to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param add_references_pos The position where a reference should be added. Sometimes
#'   you don't want the reference to be at the top, for instance if you have age groups
#'   then you may have < 25, 25-39, 40-55, > 55 and you have the reference to be 25-39 then
#'   you should set the reference list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param reference_zero_effect Used with references, tells if zero effect is in exponential 
#'   form, i.e. \code{exp(0) = 1}, or in regular format, i.e. \code{0 = 0} (can be set to any value)
#' @param groups Only used together with regular expression for ordering and grouping. 
#'   Should be a vector with group names if you want to have groups
#'   to some of the identified order groups. If you wish to skip one just
#'   us NA for that instance.
#' @param rowname.fn A function that takes a row name and sees if it needs
#'   beautifying. The function has only one parameter the coefficients name and should
#'   return a string or expression.  
#' @param use_labels If the rowname.fn function doesn't change the name then 
#'   the label should be used instead of the name, that is if there is a
#'   label and it isn't a factor. 
#' @param desc_column Add descriptive column to the crude and adjusted table
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_numb_first Whether to show the number before the percentages
#' @param desc_continuous_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. Defaults
#'  to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'  Total and Event.
#' @param output Set to latex if you want latex output
#' @param ... Passed onto the Hmisc::\code{\link{latex}} function, or to the \code{\link{htmlTable}} via the print call
#' @return \code{matrix} Returns a matrix of class printCrudeAndAdjusted that has a default
#'  print method associated with
#' 
#' @importFrom Hmisc latex
#' 
#' @example examples/printCrudeAndAdjustedModel_example.R
#' 
#' @rdname printCrudeAndAdjustedModel
#' @author max
#' @export
printCrudeAndAdjustedModel <- function(model, 
  order                 = FALSE,
  digits                = 2,
  ci_max                = Inf, 
  ci_min                = -Inf,  
  sprintf_ci_str        = "%s to %s",
  add_references        = FALSE, 
  add_references_pos    = list(),
  reference_zero_effect = NULL,
  groups                = NULL,
  rowname.fn            = NULL,
  use_labels            = TRUE,
  desc_column           = FALSE,
  desc_show_tot_perc    = FALSE,
  desc_numb_first       = TRUE,
  desc_continuous_fn    = describeMean,
  desc_prop_fn          = describeProp,
  desc_factor_fn        = describeFactors,
  desc_show_missing     = FALSE,
  desc_digits           = digits,
  desc_colnames         = c("Total", "Event"),
  output                = "html",
  ...)
{
  if (length(reference_zero_effect) == 0)
    reference_zero_effect <- ifelse(all("lm" %in% class(model)) ||
        "ols" %in% class(model) ||
        (inherits(model, "glm") && model$family$link == "identity"), 0, 1)
  
  # Just to simplify the checks below
  if (length(add_references) == 1 && add_references == FALSE)
    add_references <- NULL
  
   
  # Convert the x that should be a model into a matrix that
  # originally was expected
  x <- getCrudeAndAdjustedModelData(fit = model)
  
  ds <- prExtractPredictorsFromModel(model)
  
  x <- prCaPrepareCrudeAndAdjusted(x = x, 
    output = output,
    ci_max = ci_max,
    ci_min = ci_min,
    digits = digits,
    sprintf_ci_str = sprintf_ci_str)
  
  if (length(order) > 1 || is.character(order)){
    greps <- prCaGetOrderVariables(names = rownames(x), order = order)
    
    reordered_groups <- x[unlist(greps), ,drop=FALSE]
    if (any(rownames(reordered_groups) %nin% rownames(x))){
      stop(
        sprintf("An error occurred when reordering, there are now more variables than initially found, the following new vars exist: %s",
          paste(rownames(reordered_groups)[rownames(reordered_groups) %nin% rownames(x)], collapse=", ")))
    }else if (any(rownames(x) %nin% rownames(reordered_groups))){
      warning(
        sprintf("Not all variables selected from the model when re-ordering, these were not included: %s",
          paste(rownames(x)[rownames(x) %nin% rownames(reordered_groups)], collapse=", ")))
    }
    
    if (length(add_references) == 1 && 
      add_references == TRUE){
      reordered_groups <- prCaAddReferenceAndStatsFromModelData(model = model,
        order = order, 
        add_references = add_references,
        add_references_pos = add_references_pos,
        reference_zero_effect = reference_zero_effect, 
        values = reordered_groups, 
        ds = ds,
        output = output,
        desc_column = desc_column, 
        desc_show_tot_perc = desc_show_tot_perc,
        desc_numb_first = desc_numb_first,
        desc_continuous_fn = desc_continuous_fn, 
        desc_prop_fn = desc_prop_fn,
        desc_factor_fn = desc_factor_fn, 
        desc_show_missing = desc_show_missing,
        desc_digits = desc_digits,
        desc_colnames = desc_colnames,
        use_labels = use_labels)
      if (length(groups) > 0){
        if (length(groups) == length(attr(reordered_groups, "rgroup"))){
          attr(reordered_groups, "rgroup") <- groups
        }else{
          warning("You have wanted to use groups but the number of rgroups identified ",
              " by the automatic add_reference (", length(attr(reordered_groups, "rgroup")), " rgroups)",
              " is not equal the number of groups provided by you (", length(groups), ").",
              "\n You have provided the groups: ", paste(groups, collapse=", "), 
              "\n and the rgroups are: ", paste(attr(reordered_groups, "rgroup"), collapse=", "))
        }
      }
    }else if (length(add_references) == length(greps)){
      if (desc_column)
        warning("The descriptive column works so far only when used with automated references")
      
      line_row <- 1
      for(i in 1:length(greps)){
        # Add reference if it's not empty
        if (length(add_references) > 1 &&
          is.na(add_references[i]) == FALSE){
          within_pos <- ifelse(add_references[i] %in% add_references_pos, 
            add_references_pos[add_references[i]], 0)
          reordered_groups <- prInsertRowAndKeepAttr(reordered_groups, 
              line_row + within_pos, 
              rep(c(reference_zero_effect, "ref"), times=2),  
              rName=add_references[i])
          # Dosn't really matter the order since it checks the length
          greps[[i]] <- append(min(greps[[i]])-1, greps[[i]])
        }
        
        # Move to next position
        line_row <- line_row + length(greps[[i]])
      }
      
      # Add row groups according to the ordering
      if (length(groups) > 0){
          if (length(groups) == length(greps)){
            attr(reordered_groups, "rgroup") <- groups
            attr(reordered_groups, "rgroup")[is.na(groups)] <- ""
            attr(reordered_groups, "n.rgroup") <- c()
            for(i in 1:length(greps)){
              attr(reordered_groups, "n.rgroup") <- append(attr(reordered_groups, "n.rgroup"), 
                length(greps[[i]]))
            }
          }else{
            warning("You have wanted to use groups but the number of hits",
              " by the order regular expression argument (", length(greps), " hits)",
              " is not equal the number of groups (", length(groups), ").",
              " You have provided the groups: '", paste(groups, collapse="', '"), "'.")
          }
      }else{
        warning("If you have no groups parameter set then your factored variables", 
          " in the table wont be grouped under their variable name.")
      }
    }
  }else{
    reordered_groups <- x
    if (length(add_references) == 1 && 
      add_references == TRUE){
      reordered_groups <- prCaAddReferenceAndStatsFromModelData(model = model,
        order = order, 
        add_references = add_references,
        add_references_pos = add_references_pos,
        reference_zero_effect = reference_zero_effect, 
        values = reordered_groups, 
        ds = ds,
        output = output,
        desc_column = desc_column, 
        desc_show_tot_perc = desc_show_tot_perc,
        desc_numb_first = desc_numb_first,
        desc_continuous_fn = desc_continuous_fn, 
        desc_prop_fn = desc_prop_fn,
        desc_factor_fn = desc_factor_fn, 
        desc_show_missing = desc_show_missing,
        desc_digits = desc_digits,
        desc_colnames = desc_colnames,
        use_labels = use_labels)
    }else{
      reordered_groups <- prCaAddFactorRgroups(value_mtrx = reordered_groups,
        dataset = ds, 
        use_labels = use_labels)
    }
    greps <- NULL
  }
  
  
  if (is.function(rowname.fn)){
    rn <- list()
    for (name in rownames(reordered_groups)){
      new_name <- rowname.fn(name)
      if (new_name == name)
        new_name <- prCaGetRowname(vn = name, use_labels = use_labels, dataset = ds)
      rn <- append(rn, new_name)
    }
  }else{
    rn <- c()
    for (name in rownames(reordered_groups)){
      rn <- append(rn, prCaGetRowname(vn = name, use_labels = use_labels, dataset = ds))
    }
  }
  rownames(reordered_groups) <- unlist(rn)
  
  coef_name <- ifelse("coxph" %in% class(model), 
    "HR", 
    ifelse("lrm" %in% class(model) |
          ("glm" %in% class(model) &&
          model$family$family == "binomial"),
      "OR",
      "Coef"))
  if(desc_column){
    extra_cols <- ncol(reordered_groups)-4
    attr(reordered_groups, "align") <- c(rep("r", times=extra_cols), rep(c("r", "c"), times=2))
    attr(reordered_groups, "n.cgroup") <- c(extra_cols, 2, 2)
    attr(reordered_groups, "cgroup") = c("", "Crude", "Adjusted")
  }else{
    attr(reordered_groups, "align") <- rep(c("r", "c"), times=2)
    attr(reordered_groups, "n.cgroup") <- c(2, 2)
    attr(reordered_groups, "cgroup") <- c("Crude", "Adjusted")
  }
  
  class(reordered_groups) <- c("printCrudeAndAdjusted", class(reordered_groups))
  
  if (output == "latex"){
    return(latex(reordered_groups, 
        colheads      = latexTranslate(sub("(Crude|Adjusted)", coef_name, colnames(reordered_groups))), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = latexTranslate(rownames(reordered_groups)),
        n.cgroup      = attr(reordered_groups, "n.cgroup"), cgroup = attr(reordered_groups, "cgroup"), 
        align         = attr(reordered_groups, "align"),
        rgroup        = attr(reordered_groups, "rgroup"), 
        n.rgroup      = attr(reordered_groups, "n.rgroup"), 
        ...))
  } else {
    attr(reordered_groups, "headings") <- sub("(Crude|Adjusted)", coef_name, colnames(reordered_groups))
    attr(reordered_groups, "rowlabel.just") <-  "l" 
    attr(reordered_groups, "rowlabel") <-  "Variable"
    attr(reordered_groups, "other") <- list(...)
    return(reordered_groups)
  }
}

#' @param x The output object from the printCrudeAndAdjustedModel function 
#' @param rgroupCSSstyle Css style for the rgorup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements. Passed on to \code{\link{htmlTable}}.
#' @param rgroupCSSseparator The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator). Passed on to \code{\link{htmlTable}}.
#' @rdname printCrudeAndAdjustedModel
#' @method print printCrudeAndAdjusted
#' @S3method print printCrudeAndAdjusted
print.printCrudeAndAdjusted <- function(x,
  rgroupCSSstyle        = "",
  rgroupCSSseparator    = "", ...){
  
  call_list <- list(x = x, 
    headings      = attr(x, "headings"), 
    rowlabel.just = attr(x, "rowlabel.just"), 
    rowlabel      = attr(x, "rowlabel"),
    n.cgroup      = attr(x, "n.cgroup"), 
    cgroup        = attr(x, "cgroup"), 
    align         = attr(x, "align"),
    rgroup        = attr(x, "rgroup"), 
    n.rgroup      = attr(x, "n.rgroup"), 
    rgroupCSSstyle= rgroupCSSstyle,
    rgroupCSSseparator = rgroupCSSseparator)
  
  if (length(attr(x, "other")) > 0){
    other <- attr(x, "other")
    for (option in names(other))
      if (nchar(option) > 0) call_list[option] <- other[[option]]
  }
  
  dots <- list(...)
  if (length(dots) > 0){
    for (option in names(dots))
      if (nchar(option) > 0) call_list[option] <- dots[[option]]
  }

  do.call(htmlTable, call_list)
}


#' Add reference according to the model
#' 
#' This is of course for factored variables and not in general.
#'  
#' @param model The regression model 
#' @param order The order
#' @param add_references True if it should use the dataset to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param add_references_pos The position where a reference should be added. Sometimes
#'   you don't want the reference to be at the top, for instance if you have age groups
#'   then you may have < 25, 25-39, 40-55, > 55 and you have the reference to be 25-39 then
#'   you should set the reference list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param reference_zero_effect The zero effect that the reference uses
#' @param values The values that are to be outputted
#' @param ds The dataset
#' @param output Choose the type of output that you want returned, html, latex or raw.
#'   The raw alternative is a list with the arguments that would be sent to the latex/htmlTable
#'   functions, where x is the main content of the table.
#' @param desc_column Add descriptive column to the crude and adjusted table
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_numb_first Whether to show the number before the percentages
#' @param desc_continuous_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. Defaults
#'  to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'  Total and Event.
#' @param use_labels If labels should be used for rownames
#' @return list 
#' 
#' @author max
prCaAddReferenceAndStatsFromModelData <- function(model, 
  order, 
  add_references,
  add_references_pos,
  reference_zero_effect, 
  values, 
  ds,
  output,
  desc_column, 
  desc_show_tot_perc,
  desc_numb_first,
  desc_continuous_fn, 
  desc_prop_fn,
  desc_factor_fn, 
  desc_show_missing,
  desc_digits,
  desc_colnames,
  use_labels
  ){
  if (!is.null(model[["variance.inflation.impute"]]))
    stop("The model seems to have been created using fit.mult.impute",
      " and unfortunately that doesn't work with the current version of this function.")
  
  if ("rms" %nin% class(model) &&
    all(class(model) == c("glm", "lm")) == FALSE &&
    length(class(model)) == 1 && class(model) %nin% c("lm", "glm"))
    stop("This is only prepared for RMS, glm and lm regressions")
  
  # InsertRow fails to notice that the values are a
  # matrix unless we set the class to matrix
  if (is.matrix(values))
    class(values) <- "matrix"
  
  vars <- prGetModelVariables(model, remove_splines = TRUE,
                              remove_interaction_vars = TRUE)
  if (length(order) > 1 || is.character(order)){
    greps <- prCaGetOrderVariables(names = vars, order = order, ok2skip=TRUE)
    vars <- vars[unlist(greps)]
  }
  
  attr(values, "rgroup") = c("")
  attr(values, "n.rgroup") = c(NROW(values))
  
  outcome <- NULL
  if (desc_column){
    stats <- list()
    
    # Get the original data
    outcome <- prExtractOutcomeFromModel(model)
    if (is.matrix(outcome) && 
          "coxph" %in% class(model)){
      # Get the left part of the formula
      outcome <- outcome[,"status"]
    }
  }
  
  stats <- list()
  
  for(vn in vars)
  {
    if (desc_column && !is.null(outcome)){
      stats[[vn]] <- prCaGetVnStats(model = model,
        vn = vn, 
        outcome = outcome,
        output = output,
        ds = ds,
        add_references = add_references,
        add_references_pos = add_references_pos,
        desc_digits= desc_digits, 
        desc_continuous_fn=desc_continuous_fn, 
        desc_prop_fn=desc_prop_fn,
        desc_factor_fn=desc_factor_fn, 
        desc_show_missing=desc_show_missing,
        desc_show_tot_perc=desc_show_tot_perc,
        desc_colnames=desc_colnames)
    }
    
    if (is.factor(ds[,vn])){
      # Sometimes there is a subset argument or something leading to 
      # that one of the factors isn't included and therefore I use
      # this perhaps slightly trickier identification of all the factors
      available_factors <- as.character(unique(ds[, vn][is.na(ds[, vn]) == FALSE]))
      
      matches <- which(substr(rownames(values), 1, nchar(vn)) == vn)
      if (length(matches) > 0){
        values <- prCaAddReference(vn = vn, 
          matches = matches, 
          available_factors=available_factors, 
          values = values,
          add_references_pos = add_references_pos,
          reference_zero_effect = reference_zero_effect,
          ds = ds,
          use_labels = use_labels)
      }
    }
  }
  
  if (desc_column){
    # Intiate empty matrix
    if (is.matrix(stats[[1]])){
      cols <- ncol(stats[[1]])
    }else{
      cols <- 1
    }
    desc_mtrx <- matrix("-", ncol=cols, nrow=nrow(values))
    rownames(desc_mtrx) <- rownames(values)
    
    # Should probably make sure we're always dealing
    # with a matrix but this is a quick fix for now
    # TODO: fix consistent matrix handling
    getRows <- function(x){
      ifelse(is.matrix(x), nrow(x), length(x))
    }
    getRownames <- function(x){
      if(is.matrix(x))
        rownames(x)
      else
        names(x)
    }
    getValue <- function(x, rn){
      if(is.matrix(x))
        x[rn,] 
      else
        x[rn]
    }
    for(vn in vars){
      rowname <- prCaGetRowname(vn = vn, use_labels = use_labels, dataset = ds)
      if (is.factor(ds[,vn]) || is.logical(ds[,vn])){
        # Get the row number of the first element in that group of factors
        group_nr <- which(rowname == attr(values, "rgroup"))
        if (length(group_nr) == 0)
          stop(sprintf("Couldn't find the row '%s' (org. name %s) in the value matrix: '%s'", 
              rowname, vn, paste(attr(values, "rgroup"), collapse="', '")))
        if (length(group_nr) > 1)
          stop(sprintf(paste("Too many rows matched the row '%s' (org. name %s)\n",
                "this is probably due to the fact that the name occurs twice",
                "in the value matrix: '%s'\n",
                "The most probably cause is that you have the same label() for two variables."), 
              rowname, vn, paste(attr(values, "rgroup"), collapse="', '")))
        
        if (group_nr == 1){
          row <- 1
        }else{
          row <- sum(attr(values, "n.rgroup")[1:(group_nr-1)]) + 1
        }
        
        last_row <- row+attr(values, "n.rgroup")[group_nr]-1
        existing_labels <- rownames(values)[row:last_row]
        # TODO: check how this works when no labels are checked while there are labels set for the variables
        if (any(existing_labels %nin% rownames(stats[[vn]])))
          stop(paste("A few labels from factor", vn, "weren't found in the stats:",
              paste(existing_labels[existing_labels %nin% rownames(stats[[vn]])], 
                collapse=", ")))
        
        # Add the stats to the desc 
        for(rn in existing_labels){
          # Find that row within the group
          group_rownames <- rownames(desc_mtrx[row:last_row, ,drop=FALSE])
          row_within_group <- which(rn == group_rownames)
          if (length(row_within_group) > 1)
            stop("There are more than one occurrence within group ", vn,
              "that have the value: '", rn, "'\n",
              "The rownames in that group are: '", paste(group_rownames, "', '"), "'")
          else if (length(row_within_group) == 0)
            stop("There was no match within group ", vn,
              "for the value: '", rn, "'\n",
              "The rownames in that group are: '", paste(group_rownames, "', '"), "'")
          
          # Set the value of that row
          desc_mtrx[row + row_within_group - 1, ] <- getValue(stats[[vn]], rn)
        }
        
        # There are more values in the stats than in the 
        # regression, this is probably due to missing values,
        # these will be added to the current group last
        if (getRows(stats[[vn]]) > attr(values, "n.rgroup")[group_nr]){
          rows_2_add <- getRownames(stats[[vn]])[getRownames(stats[[vn]]) %nin% existing_labels]
          for (i in 1:length(rows_2_add)){
            rn <- rows_2_add[i]
            values <- prInsertRowAndKeepAttr(values, row + attr(values, "n.rgroup")[group_nr], rep("-", length.out=cols), rn)
            desc_mtrx <- prInsertRowAndKeepAttr(desc_mtrx, row + attr(values, "n.rgroup")[group_nr], getValue(stats[[vn]], rn), rn)
          }
          attr(values, "n.rgroup")[group_nr] <- getRows(stats[[vn]])
        }
      }else{
        # This is fairly straight forward as there is only one row per
        # value and we can find that row by just looking at the row name
        row <- grep(rowname, rownames(values))
        if (length(row) == 0){
          row <- grep(vn, rownames(values))
          if (length(row) == 0)
            stop(sprintf("Couldn't find the row '%s' (org. name %s) in the value matrix", rowname, vn))
        }
        
        desc_mtrx[row, ] <- stats[[vn]]
      }
    }
    
    values <- prCopyAllAttribsExceptDim(values, cbind(desc_mtrx, values)) 
    if (ncol(desc_mtrx) == 1 && colnames(values)[1] == "")
      colnames(values)[1] <- desc_colnames[1]
    else if(all(colnames(values)[1:2] == ""))
      colnames(values)[1:2] <- desc_colnames
    
  }
  
  return(values)
}

#' Adds a reference to value matrix
#' 
#' @param vn Variable name 
#' @param matches Rows that match
#' @param available_factors Factors that are available
#' @param values The value matrix
#' @param add_references_pos The position within the factors if it exists
#' @param reference_zero_effect The reference zero effect 
#' @param ds The data set
#' @param use_labels If labels should be used for row names
#' @return \code{matrix} A matrix with rgroup and n.rgroup attributes 
#' 
#' @author max
prCaAddReference <- function(vn, matches, available_factors, values, add_references_pos, reference_zero_effect, ds, use_labels){
  ref_value <- rep(c(reference_zero_effect, "ref"), times=2)
  
  reference <- NULL
  rms_format <- FALSE
  # The rms package generates rownames with factor name:reference factor
  # and it is therefore a good idea to find the refreence by checking
  # which one is at the end
  for (f_name in available_factors){
    tmp <- gsub(f_name, "", rownames(values)[matches], fixed=TRUE)
    if (all(grepl(":$", tmp))){
      reference <- f_name
      rms_format <- TRUE
      break
    }
  }
  
  if (length(reference) == 0){
    # TODO: Could probably be extended to other regression models but needs testing
    used_factors <- gsub(sprintf("^%s[ ]{0,1}[=-]{0,1}{0,1}", vn), "", rownames(values)[matches])
    
    # Fetch the reference level, would probably work just as well with a levels()[1]
    reference <- available_factors[!available_factors %in% used_factors]
    if (length(reference) != 1)
      stop("Error occurred in looking for reference, found ", length(reference), " reference categories",
        " instead of expected 1, out of these factors:\n ", paste(available_factors, collapse=", "),
        ifelse(length(reference) > 1, 
          sprintf(" \n The refrences found: %s", paste(reference, collapse=", ")),
          sprintf(" \n The rownames that have searched: %s", paste(rownames(values)[matches], collapse=", "))))
  }else{
    used_factors <- available_factors[reference != available_factors]
  }
  
  clean_rn = rownames(values)
  for (uf_name in used_factors){
    if (rms_format){
      clean_rn <- gsub("^.* - (.*):.*$", "\\1", rownames(values))
      r_no <- which(clean_rn == uf_name)
    }else{
      r_no <- grep(uf_name, clean_rn, fixed=TRUE)
    }
    if (!any(r_no %in% matches))
      stop("Could not find rowname with factor ", uf_name, 
        " among any of the row names: ", paste(clean_rn, collapse=", "))
    r_no <- r_no[r_no %in% matches]
    if (length(r_no) > 1)
      stop("Multiple rows matched the factor ", uf_name,
        " from the available: ", paste(clean_rn[matches], collapse=", "))
    
    # Remove the main label as that goes into the attr(values, "rgroup")
    rownames(values)[r_no] <- uf_name
    
  }
  
  # Add attr(values, "rgroup") information
  # ... yes this got way more complicated than desired but it seems to work :-)
  lab <- prCaGetRowname(vn = vn, use_labels = use_labels, dataset = ds)
  
  group_2_split <- which(cumsum(attr(values, "n.rgroup")) >= matches[1])
  if (length(group_2_split) == 0)
    stop(sprintf("Could not find the group at match %d within %d rows", matches[1], sum(attr(values, "n.rgroup"))))
  else
    group_2_split <- group_2_split[1]
  
  if (attr(values, "rgroup")[group_2_split] != "")
    stop(sprintf("An error occurred when adding the group labels, the software tried to overwrite an already existing group: %s", attr(values, "rgroup")[group_2_split]))
  
  row_prior_match <- matches[1]-1
  
  rgroup_b4 <- attr(values, "rgroup")[1:(group_2_split-1)]
  n.rgroup_b4 <- attr(values, "n.rgroup")[1:(group_2_split-1)]
  rgroup_after <- attr(values, "rgroup")[(group_2_split + 1):length(attr(values, "rgroup"))]
  n.rgroup_after <- attr(values, "n.rgroup")[(group_2_split + 1):length(attr(values, "n.rgroup"))]
  
  if (group_2_split == 1){
    rgroup_b4 <- NULL
    n.rgroup_b4 <- NULL
  }
  # It can be both the first and the last group
  # avoid therefore an else here
  if (group_2_split == length(attr(values, "rgroup"))){
    rgroup_after <- NULL
    n.rgroup_after <- NULL
  }
  
  attr(values, "rgroup") <- c(rgroup_b4, 
    "", lab, "",
    rgroup_after)
  attr(values, "n.rgroup") <- c(n.rgroup_b4,
    row_prior_match-sum(n.rgroup_b4), # The number of rows prior to our variable
    length(used_factors) + 1, # The number of factors + the one were adding
    attr(values, "n.rgroup")[group_2_split] - 
      (row_prior_match-sum(n.rgroup_b4)) - 
      length(used_factors),  # The remaining rows from that group, if 0 then removed below
    n.rgroup_after)
  
  
  # Remove empty group
  if (any(attr(values, "n.rgroup") == 0)){
    attr(values, "rgroup") <- attr(values, "rgroup")[-which(attr(values, "n.rgroup") == 0)]
    attr(values, "n.rgroup") <- attr(values, "n.rgroup")[-which(attr(values, "n.rgroup") == 0)]
  }
  
  if (any(attr(values, "n.rgroup") < 0))
    stop(sprintf("Sorry, the software created an invalid group length of less than 0, this occurred when adding the variable: %s (%s)", vn, lab))
  
  offset <- ifelse(vn %in% names(add_references_pos),
    add_references_pos[[vn]] - 1,
    0)
  if (offset > length(available_factors) - 1 ||
    offset < 0){
    warning("You have a reference position that is outside the number of levels, '", offset + 1, "'",
      " is not among, 1 to ", length(available_factors),
      ". This will therefore be ignored")
    offset <- 0
  }
  values <- prInsertRowAndKeepAttr(values, 
    matches[1] + offset, 
    ref_value,  
    rName=reference)
  
  return(values)
}

#' Gets the variable stats
#' 
#' @param model The model
#' @param vn The variable name
#' @param outcome The outcome vector
#' @param output Choose the type of output that you want returned, html, latex or raw.
#'   The raw alternative is a list with the arguments that would be sent to the latex/htmlTable
#'   functions, where x is the main content of the table.
#' @param ds The dataset
#' @param add_references True if it should use the dataset to look for references, otherwise
#'   supply the function with a vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param add_references_pos The position where a reference should be added. Sometimes
#'   you don't want the reference to be at the top, for instance if you have age groups
#'   then you may have < 25, 25-39, 40-55, > 55 and you have the reference to be 25-39 then
#'   you should set the reference list for \code{age_groups} as \code{add_references_pos = list(age_groups = 2)}
#'   so that you have the second group as the position for the reference.
#' @param desc_show_tot_perc Show percentages for the total column
#' @param desc_continuous_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeMean}}
#' @param desc_prop_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeProp}}
#' @param desc_factor_fn Stat function used for the descriptive statistics, defaults
#'   to \code{\link{describeFactors}}
#' @param desc_show_missing Show missing variables in the descriptive columns
#' @param desc_digits Number of digits to use in the descriptive columns. Defaults
#'  to the general digits if not specified.
#' @param desc_colnames The names of the two descriptive columns. By default
#'  Total and Event.
#' @return \code{matrix} A matrix from \code{\link{getDescriptionStatsBy}} or
#'  \code{\link{prGetStatistics}}
#' 
#' @author max
prCaGetVnStats <- function(model,
  vn, 
  outcome, 
  output,
  ds,
  add_references, 
  add_references_pos,
  desc_digits, 
  desc_continuous_fn, 
  desc_prop_fn,
  desc_factor_fn, 
  desc_show_missing,
  desc_show_tot_perc,
  desc_colnames){
  # TODO: add some option of handling missing from the model, a second/third column
  # TODO: add handling for logical values
  
  # If there is a binomial outcome variable then 
  # it makes sense to have two columns, the overall
  # and the event data.
  if (any(class(model) %in% c("lrm", "coxph")) ||
    ("glm" %in% class(model) &&
      model$family$family == "binomial")){
    desc_mtrx <- getDescriptionStatsBy(x=ds[is.na(outcome) == FALSE,vn], 
      by=outcome[is.na(outcome) == FALSE],
      hrzl_prop = TRUE,
      digits = desc_digits,
      continuous_fn = desc_continuous_fn,
      prop_fn = desc_prop_fn,
      factor_fn = desc_factor_fn,
      show_all_values = add_references,
      show_missing = desc_show_missing,
      add_total_col = TRUE,
      total_col_show_perc = desc_show_tot_perc, 
      html = output == "html")
    
    # Don't select the no-event alternative as this is usually
    # not interesting since we have the total column
    desc_mtrx <- desc_mtrx[,c(1,3),drop=FALSE]
    colnames(desc_mtrx) <- desc_colnames
  }else{
    desc_mtrx <- prGetStatistics(x=ds[is.na(outcome) == FALSE,vn],  
      show_perc = desc_show_tot_perc, 
      html = output == "html",
      digits = desc_digits,
      continuous_fn = desc_continuous_fn,
      prop_fn = desc_prop_fn,
      factor_fn = desc_factor_fn,
      show_missing = desc_show_missing)
    
  }
  
  if (!is.matrix(desc_mtrx)){
    rn <- names(desc_mtrx)
    desc_mtrx <- matrix(desc_mtrx, ncol=1)
    if (!is.null(rn))
      rownames(desc_mtrx) <- rn
  }
  
  # As the first element in a factor is always the
  # reference then we need to move it to the wanted
  # position
  if (is.factor(ds[,vn]) && 
    vn %in% names(add_references_pos) &&
    add_references_pos[[vn]] != 1){
    
    if (nrow(desc_mtrx) == 2){
      if (add_references_pos[[vn]] == 2)
        desc_mtrx <- desc_mtrx[c(2,1), ]
    }else if (nrow(desc_mtrx) > 2){
      if (add_references_pos[[vn]] == nrow(desc_mtrx)){
        desc_mtrx <- desc_mtrx[c(2:nrow(desc_mtrx), 1), ] 
      }else{
        desc_mtrx <- desc_mtrx[c(2:add_references_pos[[vn]], 
            1,
            (add_references_pos[[vn]]+1):nrow(desc_mtrx)), ]
      }
    }
  }
  
  return(desc_mtrx)
}

#' Gets the labelled rowname if it exists
#' 
#' Looks for matches inside factors if rowname
#' contains the name of the column. Capitalizes
#' the name.
#' 
#' @param vn The variable name 
#' @param use_labels If labels should be used
#' @param dataset The dataset
#' @return \code{string} The rowname 
#' 
#' @importFrom Hmisc capitalize
#' @author max
prCaGetRowname <- function(vn, use_labels, dataset){
  vn <- as.character(vn)
  if(vn %in% colnames(dataset) &&
    use_labels && 
    label(dataset[,vn]) != ""){
    return(capitalize(label(dataset[,vn])))
  }else if (any(vn == colnames(dataset))){
    # An exact match means that there is no factor information
    # for this row and we should be able to return this row
    return(capitalize(vn))
  }
    
  # Check if this is actually a factor and return that factors name
  colno_containing_name = unlist(lapply(colnames(dataset), function(x) grepl(x, vn, fixed=TRUE)))
  if (sum(colno_containing_name) == 1){
    lvls <- levels(dataset[,colno_containing_name])
    matching_lvl <- unlist(lapply(lvls, function(x) grepl(x, vn, fixed=TRUE)))
    if (sum(matching_lvl) == 1)
      return(capitalize(lvls[matching_lvl]))
    
    warning("Could not identify the rowname '", vn, "'",
      " from the factor variable '", colnames(dataset)[colno_containing_name], "'",
      " that has the factors: '", paste(lvls, collapse="', '"), "'")
  }
  
  return(capitalize(vn))
}

#' Re-order variables
#' 
#' @param names The names of the variables 
#' @param order The order regular expression
#' @param ok2skip If you have the intercept then
#'  it should be ok for the function to skip that
#'  variable if it isn't found among the variable list
#' @return \code{vector} A vector containing the greps 
#' 
#' @author max
prCaGetOrderVariables <- function(names, order, ok2skip = FALSE){
  greps <- c()
  for (r_expr in order) {
    # Find the names that matches
    matches <- grep(r_expr, names)
    if (length(matches) == 0 & !ok2skip){
      stop("You have a strange selection order,",
           "this could be due to that you try to select a factor subvariable and not the full variable.",
           "Re-arranging factors should be done in the factor() function and not here.",
           sprintf("Anyway the expression '%s' was not found in these variable names:", r_expr),
           paste(names, collapse=", "))
    }else if (length(matches) > 0){
      # Avoid reselecting
      new_vars <- setdiff(matches, unlist(greps))
      if (length(new_vars) > 0){
        greps <- append(greps, list(new_vars))
      }
    }
  }
  return(greps)
}


#' A function for adding factor rgroups
#' 
#' @param value_mtrx A matrix containing the values of prCa
#' @param dataset The dataset that is used for the fit
#' @param use_labels Wether or not to use labels
#' @return \code{list} Returns the value matrix with the attributes rgroup and n.rgroup  
#' 
#' @author max
prCaAddFactorRgroups <- function(value_mtrx, dataset, use_labels){
  rn <- rownames(value_mtrx)
  cols <- colnames(dataset)[unlist(lapply(colnames(dataset), function(x) any(grepl(x, rn, fixed=TRUE))))]
  if (length(cols) > 0){
    rgroup <- c()
    n.rgroup <- c()
    last_single_row <- FALSE
    for (i in 1:length(rn)){
      col_match <- unlist(lapply(cols, function(x) any(grepl(x, rn[i], fixed=TRUE))))
      if (!any(col_match)){
        rgroup <- append(rgroup, "")
        n.rgroup <- append(n.rgroup, 1)
      }else{
        colname <- cols[col_match]
        if (is.numeric(dataset[,colname])){
          if (last_single_row){
            n.rgroup[length(n.rgroup)] <- n.rgroup[length(n.rgroup)] + 1 
          }else{
            last_single_row = TRUE
            rgroup <- append(rgroup, "")
            n.rgroup <- append(n.rgroup, 1)
          }
        }else{
          affected_rows <- rn[unlist(lapply(rn, function(x) any(grepl(colname, x, fixed=TRUE))))]
          # If the row name is just one then there is no need for a 
          # separate group
          if (length(affected_rows) <= 1){
            if (last_single_row){
              n.rgroup[length(n.rgroup)] <- n.rgroup[length(n.rgroup)] + 1 
            }else{
              last_single_row = TRUE
              rgroup <- append(rgroup, "")
              n.rgroup <- append(n.rgroup, 1)
            }
          }else{
            last_single_row = FALSE
            name <- prCaGetRowname(colname, use_labels = use_labels, dataset = dataset)
            if (length(rgroup) == 0 ||
              rgroup[length(rgroup)] != name){
              rgroup <- append(rgroup, name)
              n.rgroup <- append(n.rgroup, 1)
            }else{
              n.rgroup[length(n.rgroup)] <- n.rgroup[length(n.rgroup)] + 1
            }
          }
        }
      }

    }
  }
  
  attr(value_mtrx, "rgroup") <- rgroup
  attr(value_mtrx, "n.rgroup") <- n.rgroup
  return(value_mtrx)
}

#' Prettify the text
#' 
#' Sets the number of digits, formats the confidence interval and 
#' changes the number of cols into 4 where the upper and lower CI 
#' meet in one string column
#' 
#' @param x The value matrix from getCrudeAndAdjusted 
#' @param output The type of output
#' @param ci_max The max confidence interval
#' @param ci_min The min confidence interval
#' @param digits The number of decimal digits to use
#' @param sprintf_ci_str The \code{\link{sprintf}} code for the confidence interval
#' @return \code{matrix} A string matrix with the values formated
#' 
#' @author max
prCaPrepareCrudeAndAdjusted <- function(x, output, ci_max, ci_min, digits, sprintf_ci_str){
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
    if (upper > ci_max)
      upper <- sprintf(ifelse(output == "html", "&gt; %s", "$> %s$"), format_number(ci_max))
    else
      upper <- format_number(upper)
    
    lower <- min(ci)
    if (lower < ci_min)
      lower <- sprintf(ifelse(output == "html", "&gt; %s", "$ %s$"), format_number(ci_min))
    else
      lower <- format_number(lower)
    
    return(sprintf(sprintf_ci_str, lower, upper))
  }
  
  values <- cbind(
    tapply(x[,1, drop=FALSE], 1:NROW(x), FUN = format_number),
    apply(x[,2:3, drop=FALSE], MARGIN=1, FUN=format_ci),
    tapply(x[,4, drop=FALSE], 1:NROW(x), FUN = format_number),
    apply(x[,5:6, drop=FALSE], MARGIN=1, FUN=format_ci))
  
  colnames(values) <- c(
    colnames(x)[1], 
    sprintf("%s to %s", colnames(x)[2], colnames(x)[3]),
    colnames(x)[4], 
    sprintf("%s to %s", colnames(x)[5], colnames(x)[6]))
  
  rownames(values) <- rownames(x)
  
  return(values)
}