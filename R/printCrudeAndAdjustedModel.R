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
#' @param rgroupCSSstyle Css style for the rgorup, if different styles are wanted for each of the
#'  rgroups you can just specify a vector with the number of elements. Passed on to \code{\link{htmlTable}}.
#' @param rgroupCSSseparator The line between different rgroups. The line is set to the TR element
#'  of the lower rgroup, i.e. you have to set the border-top/padding-top etc to a line with
#'  the expected function. This is only used for rgroups that are printed. You can specify
#'  different separators if you give a vector of rgroup - 1 length (this is since the first
#'  rgroup doesn't have a separator). Passed on to \code{\link{htmlTable}}.
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
  reference_zero_effect = ifelse(all("lm" %in% class(model)) ||
      "ols" %in% class(model) ||
      (inherits(model, "glm") && model$family$link == "identity"), 0, 1),
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
  output                = c("html", "latex", "raw"),
  rgroupCSSstyle        = "",
  rgroupCSSseparator    = "",
  ...)
{
  require("miscTools") || stop("`miscTools' package not found")
  
  output <- match.arg(output)
  
  # Just to simplify the checks below
  if (length(add_references) == 1 && add_references == FALSE)
    add_references <- NULL
  
  getOrderVariables <- function(names){
    greps <- c()
    for (r_expr in order) {
      # Find the names that matches
      matches <- grep(r_expr, names)
      if (length(matches) == 0)
        stop("You have a strange selection order,",
          "this could be due to that you try to select a factor subvariable and not the full variable.",
          "Re-arranging factors should be done in the factor() function and not here.",
          sprintf("Anyway the expression '%s' was not found in these variable names:", r_expr),
          paste(names, collapse=", "))
      
      # Avoid reselecting
      new_vars <- setdiff(matches, unlist(greps))
      if (length(new_vars) > 0){
        greps <- append(greps, list(new_vars))
      }
    }
    return(greps)
  }
  
  # Convert the x that should be a model into a matrix that
  # originally was expected
  x <- getCrudeAndAdjustedModelData(fit = model)
  
  ds <- prExtractPredictorsFromModel(model)
  
  getRowname <- function(vn){
    ifelse (vn %in% colnames(ds) &&
        use_labels && 
        label(ds[,vn]) != "", label(ds[,vn]), vn)
  }
  
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
        upper <- sprintf(ifelse(output == "html", "&gt; %s", "$> %s$"), format_number(max))
      else
        upper <- format_number(upper)
      
      lower <- min(ci)
      if (lower < min)
        lower <- sprintf(ifelse(output == "html", "&gt; %s", "$ %s$"), format_number(min))
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
  
  # Add reference according to the model
  # this is of course for factored variables
  addReferenceAndStatsFromModelData <- function(values){
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

    vars <- prGetModelVariables(model)
    if (length(order) > 1 || is.character(order)){
      greps <- getOrderVariables(vars)
      vars <- vars[unlist(greps)]
    }
    
    ret <- list("values" = values, "rgroup" = c(""), "n.rgroup" = c(NROW(values)))
    
    if (desc_column){
      stats <- list()
      
      # Get the original data
      outcome <- prExtractOutcomeFromModel(model)
      if ("coxph" %in% class(model)){
        # Get the left part of the formula
        outcome <- outcome[,"status"]
      }
    }
    
    stats <- list()
    
    addReference <- function(vn, matches, available_factors, ret){
      ref_value <- rep(c(reference_zero_effect, "ref"), times=2)
      
      # TODO: Could probably be extended to other regression models but needs testing
      used_factors <- gsub(sprintf("^%s[=]{0,1}", vn), "", rownames(ret$values)[matches])
      
      # Fetch the reference level, would probably work just as well with a levels()[1]
      reference <- available_factors[available_factors %nin% used_factors]
      if (length(reference) > 1)
        stop(sprintf("Error occurred in looking for reference, found %d reference categories instead of expected 1, %s", 
            length(reference), 
            paste(reference, collapse=", ")))
      
      # Remove the main label as that goes into the ret$rgroup
      rownames(ret$values)[matches] <- used_factors
      
      # Add ret$rgroup information
      # ... yes this got way more complicated than desired but it seems to work :-)
      lab <- getRowname(vn)
      
      group_2_split <- which(cumsum(ret$n.rgroup) >= matches[1])
      if (length(group_2_split) == 0)
        stop(sprintf("Could not find the group at match %d within %d rows", matches[1], sum(ret$n.rgroup)))
      else
        group_2_split <- group_2_split[1]
      
      if (ret$rgroup[group_2_split] != "")
        stop(sprintf("An error occurred when adding the group labels, the software tried to overwrite an already existing group: %s", ret$rgroup[group_2_split]))
      
      row_prior_match <- matches[1]-1
      
      rgroup_b4 <- ret$rgroup[1:(group_2_split-1)]
      n.rgroup_b4 <- ret$n.rgroup[1:(group_2_split-1)]
      rgroup_after <- ret$rgroup[(group_2_split + 1):length(ret$rgroup)]
      n.rgroup_after <- ret$n.rgroup[(group_2_split + 1):length(ret$n.rgroup)]
      
      if (group_2_split == 1){
        rgroup_b4 <- NULL
        n.rgroup_b4 <- NULL
      }
      # It can be both the first and the last group
      # avoid therefore an else here
      if (group_2_split == length(ret$rgroup)){
        rgroup_after <- NULL
        n.rgroup_after <- NULL
      }
      
      ret$rgroup <- c(rgroup_b4, 
        "", lab, "",
        rgroup_after)
      ret$n.rgroup <- c(n.rgroup_b4,
        row_prior_match-sum(n.rgroup_b4), # The number of rows prior to our variable
        length(used_factors) + 1, # The number of factors + the one were adding
        ret$n.rgroup[group_2_split] - 
          (row_prior_match-sum(n.rgroup_b4)) - 
          length(used_factors),  # The remaining rows from that group, if 0 then removed below
        n.rgroup_after)
      
      
      # Remove empty group
      if (any(ret$n.rgroup == 0)){
        ret$rgroup <- ret$rgroup[-which(ret$n.rgroup == 0)]
        ret$n.rgroup <- ret$n.rgroup[-which(ret$n.rgroup == 0)]
      }
      
      if (any(ret$n.rgroup < 0))
        stop(sprintf("Sorry, the software created an invalid group length of less than 0, this occurred when adding the variable: %s (%s)", vn, lab))
      
      ret$values <- insertRow(ret$values, 
        matches[1], 
        ref_value,  
        rName=reference)
      
      return(ret)
    }
    
    getVnStats <- function(vn){
      # TODO: add some option of handling missing from the model, a second/third column
  
      # If there is a binomial outcome variable then 
      # it makes sense to have two columns, the overall
      # and the event data.
      if (any(class(model) %in% c("lrm", "coxph")) ||
          ("glm" %in% class(model) &&
            model$family$family == "binomial")){
          ret <- getDescriptionStatsBy(x=ds[is.na(outcome) == FALSE,vn], 
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
          ret <- ret[,c(1,3),drop=FALSE]
          colnames(ret) <- c("Total", "Event")
      }else{
        ret <- prGetStatistics(x=ds[is.na(outcome) == FALSE,vn],  
          show_perc = desc_show_tot_perc, 
          html = output == "html",
          digits = desc_digits,
          continuous_fn = desc_continuous_fn,
          prop_fn = desc_prop_fn,
          factor_fn = desc_factor_fn,
          show_missing = desc_show_missing)
        
      }
      return(ret)
    }
    
    for(vn in vars)
    {
      if (desc_column){
        stats[[vn]] <- getVnStats(vn)
      }
      
      if (is.factor(ds[,vn])){
        # Sometimes there is a subset argument or something leading to 
        # that one of the factors isn't included and therefore I use
        # this perhaps slightly trickier identification of all the factors
        available_factors <- as.character(unique(ds[, vn][is.na(ds[, vn]) == FALSE]))
        # We need to clean from characters that might cause issues with the 
        # regular expression
        regex_clean_levels <- gsub("([()\\[\\]{}\\.])", "\\\\\\1", available_factors, perl=TRUE)
        regex_clean_levels <- gsub("([+*\\.])", "[\\1]", regex_clean_levels, available_factors, perl = TRUE)
        matches <- grep(
          sprintf("^%s[=]{0,1}(%s)$", vn, 
            paste(regex_clean_levels,
              collapse="|")), 
          rownames(ret$values))
        if (length(matches) > 0){
          ret <- addReference(vn = vn, matches = matches, available_factors=available_factors, ret = ret)
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
      desc_mtrx <- matrix("-", ncol=cols, nrow=nrow(ret$values))
      rownames(desc_mtrx) <- rownames(ret$values)

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
        rowname <- getRowname(vn)
        if (is.factor(ds[,vn])){
          # Get the row number of the first element in that group of factors
          group_nr <- which(rowname == ret$rgroup)
          if (length(group_nr) == 0)
            stop(sprintf("Couldn't find the row '%s' (org. name %s) in the value matrix: '%s'", 
                rowname, vn, paste(ret$rgroup, collapse="', '")))
          if (length(group_nr) > 1)
            stop(sprintf(paste("Too many rows matched the row '%s' (org. name %s)\n",
                  "this is probably due to the fact that the name occurs twice",
                  "in the value matrix: '%s'\n",
                  "The most probably cause is that you have the same label() for two variables."), 
                rowname, vn, paste(ret$rgroup, collapse="', '")))
          
          if (group_nr == 1){
            row <- 1
          }else{
            row <- sum(ret$n.rgroup[1:(group_nr-1)]) + 1
          }

          last_row <- row+ret$n.rgroup[group_nr]-1
          existing_labels <- rownames(ret$values)[row:last_row]
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
          if (getRows(stats[[vn]]) > ret$n.rgroup[group_nr]){
            rows_2_add <- getRownames(stats[[vn]])[getRownames(stats[[vn]]) %nin% existing_labels]
            for (i in 1:length(rows_2_add)){
              rn <- rows_2_add[i]
              ret$values <- insertRow(ret$values, row + ret$n.rgroup[group_nr], rep("-", length.out=cols), rn)
              desc_mtrx <- insertRow(desc_mtrx, row + ret$n.rgroup[group_nr], getValue(stats[[vn]], rn), rn)
            }
            ret$n.rgroup[group_nr] <- getRows(stats[[vn]])
          }
        }else{
          # This is fairly straight forward as there is only one row per
          # value and we can find that row by just looking at the row name
          row <- grep(rowname, rownames(ret$values))
          if (length(row) == 0){
            row <- grep(vn, rownames(ret$values))
            if (length(row) == 0)
              stop(sprintf("Couldn't find the row '%s' (org. name %s) in the value matrix", rowname, vn))
          }
          
          desc_mtrx[row, ] <- stats[[vn]]
        }
      }
      
      ret$values <- cbind(desc_mtrx, ret$values)
      if (ncol(desc_mtrx) == 1 && colnames(ret$values)[1] == "")
        colnames(ret$values)[1] <- "Total"
      else if(all(colnames(ret$values)[1:2] == ""))
        colnames(ret$values)[1:2] <- c("Total", "Event")
      
    }
    
    return(ret)
  }
  
  rgroup <- NULL
  n.rgroup <- NULL
  if (length(order) > 1 || is.character(order)){
    greps <- getOrderVariables(rownames(x))
    
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
      ret <- addReferenceAndStatsFromModelData(reorderd_groups)
      reorderd_groups <- ret$values
      n.rgroup <- ret$n.rgroup
      rgroup <- ret$rgroup
    }else if (length(add_references) == length(greps)){
      if (desc_column)
        warning("The descriptive column works so far only when used with automated references")
      
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
      ret <- addReferenceAndStatsFromModelData(reorderd_groups)
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
      if (new_name == name)
        new_name <- getRowname(name)
      rn <- append(rn, new_name)
    }
  }else{
    rn <- c()
    for (name in rownames(reorderd_groups)){
      rn <- append(rn, getRowname(name))
    }
  }
  
  coef_name <- ifelse("coxph" %in% class(model), 
    "HR", 
    ifelse("lrm" %in% class(model) |
          ("glm" %in% class(model) &&
          model$family$family == "binomial"),
      "OR",
      "Coef"))
  if(desc_column){
    extra_cols <- ncol(reorderd_groups)-4
    just <- c(rep("r", times=extra_cols), rep(c("r", "c"), times=2))
    n.cgroup <- c(extra_cols, 2, 2)
    cgroup = c("", "Crude", "Adjusted")
  }else{
    just <- rep(c("r", "c"), times=2)
    n.cgroup <- c(2, 2)
    cgroup <- c("Crude", "Adjusted")
  }
  
  if (output == "html"){
    return(htmlTable(reorderd_groups, 
        headings      = sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups)), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = unlist(rn),
        n.cgroup      = n.cgroup, cgroup = cgroup, 
        col.just      = just,
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        rgroupCSSstyle= rgroupCSSstyle,
        rgroupCSSseparator = rgroupCSSseparator,
        ...))
  }else if (output == "latex"){
    return(latex(reorderd_groups, 
        colheads      = latexTranslate(sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups))), 
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = latexTranslate(unlist(rn)),
        n.cgroup      = n.cgroup, cgroup = cgroup, 
        col.just      = just,
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup, 
        ...))
  } else {
    return(list(x     = reorderd_groups, 
        headings      = sub("(Crude|Adjusted)", coef_name, colnames(reorderd_groups)),
        rowlabel.just = "l", 
        rowlabel      = "Variable",
        rowname       = unlist(rn),
        n.cgroup      = n.cgroup, cgroup = cgroup, 
        col.just      = just,
        rgroup        = rgroup, 
        n.rgroup      = n.rgroup))
  }
}
