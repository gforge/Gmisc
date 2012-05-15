#' This function helps with printing regression models
#' 
#' This function is used for printing the adjusted and unadjusted values
#' for a regression model. It takes a full model and walks through each
#' variable, removes in the regression all variables except one then
#' reruns that variable to get the unadjusted value.
#' 
#' This function saves a lot of time creating tables since it compiles a fully
#' unadjusted list of all your used covariates.
#' 
#' If the model is an exponential logit/cox regression model then it automatically
#' reports the exp() values instead of the original values
#' 
#' The function skips by default all spline variables since this becomes very complicated
#' and there is no simple \deqn{\beta}{beta} to display. 
#' 
#' @param fit The regression model
#' @param digits Number of digits you want in your estimates
#' @returnType matrix
#' @return Returns a matrix with the columns c("Crude", "95% CI", "Adjusted", "95% CI").
#'   The rows are not changed from the original fit.
#' 
#' @author max
#' @export
print_adjusted_and_unadjusted <- function(fit, digits=2){
    # Just a prettifier for the output an alternative could be:
    # paste(round(x[,1],1), " (95% CI ", min(round(x[,2:3])), "-", max(round(x[,2:3])), ")", sep="")  
    get_coef_and_ci <- function(fit, skip_intercept=FALSE){
        # Just to make sure that it gives 1.0 and 
        # not 1 if digits = 1, in cases where a 
        # adding another decimal that is used
        # since everyone is so hyped about p-val < 0.05
        add_zero_to_var <- function(x){
            return(sprintf(sprintf("%%0.%df", digits), x))
        }

        # Get coefficients and conf. interval
        my_coefficients <- coef(fit)
        ci <- confint(fit)
        coef_names <- names(my_coefficients)
        
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

        
        confidence_string <- sprintf("%%0.%df - %%0.%df", digits, digits)
        if (length(my_coefficients) > 1){
            my_coefficients <- tapply(my_coefficients, 1:length(my_coefficients), FUN = add_zero_to_var)
            ci <- sprintf(confidence_string, ci[,1], ci[,2])
        }else{
            my_coefficients <- add_zero_to_var(my_coefficients)
            ci <- sprintf(confidence_string, ci[1], ci[2])
        }
                
        ret_val <- cbind(my_coefficients, ci)
        colnames(ret_val) <- c("", "2.5% - 97.5%")
        rownames(ret_val) <- coef_names
        return(ret_val)
    }
    
    # Extract all the term names
    all.terms <- terms(fit)	
    var_names <- attr(all.terms, 'term.labels')
    
    # Skip variables consisting of
    # functions such as spline, strata variables
    regex_for_unwanted_vars <- "^(strat[a]{0,1}|ns|rcs|bs|pspline)[(]"
    skip_variables <- grep(regex_for_unwanted_vars, var_names)
    
    # Get the adjusted variables
    adjusted <- get_coef_and_ci(fit)
    # When using splines, rcs in cox regression this shows a little different

    # Remove all the splines, rcs etc
    rn <- rownames(adjusted)
    remove <- grep("(\'{1,}|[[][0-9]+[]]|[)][0-9]+)$", rn)
    remove <- union(remove, grep(regex_for_unwanted_vars, rn))
    
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
    for(variable in variables_to_check){
        interaction_variable <- length(grep(":", variable)) > 0
        
        # If it's an interaction variable the
        # interacting parts have to be included  
        if (interaction_variable){
            variable <- paste(paste(unlist(strsplit(variable, ":")), sep="", collapse=" + "), variable, sep=" + ")
        }
        
        # Run the same fit but with only one variable
        fit_only1 <- update(fit, paste(".~", variable))
        
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
        unadjusted <- rbind(c("-", "-"), unadjusted)
        rownames(unadjusted)[1] <- "Intercept"
    }
    
    both <- cbind(unadjusted, adjusted)
    colnames(both) <- c("Crude", "95% CI", "Adjusted", "95% CI")
    return(both)
}

#' Prints a simple fit summary
#' 
#' Sometimes you just want a simple fit print without all the details
#' and with only the confidence intervals. 
#' 
#' @param fit Regression model object 
#' @param digits Number of digits you want to round to
#' @returnType void
#' @return The function only prints
#' 
#' @author max
#' @export
print_fit <- function(fit, digits=2){
    tmp_vals <- cbind(round(coef(fit), digits), round(confint(fit), digits))
    vars <- apply(tmp_vals, 1, FUN=function(x){
                        tmp <- paste(x[1], "; 95% CI ", min(x[2:3]), "-", max(x[2:3]), sep="")
                        return(tmp)
                })
    
    nchar(names(vars))
    length(as.character())
    for(i in 1:length(vars)){
        cat(names(vars)[i], ":", sep="")
        if (substr(vars[i], 1, 1) == "-"){
            add <- 0
        }else{
            add <- 1
        }
        cat(rep(" ", times=max(nchar(names(vars)))-nchar(names(vars)[i])+add), sep="")
        cat(vars[i])
        cat("\n")
    }
}


#' Prints latex table for a fitted object
#' 
#' @param x An output from the print_adjusted_and_unadjusted() function or
#'   a regression model if you want to call the function from within
#' @param order A vector with regular expresions for each group.
#' @param add_references A vector with names. Sometimes you want to indicate 
#'   the reference row for each group. This needs to be just as many as the 
#'   groups as the order identified. Use NA if you don't want to have a 
#'   reference for that particular group.
#' @param groups A vector with group names if you want to have groups
#'   to some of the identified order groups. If you wish to skip one just
#'   us NA for that instance.
#' @param caption The table caption
#' @param ctable If the latex is to be in ctable format
#' @returnType String
#' @return Returns a latex formatted table
#' @importMethodsFrom miscTools insertRow
#' 
#' @author max
#' @export
latex_crude_and_adjusted <- function(x, 
		order,
		digits = 2,
        add_references=NULL, 
		groups=NULL,
        caption=NULL, 
		ctable=TRUE)
{
	# Convert the x that should be a model into a matrix that
	# originally was expected
	if(is.matrix(x) == FALSE){
		x <- print_adjusted_and_unadjusted(x, 2)
	}
	
    if (length(order) == 0 || is.vector(order) == FALSE 
            || is.character(order) == FALSE){
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
        library(micEcon)
        line_row <- 1
        for(i in 1:length(greps)){
            # Add reference if it's not empty
            if (is.na(add_references[i]) == FALSE){
                reorderd_groups<- insertRow(reorderd_groups, 
						line_row, 
						rep(c(1, "ref"), times=2),  
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
    
    rn <- modify_rownames(rownames(reorderd_groups))
    return(latex(reorderd_groups, file="", colheads=latexTranslate(colnames(reorderd_groups)), 
                  rowlabel.just="l", rowlabel="Variable",
                  rowname=latexTranslate(unlist(rn)),
                  n.cgroup = c(2, 2), cgroup = c("Crude", "Adjusted"), 
                  col.just = strsplit("rcrc", "")[[1]],
                  caption = caption,
                  rgroup=rgroup, n.rgroup=n.rgroup, 
                  ctable=ctable))
}