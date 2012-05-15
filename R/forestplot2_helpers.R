#' Gets the boundaries for a fit
#' 
#' TODO: Change the cofidence interval by hand into confint()
#' 
#' @param crr.fit A model 
#' @param conf.int The interval of interest 0-1
#' @returnType data.frame
#' @return A data frame with all the details
#' 
#' @author max
getBoundariesDataFrameFromCRR <- function(crr.fit, conf.int = 0.95){
    # For sorting reasons create a data frame
    # from the crr object
    t <- data.frame(
            beta = crr.fit$coef,
            coef = exp(crr.fit$coef),  
            se = sqrt(diag(crr.fit$var)), 
            p_val = signif(2 * (1 - pnorm(abs(crr.fit$coef)/sqrt(diag(crr.fit$var)))), 5),
            order = rep(-1, length(crr.fit$coef)))
      
    # Set the names of the rows
    row.names(t) <- names(crr.fit$coef)
      
    # Calculate the confidence intervals
    a <- (1 - conf.int)/2
    a <- c(a, 1 - a)
    z <- qnorm(a)
      
    t$low <- exp(t$beta + z[1] * t$se)
    t$high <- exp(t$beta + z[2] * t$se)
    
    return(t)
}

#' Gets the boundaries for a fit
#' 
#' TODO: Change the confidence interval calculation to confint()
#' 
#' @param glm.fit A regression model 
#' @param conf.int The confidence interval 0-1
#' @returnType data.frame
#' @return A frame with all the data
#' 
#' @author max
getBoundariesDataFrameFromGlmFit <- function(glm.fit, conf.int = 0.95){
    summary_glm <- summary.glm(glm.fit)
    
    # Extract the summary values of interest
    summary_se <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Std. Error"]
    if ("quasipoisson" %in% glm.fit$family){
        summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|t|)"]
    }else if("poisson" %in% mg.poisson_fit.short.Charlsons_1yr$family){
        summary_p_val <- summary_glm$coefficients[,colnames(summary_glm$coefficients) == "Pr(>|z|)"]
    }else{
        stop("Type of analysis not prepared!")
    }
    
    variable_count <- length(glm.fit$coefficients)
    # For sorting reasons create a data frame
    # from the crr object
    t <- data.frame(
            beta = glm.fit$coefficients,
            coef = exp(glm.fit$coefficients),
            se = summary_se, 
            p_val = summary_p_val,
            order = rep(-1, variable_count))
    
    # Set the names of the rows
    row.names(t) <- names(glm.fit$coefficients)
    
    # Remove the intercept
    t <- t[names(glm.fit$coefficients) != "(Intercept)", ]
    
    # Calculate the confidence intervals
    a <- (1 - conf.int)/2
    a <- c(a, 1 - a)
    z <- qnorm(a)
    
    t$low <- exp(t$beta + z[1] * t$se)
    t$high <- exp(t$beta + z[2] * t$se)
    
    return(t)
}


#' Gets the boundaries for a fit
#' 
#' TODO: Change the confidence interval calculation to confint()
#' 
#' @param coxph.fit A cox regression model 
#' @param conf.int The confidence interval 0-1
#' @returnType data.frame
#' @return A frame with all the data
#' 
#' @author max
getBoundariesDataFrameFromCoxPH <- function(coxph.fit, conf.int = 0.95){
    # For sorting reasons create a data frame
    # from the crr object
    t <- data.frame(
            beta = coxph.fit$coefficients,
            coef = exp(coxph.fit$coefficients),  
            se = sqrt(diag(coxph.fit$var)), 
            p_val = signif(2 * (1 - pnorm(abs(coxph.fit$coefficients)/sqrt(diag(coxph.fit$var)))), 5),
            order = rep(-1, length(coxph.fit$coefficients)))
    
    # Set the names of the rows
    row.names(t) <- names(coxph.fit$coefficients)
    
    # Calculate the confidence intervals
    a <- (1 - conf.int)/2
    a <- c(a, 1 - a)
    z <- qnorm(a)
    
    t$low <- exp(t$beta + z[1] * t$se)
    t$high <- exp(t$beta + z[2] * t$se)
    
    return(t)
}

#' Inserts an empty row
#' 
#' @param org.data.frame A data frame 
#' @param new_row_position Where to insert the empty row
#' @returnType data.frame
#' @return Returns the same frame with an inserted row
#' 
#' TODO: This should probably be changed to insertRow in the misc package
#' 
#' @author max
addEmptyRow2Dataset <- function(org.data.frame, new_row_position){
    if (is.list(new_row_position)){
        if (is.numeric(new_row_position[[1]]) == FALSE){
            stop("The first element in the list containing numbers should be the row number")
        }
        
        row_name <- new_row_position[[2]]
        new_row_position <- new_row_position[[1]]
    }else if (is.numeric(new_row_position) == FALSE){
        stop("The row position should either be a row number or a list with a number and a row name")
    }else{
        row_name = "* EMPTY ROW *"
    }
    if (new_row_position == 0){
        # add row first
        new.data.frame <- rbind(
                            data.frame(
                                        beta=NA, 
                                        coef=NA, 
                                        se=NA, 
                                        p_val=NA, 
                                        order=-1,
                                        low=NA,
                                        high=NA, row.names=c(row_name)),
                            org.data.frame)
    }else if (new_row_position > nrow(org.data.frame)){
        # Add empty row last
        new.data.frame <- rbind(org.data.frame,
                            data.frame(
                                        beta=NA, 
                                        coef=NA, 
                                        se=NA, 
                                        p_val=NA, 
                                        order=-1,
                                        low=NA,
                                        high=NA, row.names=c(row_name)))
    }else{
        new.data.frame <- rbind(org.data.frame[1:new_row_position,],
                            data.frame(
                                        beta=NA, 
                                        coef=NA, 
                                        se=NA, 
                                        p_val=NA, 
                                        order=-1,
                                        low=NA,
                                        high=NA, row.names=c(row_name)),
                            org.data.frame[(1 + new_row_position):nrow(org.data.frame),])
    }
    return(new.data.frame)
}

#' Gets the x-ticks in a formatted version
#' 
#' @param low lower bound 
#' @param high upper bound
#' @param clip if the ci are clipped
#' @returnType vector
#' @return Returns a vector with the ticks apropriate
#' 
#' @author max
#' @export
getXTicks <- function(low, high, clip){
    # Get the right ticks
    lowest <- max(min(low, na.rm=TRUE), clip[1])
    highest <- min(max(high, na.rm=TRUE), clip[2])
    resolution <- highest-lowest
    if (resolution > 6){
        lowest <- floor(lowest)
        highest <- ceiling(highest)
        xticks <- seq(from=lowest, to=highest, by=1)
    }else{
        lowest <- floor(lowest*2)/2
        highest <- ceiling(highest*2)/2
        xticks <- seq(from=lowest, to=highest, by=.5)
    }
    
    return(xticks)
}

#' Plot survival objects
#' 
#' @param plot_title The title of the plot 
#' @param survival.fit A survival regression model
#' @param increase.line_height If you want to space the lines further apart
#' @param lwd.forestlines The width of the forestplots confidence interval
#' @param clip Lower and upper limits for clipping confidence intervals to arrows
#' @param lwd.xaxis lwd for the xaxis
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line
#' @param col.zero The color of the zero effect line (HR=1)
#' @param order.output The order in which to show the output
#' @param skip.variables Which variables to use
#' @param legend.title The title of the legend
#' @param legend.content Content of the legend
#' @param legend.position Position of the legend
#' @param legend.inset The inset of the legend
#' @param add.empty.rows Add empty rows
#' @param box.default.size The size of the boxes indicating the estimate in the forestplot 
#' @param cex Font size expansion
#' @param bg Background color
#' @param ... Passed to forestplot2()
#' @returnType void
#' @return Does not return anything
#' 
#' TODO: Should probably look into this function and clean it up a little
#' 
#' @author max
#' @export
forestplotSurvivalObjects <- function(plot_title,  
        survival.fit, 
        increase.line_height = 1.2,
        lwd.forestlines=2,
        clip = c(-Inf, Inf), 
        lwd.xaxis=2,
        lwd.zero=2,
        col.zero = "lightgray",
        order.output = "P-value",
        skip.variables = NA,
        legend.title = "Type of analysis",
        legend.content = NA,
        legend.position = "topright",
        legend.inset = .05,
        add.empty.rows = NA,
        box.default.size = NA,
        cex = 1,
        bg="transparent",
        ...)
{
    checkValidSurvivalObject <- function(fit){
        if (!is.object(fit)){
            stop("You must provide a valid survival object of either Cox PH, CRR or Poisson Regr.")
        }
        if (length(class(fit)) > 1){
            if (class(fit)[1] != "glm"){
                stop(paste("Not a valid Poisson object: class=", class(fit)))
            }
        }else if(class(fit) != "crr" && class(fit) != "coxph"){
            stop("Object not of Cox PH type or Comp. Risk Regression")
        }
        
        return(TRUE)
    }
    
    if (is.list(survival.fit)){
        for(i in 1:length(survival.fit)){
            checkValidSurvivalObject(survival.fit[[i]])
        }
        
        # Reverse so the first doesn't end up last
        survival.fit <- rev(survival.fit)
        if (length(legend.content) > 1){
            legend.content <- rev(legend.content)
        } 
    }else{
        checkValidSurvivalObject(survival.fit)
        # Transform into a list to make the handling easier
        survival.fit = list(survival.fit)
    }
    
    
    plotSurvivalForestplot <- function(fontfamily.summary = NULL, fontfamily.labelrow = NULL){
        if (length(add.empty.rows) > 1 || is.list(add.empty.rows)){
            count = 0
            for(row_nr in add.empty.rows){
                for(i in 1:length(survival.dataframes)){
                    if (is.list(row_nr)){
                        survival.dataframes[[i]] <- 
                                addEmptyRow2Dataset(org.data.frame = survival.dataframes[[i]], 
                                        new_row_position = list(row_nr[[1]]+count, row_nr[[2]]))
                    }else{
                        survival.dataframes[[i]] <- 
                                addEmptyRow2Dataset(org.data.frame = survival.dataframes[[i]], 
                                        new_row_position = row_nr+count)
                    }
                    
                }
                count = count + 1
            }
        }
        
        
        t.clr <- meta.colors(box="royalblue",line="darkblue", summary="royalblue", zero=col.zero)
        
        rn <- rownames(survival.dataframes[[1]])
        keep.variables = rep(TRUE, length=length(rn))
        if (length(skip.variables) > 1){
            for(sk in skip.variables){
                keep.variables = !(sk == rn) & keep.variables
            }
        }
        rn <- modify_rownames(rn[keep.variables])
        col1 <- append("Variable", rn[[1]])
        is.summary <- c(TRUE, rep(FALSE, length(rn[[1]])))
        
        s.frame_count <- length(survival.dataframes)
        for(i in 1:s.frame_count){
            survival.dataframes[[i]] <- subset(survival.dataframes[[i]], keep.variables)
        }
        
        survival.dataframes[[1]] <- addEmptyRow2Dataset(org.data.frame = survival.dataframes[[1]], 
                new_row_position = 0)
        
        # Initiate the plot data
        t.coef <- survival.dataframes[[1]]$coef
        t.low <- survival.dataframes[[1]]$low
        t.high <- survival.dataframes[[1]]$high
        
        
        if (is.na(box.default.size)){
            box.default.size <- 1/s.frame_count    
        }
        
        variable_count <- length(survival.dataframes[[1]]$beta) 
        b_size <- rep(box.default.size, variable_count)
        b_size[survival.dataframes[[1]]$p_val<.05] <- box.default.size*1.5
        
        if (s.frame_count > 1){
            available_colors <- rbind(
                    c("royalblue", "darkblue"), 
                    c("gold", "orange"), 
                    c(rgb(200/255, 0, 0), rgb(100/255, 0, 0)),
                    c(rgb(148/255, 0, 211/255), rgb(148/255, 0, 211/255))
            )
            box_clr <- available_colors[1,1]
            line_clr <- available_colors[1,2]
            for(i in 2:s.frame_count){
                survival.dataframes[[i]] <- addEmptyRow2Dataset(org.data.frame = survival.dataframes[[i]], 
                        new_row_position = 0)
                t.coef <- cbind(t.coef, survival.dataframes[[i]]$coef)
                t.low <- cbind(t.low, survival.dataframes[[i]]$low)
                t.high <- cbind(t.high, survival.dataframes[[i]]$high)
                
                b_size_tmp <- rep(box.default.size, variable_count)
                b_size_tmp[survival.dataframes[[i]]$p_val<.05] <- box.default.size*1.5
                b_size <- cbind(b_size, b_size_tmp)
                
                box_clr <- cbind(box_clr, available_colors[i,1])
                line_clr <- cbind(line_clr, available_colors[i,2])
            }
            
            t.clr <- meta.colors(
                    box=box_clr, 
                    line=line_clr, 
                    zero=col.zero)
            
            rn <- list(col1)
        }else{
            # To complex for most
            #col2 <- list(expression(plain(e)^beta));
            col2 <- list("HR");
            # Actually this is only a bug that I tried to circumvent...
            for(coef in survival.dataframes[[1]]$coef[2:nrow(survival.dataframes[[1]])]){
                if (is.na(coef)){
                    col2 <- append(col2, NA)
                }else{
                    str_coef <- toString(round(coef, 2))
                    if (nchar(str_coef) == 3){
                        str_coef = paste(str_coef, "0", sep="")
                    }else if (nchar(str_coef) == 1){
                        str_coef = paste(str_coef, ".00", sep="")
                    }
                    # Add some space in the graph
                    col2 <- append(col2, paste(" ", str_coef, " ", sep=""))
                }
            }
            rn <- list(
                    col1,
                    col2)
            
            
        }
        
        # Get the right ticks
        xticks <- getXTicks(t.low, t.high, clip)
        
        forestplot2(rn, 
                xlim=c(0,10),
                mean=t.coef, 
                lower=t.low, 
                upper=t.high,
                increase.line_height = increase.line_height,
                lwd.forestlines=lwd.forestlines,
                fontfamily.labelrow = fontfamily.labelrow,
                fontfamily.summary = fontfamily.summary,
                graphwidth=unit(15, "centimeters"), 
                clip=c(lowest, highest),
                col= t.clr,
                boxsize=b_size,
                cex=cex,
                lwd.xaxis=lwd.xaxis,
                xticks=xticks,
                lwd.zero=lwd.zero,
                xlab="Hazard Ratio",
                is.summary = is.summary,
                zero=1, ...)
        title(main=list(plot_title, cex=cex*1.2))
        
        if (length(legend.content) > 1 || is.na(legend.content) == FALSE){
            legend(legend.position, inset=legend.inset,
                    cex=cex, 
                    title=legend.title,
                    legend=rev(legend.content), 
                    fill=rev(t.clr$box), horiz=FALSE, text.width=.2)
        }
    }
    
    getBoundaries4SurvivalObject <- function(survival.object){
        if (length(class(survival.object)) == 1){
            if(class(survival.object) == "crr"){
                sd <- getBoundariesDataFrameFromCRR(crr.fit = survival.object, conf.int = 0.95)
            }else if(class(survival.object) == "coxph"){
                sd <- getBoundariesDataFrameFromCoxPH(coxph.fit = survival.object, conf.int = 0.95)
            }else{
                stop(paste("Unknown fit class type:", class(survival.object)))
            }  
            
        } else if(class(survival.object)[1] == "glm"){
            sd <- getBoundariesDataFrameFromGlmFit(glm.fit = survival.object, conf.int = 0.95)
        } else {
            stop(paste("Unknown fit class type:", class(survival.object)))
        }
        
        return(sd)
    }
    
    
    checkIfCompatibleFits <- function(sd1, sd2){
        checkEqualNames <- function(base_names, cmpr_names){
            # A function for checking that the labels/row names of 
            # the two vectors are the same so that we compare the
            # right elements 
                    
            getFilteredNames <- function(name_list){
                # The row names usually have some prependings differing
                # the different variables and therefore looking for regex
                # might be a solution for finding only the parts interesting
                # for comparison, example: 1yr_variable_a == 2yr_variable_a
                reg_ex_str <- c("^(\\* EMPTY ROW \\*|age_group\\.(less|more)\\.(50|75)|age_group\\.50\\.to\\.59|male_gender)$",
                                            "^[A-Za-z]+_[0-9]+yr_([A-Za-z_])",
                                            "^x(RF [0-9])", "^(RF [0-9])",
                                            "^x(Risk factor no: [0-9])", "^(Risk factor no: [0-9])")
                filtered_names = c()
                for(name in name_list){
                    found=FALSE
                    
                    # Look for matching string part
                    for(rx in reg_ex_str){
                        t <- strapply(name, rx)
                        if (!is.null(t[[1]])){
                            filtered_names <- append(filtered_names, t[[1]])
                            found=TRUE
                            break;
                        }
                    }
                    
                    # If not found then use the actual name
                    if (!found){
                        filtered_names <- append(filtered_names, name)
                    }
                }
                
                return(filtered_names)
            }
            
            base <- getFilteredNames(base_names)
            comp <- getFilteredNames(cmpr_names)
            
            # Check that the vectors contain the same elements
            if (length(comp[comp %in% base == FALSE]) > 0){
                extra_elements <- paste(comp[comp %in% base == FALSE], collapse=", ")
                stop(paste("There are more elements in comparing vector, additional elements are:", extra_elements))
            }else if (length(base[base %in% comp == FALSE]) > 0){
                extra_elements <- paste(base[base %in% comp == FALSE], collapse=", ")
                stop(paste("There are more elements in base vector, additional elements are:", extra_elements))
            }
            
            # Check that the sorting order is the same between the two vectors
            for(i in 1:length(comp)){
                if (base[i] != comp[i])
                    stop(paste("Invalid sort order between the two vectors", base[i], "!=", comp[i]))
            }
            
            return(TRUE)
        }
        
        if (length(sd1$coef) != length(sd2$coef)){
            stop("The number of variables in the two objects dont match")
        }
        if (checkEqualNames(rownames(sd1), rownames(sd2)) == FALSE){
            stop("Failed to pass the name check!")
        }
        
        return(TRUE)
    }
    
    base_sd <- getBoundaries4SurvivalObject(survival.fit[[1]])
    survival.dataframes <- list(base_sd)
    if (length(survival.fit) > 1){
        for(i in 2:length(survival.fit)){
            new_sd <- getBoundaries4SurvivalObject(survival.fit[[i]])
            checkIfCompatibleFits(base_sd, new_sd)
            survival.dataframes <- c(survival.dataframes, list(new_sd))
        }
    }
    
    # Find the variables that belong to the score
    # the other variables should not be sorted
    # and a blank space is to appear between to separate them
    score_variables <- grep("[A-Z]+[a-zA-Z0-9_]+_[0-9]+yr_|^Combined|^elixhausers_group|^charlsons_group|^rs_charlsons_group", 
            rownames(survival.dataframes[[1]]))
       
    no_frames <- length(survival.dataframes)
    if (tolower(order.output) == "p-value" && length(score_variables) > 1){
        # Sort according to p-value
        # Since reversed used the last frame to sort upon
        survival.dataframes[[no_frames]]$order[score_variables] <- survival.dataframes[[no_frames]]$p_val[score_variables]
               
        if (no_frames > 1){
            for(i in 1:(no_frames-1)){
                # Sort the cox regression in the sam manner and therefore they have the same order
                survival.dataframes[[i]]$order <- survival.dataframes[[no_frames]]$order
                survival.dataframes[[i]] <- survival.dataframes[[i]][order(survival.dataframes[[i]]$order), ]
            }
        }
        
        # Sort the base data frame
        survival.dataframes[[no_frames]] <- survival.dataframes[[no_frames]][order(survival.dataframes[[no_frames]]$order), ]
    }
    
    # Insert a new row at the end of the regular variables
    if (length(score_variables) > 0){
        new_row_position <- length(survival.dataframes[[1]]$beta)-length(score_variables)
        
        if (length(survival.dataframes) > 1){
            for(i in 2:length(survival.dataframes)){
                survival.dataframes[[i]] <- addEmptyRow2Dataset(survival.dataframes[[i]], new_row_position = new_row_position)
            }
        }
        
        survival.dataframes[[1]] <- addEmptyRow2Dataset(survival.dataframes[[1]], new_row_position = new_row_position)
    }
    
    plotSurvivalForestplot()
}