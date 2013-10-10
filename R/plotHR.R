## Copyright (C) 2009 Reinhard Seifert, 
## biostatistician at Haukeland University Hospital Bergen, Norway.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## Improvements made 2012 by Max Gordon, 
## orthopaedic surgeon and PhD-student at the Karolinska Institute
##
## The changes consist of adaptation for use with the rms package, 
## multiple models plotting and some code optimization

#' Plot a spline in a Cox regression model
#' 
#' This function is a more specialized version of the \code{\link{termplot}()} function. It
#' creates a simple plot with the spline against hazard ratio. 
#' 
#' The function allows for plotting multiple splines in one graph. Sometimes you 
#' might want to show more than one spline for the same variable. This allows 
#' you to create that comparison. 
#' 
#' Examples of a situation where I've used multiple splines in one plot is when 
#' I want to look at a variables behavior in different time periods. This is another
#' way of looking at the proportional hazards assumption. The Schoenfeld residuals 
#' can be a little tricky to look at when you have the splines.
#' 
#' Another example of when I've used this is when I've wanted to plot adjusted and
#' unadjusted splines. This can very nicely demonstrate which of the variable span is
#' mostly confounded. For instance - younger persons may exhibit a higher risk for a
#' procedure but when you put in your covariates you find that the increased hazard
#' changes back to the basic
#' 
#' @param models A single model or a list() with several models
#' @param term The term of interest. Can be either the name or the number of the
#'   covariate in the model.
#' @param se Boolean if you want the confidence intervals or not
#' @param polygon_ci If you want a polygon as indicator for your confidence interval.
#'   This can also be in the form of a vector if you have several models. Sometimes
#'   you only want one model to have a polygon and the rest to be dotted lines. This
#'   gives the reader an indication of which model is important.
#' @param rug The rug is the density of the population along the spline variable. Often
#'   this is displayed as a jitter with bars that are thicker & more common when there
#'   are more observations in that area or a smooth density plot that looks like a
#'   mountain. Use "density" for the mountain view and "ticks" for the jitter format.
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param main The main title of the plot
#' @param xlim A vector with 2 elements containing the upper & the lower bound of the x-axis
#' @param ylim A vector with 2 elements containing the upper & the lower bound of the y-axis
#' @param col.term The color of the estimate line. If multiple lines you can have 
#'   different colors by giving a vector.
#' @param col.se The color of the confidence interval. If multiple lines you can have 
#'   different colors by giving a vector.
#' @param col.dens The color of the density plot. Ignored if you're using jitter
#' @param lwd.term The width of the estimated line. If you have more than one model then
#'   provide the function with a vector if you want to have different lines for
#'   different width for each model.
#' @param lty.term The typeof the estimated line, see lty. If you have more than one model 
#'   then provide the function with a vector if you want to have different line types for
#'   for each model.
#' @param lwd.se The line width of your confidence interval. This is ignored if you're using
#'   polygons for all the confidence intervals.
#' @param lty.se The line type of your confidence interval.  This is ignored if you're using
#'   polygons for all the confidence intervals.
#' @param x.ticks The ticks for the x-axis if you desire other than the default.
#' @param y.ticks The ticks for the y-axis if you desire other than the default.
#' @param ylog Show a logarithmic y-axis. Not having a logarithmic axis might seem easier 
#'   to understand but it's actually not really a good idea. The distance between HR 0.5 and
#'   2.0 should be the same. This will only show on a logarithmic scale and therefore it is
#'   strongly recommended to use the logarithmic scale.
#' @param cex Increase if you want larger font size in the graph.
#' @param plot.bty Type of box that you want. See the bty description in graphical parameters (par)
#'   If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles 
#'   the corresponding upper case letter. A value of "n" suppresses the box.
#' @param y_axis_side The side that the y axis is to be plotted, see axis() for details
#' @aliases par 
#' @param axes A boolean that is used to identify if axes are to be plotted 
#' @param alpha The alpha level for the confidence intervals
#' @param ... Any additional values that are to be sent to the plot() function
#' @return The function does not return anything
#' @references \url{http://rforge.org/plothr/}
#' 
#' @example examples/plotHR_example.R
#' 
#' @author Reinhard Seifert, Max Gordon
#' @export
plotHR <- function (models, 
  term      = 1, 
  se         = TRUE , 
  polygon_ci = TRUE, 
  rug        = "density", 
  xlab       = "", 
  ylab       = "Hazard Ratio" , 
  main       = NULL, 
  xlim       = NULL, 
  ylim       = NULL, 
  col.term   = "#08519C", 
  col.se     = "#DEEBF7", 
  col.dens   = grey(.9),
  lwd.term   = 3, 
  lty.term   = 1, 
  lwd.se     = lwd.term,
  lty.se     = lty.term,
  x.ticks    = NULL, 
  y.ticks    = NULL, 
  ylog       = TRUE,
  cex        = 1, 
  y_axis_side = 2,
  plot.bty   = "n", 
  axes       = TRUE, 
  alpha      = .05,
  ...){

  getCleanLabels <- function(m){
    ## extract the names of all model covariates
    all.labels <- attr(m$term , "term.labels")
    
    # remove 'strata()' / 'factor()' / 'pspline()' / 'rcs()' / 'as.integer()'
    all.labels <- sub ( "[a-zA-Z\\._]+[0-9a-zA-Z\\._][(]([a-zA-Z._0-9]*)[, .a-zA-Z_0-9=]*[)]" , "\\1" , all.labels) 
    
    # Remove interaction terms since the data can't be found, ex. male_gender:prosthesis
    # TODO: Remove and recheck since this is now in prGetModelVariables()
    terms_with_interaction <- grep("[_.a-zA-Z0-9]+:[_.a-zA-Z0-9]+", all.labels)
    if(length(terms_with_interaction)>0){
      all.labels <- all.labels[-terms_with_interaction]
    }
    
    return(all.labels)    
  }
  
  ## extract data from model;
  # only the covariates really used in the model
  # only complete covariate records (only used in the model anyway)
  # 'as.data.frame()' and 'names()' have to be explicitly specified in case of a univariate model
  getDataFromModel <- function(m){
    org_ds <- prExtractPredictorsFromModel(m)
    labels <- getCleanLabels(m)
    org_ds <- as.data.frame(na.exclude(org_ds[ , labels]))
    names(org_ds) <- labels
    return(org_ds)
  }
  
  ### _______________ the smooth term prediction ____________
  ## calculate the HR for all the covariate values found in the dataset
  getFitAndConfidenceInterval <- function(model){
    # Get new data to use as basis for the prediction
    new_data <- getDataFromModel(model)
    # Set all other but the variable of interest to the
    # mode or the median
    for (variable in colnames(new_data)){
      if (variable != term.label){
        if (is.numeric(new_data[,variable])){
          new_data[, variable] <- median(new_data[, variable])
        }else{
          new_data[, variable] <- names(which.max(table(new_data[, variable])))
		  new_data[, variable] <- factor(new_data[, variable])
        }
      }
    }
    new_data <- new_data[!duplicated(new_data[, term.label]), ]
    if (length(xlim) == 2){
      if (NCOL(new_data) == 1)
        new_data <- as.data.frame(
          matrix(
            new_data[new_data >= min(xlim) & 
                new_data <= max(xlim)], 
            ncol=1, 
            dimnames=list(NULL, c(term.label))))
      else
        new_data <- new_data[new_data[, term.label] >= min(xlim) &
            new_data[, term.label] <= max(xlim), ]
    }
    if(length(grep("cph", class(model))) > 0){
      # If this is a cph model then don't exclude the na values
      term <- predict (model, newdata=new_data, se.fit = TRUE , expand.na=FALSE, na.action=na.pass)
      
      term$fit <- term$linear.predictor
      term$se.fit <- term$se.fit
    }else{
      term <- predict (model, newdata=new_data, type="terms" , se.fit = TRUE , terms = term)
    }
    
    # attach the smooth fit for the HR ('fit') and the CI's to the dataset
    # The as.double is a fix since the data.frame otherwise changes name if pspline in coxph
    df <- data.frame(xvalues= new_data[,term.label],
      fit = as.double(term$fit), 
      ucl = as.double(term$fit + qnorm(1 -alpha/2) * term$se.fit),
      lcl = as.double(term$fit - qnorm(1 -alpha/2) * term$se.fit))
    
    # Change to exponential form
    if (ylog == FALSE){
      df$fit <- exp(df$fit)
      df$ucl <- exp(df$ucl)
      df$lcl <- exp(df$lcl)
    }
    
    # The line doesn't get any better if the value is a duplicate 
    # but the PDF gets very large if the dataset is large. By removing
    # duplicates this is avoided
    dups <- duplicated(df$xvalues)
    df <- df[dups == FALSE, ]
    
    return(df)    
  }

  # If the user wants to compare different models the same graph
  # the first dataset is then choosen as the default dataset
  # for getting the rug data.  
  if (length(class(models)) != 1 || class(models) != "list"){
    models <- list(models)
  }
  
  # Create vectors of the colors, line types etc to 
  # allow for specific settings for each model
  col.se <- rep(col.se, length.out=length(models))
  col.term <- rep(col.term, length.out=length(models))
  polygon_ci <- rep(polygon_ci, length.out=length(models))
  lty.term <- rep(lty.term, length.out=length(models))
  lwd.term <- rep(lwd.term, length.out=length(models))
  lty.se   <- rep(lty.se, length.out=length(models))
  lwd.se <- rep(lwd.se, length.out=length(models))
  
  # set plotting parameters
  par(las = 1 , cex = cex)
  
  # The y-limit needs to be log transformed to work as expected
  if(length(ylim) && ylog==TRUE){
    ylim <- log(ylim)
  }
  
  # Get the term number and it's label
  all.labels <- getCleanLabels(models[[1]])
  
  # Allow the term searched for be a string
  if (is.character(term)){
    term <- grep(term, all.labels)
    if(length(term) == 0){
      stop(paste("Could not find term:", term))
    }
  }
  
  # pick the name of the main term which is goint to be plotted
  term.label <- all.labels[term]
  if (is.na(term.label)){
    stop(paste("Term", term, "not found"))
  }
  
  if (length(ylim) > 0){
    plot_boundaries.y <- ylim
  }else{
    plot_boundaries.y <- NULL
  }
  
  # Just add the boundary values
  getYBoundaries <- function(ylim, variable){
    variable <- variable[is.infinite(variable) == FALSE]
    return (c(min(ylim, variable), max(ylim, variable)))
  } 
  
  xvalues <- NULL
  multi_data <- list()
  for (m in models){
    line_data <- getFitAndConfidenceInterval(m)
    multi_data <- append(multi_data, list(line_data))
    plot_boundaries.y <- getYBoundaries(plot_boundaries.y, line_data$fit)
	if (rug  == "ticks"){
		plot_boundaries.y[1] <- min(plot_boundaries.y) - (max(plot_boundaries.y)-min(plot_boundaries.y))*.1
	}
    if (NROW(line_data) > length(xvalues)){
      xvalues <- line_data$xvalues
    }
  }
  
  
  ### _______________ what now follows is the graphical manipulation of the plots ______
  
  # plot empty plot with coordinatesystem and labels
  plot_boundaries.x <- range(xvalues)
  plot(y=plot_boundaries.y, x=plot_boundaries.x, xlim = xlim , ylim = ylim , xlab = xlab,
    ylab = ylab , main = main, type = "n" , axes = FALSE, ...)
  
  # plot CI as polygon shade - if 'se = TRUE' (default)
  if (se) {
    plot_conf_interval <- function(model_data, line_or_polygon_color, polygon_ci, lwd, lty){
      current_i.backw <- order(model_data$xvalues , decreasing = TRUE)
      current_i.forw <- order(model_data$xvalues)
      
      if (polygon_ci == TRUE){
        # The x-axel is always the same
        x.poly <- c(model_data$xvalues[current_i.forw] , model_data$xvalues[current_i.backw])
        # The y axel is based upin the current model
        y.poly <- c(model_data$ucl[current_i.forw] , model_data$lcl[current_i.backw])
        polygon(x.poly , y.poly , col = line_or_polygon_color, border = NA)
      }else{
        lines(model_data$xvalues[current_i.forw] , model_data$ucl[current_i.forw], 
          col = line_or_polygon_color, 
          lwd = lwd, lty= lty)
        lines(model_data$xvalues[current_i.forw], 
          model_data$lcl[current_i.forw], 
          col = line_or_polygon_color, 
          lwd = lwd, lty= lty)
      }
    }
    
    # Plot the last on top
    for (i in rev(1:length(models))) {
      plot_conf_interval(multi_data[[i]], col.se[[i]], polygon_ci[[i]], lwd = lwd.se[i], lty=lty.se[i])
    }
  }
  
  
  # Use first model for density data
  base_data <- getDataFromModel(models[[1]])
  xvalues_4_density <- base_data[,term.label]
  
  # Choose within limits
  if (length(xlim) == 2){
    xvalues_4_density <- xvalues_4_density[xvalues_4_density >= min(xlim) & xvalues_4_density <= max(xlim)]
  }
  
  ### _______________ rug = "density" ____________________________________________________
  ### density plot at bottom of panel if rug = "density" in function call
  if (rug == "density") {
    # calculate the coordinates of the density function
    density <- density( xvalues_4_density )
    # the height of the densityity curve
    max.density <- max(density$y)
    
    # Get the boundaries of the plot to
    # put the density polygon at the x-line
    plot_coordinates <- par("usr")
    
    # get the "length" and range of the y-axis
    y.scale <- plot_coordinates[4] - plot_coordinates[3]
    
    # transform the y-coordinates of the density
    # to the lower 10% of the plotting panel
    density$y <- (0.1 * y.scale / max.density) * density$y + plot_coordinates[3]
    
    ## plot the polygon
    polygon( density$x , density$y , border = F , col = col.dens)
  }
  
  ## get the quartiles of the main term 
  quantiles <- quantile(xvalues , probs = c(0.025,0.25,0.50,0.75,0.975))
  
  # plot white lines (background color) for 2.5%tile, 1Q, median, 3Q and 97.5%tile through confidence shade and density plot
  axis( side = 1 , at = quantiles , labels = FALSE , lwd = 0 , col.ticks = "white"  , lwd.ticks = 1 , tck = 1 )
  
  
  ### _______________ rug = "ticks" ____________________________________________________
  ### rug plot if "ticks" is specified in function call
  if (rug == "ticks") {
    
    # rugs at datapoints
    axis(side = 1 , line = -1.2 , at = jitter(xvalues_4_density) , labels = F , tick = T , tcl = 0.8 , lwd.ticks = 0.1 , lwd = 0)
    # rugs and labels at 1Q, median and 3Q
    axis(side = 1 , line = -1.0 , at = fivenum(xvalues_4_density)[2:4], lwd = 0 , tick = T, tcl = 1.2 , lwd.ticks = 1 , col.ticks = "black" , labels = c("Quartile 1","Median","Quartile 3"), cex.axis = 0.7, col.axis = "black" , padj = -2.8)
    axis(side = 1 , line = 0.0 , at = fivenum(xvalues_4_density)[2:4], lwd = 0 , tick = T, tcl = 0.2 , lwd.ticks = 1 , col.ticks = "black", labels = FALSE)
  }
  
  
  # Plot the last fit on top, therefore use the reverse
  for (i in rev(1:length(models))) {
    current_i.forw <- order(multi_data[[i]]$xvalues)
    
    # Plots the actual regression line
    lines(multi_data[[i]]$xvalues[current_i.forw], 
      multi_data[[i]]$fit[current_i.forw], 
      col = col.term[[i]], 
      lwd = lwd.term[[i]],
      lty = lty.term[[i]])
  }
  
  # ___________ main plot _____________
  
  
  # plot the axes
  if (axes){
    axis(side = 1, at = x.ticks)
    if (is.null(y.ticks)){
      y.ticks <- axTicks(2)
    }else if (ylog == TRUE){
      # This is an assumption that the ticks
      # aren't provided in log
      y.ticks <- log(y.ticks)
    }
    
    
    if (ylog == TRUE){
      y.ticks_labels <- ifelse(exp(y.ticks) >= 1, 
        sprintf("%0.1f", exp(y.ticks)),
        sprintf("%0.2f", exp(y.ticks)))
      # Get familiar y-axis instead of the log
      axis(side = y_axis_side, at = y.ticks, 
           labels = y.ticks_labels)
    }else{
      axis(side = y_axis_side , at = y.ticks)
    }
  }
  
  # plot a box around plotting panel if specified - not plotted by default
  box(bty = plot.bty)
}
