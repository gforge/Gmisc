#' A more advanced version of the original R builtin termplot function.
#'
#' This function can plot polygons instead of lines for confidence intervals,
#' density curve at the x-axis instead of just a jitter rug,  
#' identifies and warns for interaction terms. It also allows for rescaling
#' to an exponential y-axis.
#'  
#' @param model fitted model object
#' @param data data frame in which variables in model can be found
#' @param envir environment in which variables in model can be found
#' @param partial.resid logical; should partial residuals be plotted?
#' @param rug add rugplots (jittered 1-d histograms) to the axes?
#' @param terms which terms to plot (default NULL means all terms)
#' @param se plot pointwise standard errors?
#' @param xlabs vector of labels for the x axes
#' @param ylabs vector of labels for the y axes
#' @param main logical, or vector of main titles; if TRUE, the model's 
#'   call is taken as main title, NULL or FALSE mean no titles
#' @param col.term color for the term curve, see lines.
#' @param lwd.term line width for the term curve, see lines.
#' @param col.se colorfor the twice-standard-error curve when se = TRUE
#' @param lty.se line type for the twice-standard-error curve when se = TRUE
#' @param lwd.se line width for the twice-standard-error curve when se = TRUE
#' @param col.res color when partial.resid = TRUE, see points. 
#' @param cex plotting character expansion when partial.resid = TRUE, see points. 
#' @param pch type for partial residuals, when partial.resid = TRUE, see points. 
#' @param col.smth Passed to smooth
#' @param lty.smth Passed to smooth
#' @param span.smth Passed to smooth
#' @param ask logical; if TRUE, the user is asked before each plot, see par(ask=.).
#' @param use.factor.levels hould x-axis ticks use factor levels or numbers for factor terms?
#' @param smooth NULL or a function with the same arguments as panel.smooth 
#'   to draw a smooth through the partial residuals for non-factor terms
#' @param ylim an optional range for the y axis, or "common" when a range sufficient 
#'   for all the plot will be computed, or "free" when limits are computed for each plot.
#' @param rug.type if "density" rug or regular "rug" (i.e. jitter) should be used 
#' @param yscale if the scale should be "exponential" or "regular"
#' @param col.dens the color of the density. Only used when rug.type = "density"
#' @param se.type if the confidence interval should be two lines or a polygon 
#' @param density.proportion the height of the density plot
#' @param ... passed on to plot() function 
#' @return number of plotted terms
#' 
#' @example examples/termplot2_example.R
#' 
#' @author Max Gordon, R-project
#' 
#' @export
termplot2 <- function (model, 
  data               = NULL, 
  envir              = environment(formula(model)), 
  partial.resid      = FALSE, 
  rug                = FALSE, 
  terms              = NULL, 
  se                 = FALSE, 
  xlabs              = NULL, 
  ylabs              = NULL, 
  main               = NULL, 
  col.term           = 2, 
  lwd.term           = 1.5, 
  col.se             = "orange", 
  lty.se             = 2, 
  lwd.se             = 1, 
  col.res            = "gray", 
  cex                = 1, 
  pch                = par("pch"), 
  col.smth           = "darkred", 
  lty.smth           = 2, 
  span.smth          = 2/3, 
  ask                = dev.interactive() && nb.fig < n.tms, 
  use.factor.levels  = TRUE, 
  smooth             = NULL, 
  ylim               = "common",
  rug.type           = "rug", 
  yscale             = "regular",
  col.dens           = "#80808033", 
  se.type            = "line", 
  density.proportion = .1, 
  ...) 
{
  # Options for new variables
  # rug.type: "rug", "density"
  # yscale: "regular", "exponential"
  # se.type: "line", "polygon"
  
  # Basic functions used by termplot
  se.lines <- function(x, iy, i, ff = 2) {
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    lines(x, upper_ci, lty = lty.se, lwd = lwd.se, 
      col = col.se)
    lines(x, lower_ci, lty = lty.se, lwd = lwd.se, 
      col = col.se)
  }
  # The iy variable contains the ordering of the y-variable
  # the x-variable is already ordered 
  se.polygon <- function(x, iy, i, ff = 2){
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    
    current_i.backw <- order(x, decreasing = TRUE)
    current_i.forw <- order(x, decreasing = FALSE)
    
    # The x-axel is always the same
    x.poly <- c(x[current_i.forw] , x[current_i.backw])
    # The y axel is based upin the current model
    y.poly <- c(upper_ci[current_i.forw], lower_ci[current_i.backw])
    polygon(x.poly , y.poly , col = col.se, border = NA)
  }
  plot.density <- function(xx){
    # calculate the coordinates of the density function
    density <- density( xx )
    # the height of the densityity curve
    max.density <- max(density$y)
    
    # transform the y-coordinates of the density
    if (density.proportion >= 1){
      warning("Can't have a density proportion of 100 % of the plot, recommended is less than 0.2")
      density.proportion <- .1
    }
    
    # Get the boundaries of the plot to
    # put the density polygon at the x-line
    yscale <- par("usr")[3:4]
    # get the "length" and range of the y-axis
    yspan <- max(yscale) - min(yscale)
    
    density_percent <- density$y/max.density
    height <- density.proportion * density_percent * yspan  + min(yscale)
    if (par("ylog")){
      # For some odd reason the default log scale is 10-based
      # when the y-scale is logarithmic
      height <- 10^(height)
    }
    
    ## plot the polygon
    polygon( density$x , height, border = F, col = col.dens)
  }
  plot.rug <- function(xx){
    n <- length(xx)
    lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1L] + 
          c(0, 0.05, NA) * diff(ylims), n))
    if (partial.resid) 
      lines(rep.int(xlims[1L] + c(0, 0.05, NA) * diff(xlims), 
          n), rep.int(pres[, i], rep.int(3, n)))
  }
  
  getXlims.factor <- function(ff){
    ll <- levels(ff)
    xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
    
    if (rug) {
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
      xlims[2L] <- xlims[2L] + 0.03 * diff(xlims)
    }
    return (xlims)
  }
  
  getXlims.continuos <- function(xx){
    range(xx, na.rm = TRUE)
    if (rug && rug.type != "density") 
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
    
    return (xlims)
  }
  
  plot.factor <- function(i, ff, xx, 
    xlab, ylab, main,
    xlims = xlims){
    
    if (!is.null(model$na.action)) 
      ff <- naresid(model$na.action, ff)
    
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(1, 0, type = "n", 
      xlab = xlab, 
      ylab = ylab, 
      xlim = xlims, 
      ylim = tmp_ylims, 
      main = main, 
      xaxt = "n", 
      ...)
    
    ll <- levels(ff)
    if (use.factor.levels) 
      axis(1, at = seq_along(ll), labels = ll, ...)
    else axis(1)
    for (j in seq_along(ll)) {
      ww <- which(ff == ll[j])[c(1, 1)]
      jf <- j + c(-0.4, 0.4)
      
      # Plot confidence interval
      if (se){
        if (se.type == "polygon"){
          se.polygon(jf, iy = ww, i = i)
        }else{
          se.lines(jf, iy = ww, i = i)
        }
      }
      
      yvalues <- tms[ww, i]
      if (identical(yscale, "exponential")){
        yvalues <- exp(yvalues)
      }
      lines(jf, yvalues, col = col.term, lwd = lwd.term,
        ...)
    }
  }
  
  plot.continuous <- function(i,
    xx, 
    xlab, ylab, main,
    xlims = xlims){
    
    
    if (!is.null(use.rows)) 
      xx <- xx[use.rows]

    oo <- order(xx)
    
    yvalues <- tms[, i]
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      yvalues <- exp(yvalues)
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(range(xx), range(yvalues), type = "n", xlab = xlab,  log=log,
      ylab = ylab, xlim = xlims, ylim = tmp_ylims, 
      main = main[i], 
      ...)
    
    # Plot confidence interval
    if (se){
      if (se.type == "polygon"){
        se.polygon(xx[oo], iy = oo, i = i)
      }else{
        se.lines(xx[oo], iy = oo, i = i)
      }
      
    }
    
    lines(xx[oo], yvalues[oo], 
      col = col.term, lwd = lwd.term)
  }
  
  # Get the the terms that are of interest
  which.terms <- terms
  terms <- if (is.null(terms)) 
      predict(model, type = "terms", se.fit = se)
    else predict(model, type = "terms", se.fit = se, terms = terms)
  
  # Get the data used for the rug
  mf <- model.frame(model)
  if (is.null(data)) 
    data <- eval(model$call$data, envir)
  if (is.null(data)) 
    data <- mf
  
  tms <- as.matrix(if (se) 
        terms$fit
      else terms)
  
  use.rows <- if (NROW(tms) < NROW(data)) 
    match(rownames(tms), rownames(data))
  
  nmt <- colnames(tms)
  # Remove interaction terms
  if (any(grep(":", nmt) > 0)){
    for(i in grep(":", nmt)){
      warning(sprintf("The interaction term '%s' won't be displayed", nmt[i]))
      nmt <- nmt[-i]
    }
  }
  
  n.tms <- length(nmt)
  
  cn <- parse(text = nmt)
  if (!is.null(smooth)) 
    smooth <- match.fun(smooth)
  if (is.null(ylabs)) 
    ylabs <- paste("Partial for", nmt)
  if (is.null(main)) 
    main <- ""
  else if (is.logical(main)) 
    main <- if (main) 
        deparse(model$call, 500)
      else ""
  else if (!is.character(main)) 
    stop("'main' must be TRUE, FALSE, NULL or character (vector).")
  main <- rep(main, length.out = n.tms)
  pf <- envir
  carrier <- function(term) {
    if (length(term) > 1L) 
      carrier(term[[2L]])
    else eval(term, data, enclos = pf)
  }
  carrier.name <- function(term) {
    if (length(term) > 1L) 
      carrier.name(term[[2L]])
    else as.character(term)
  }
  if (is.null(xlabs)) 
    xlabs <- unlist(lapply(cn, carrier.name))
  if (partial.resid || !is.null(smooth)) {
    pres <- residuals(model, "partial")
    if (!is.null(which.terms)) 
      pres <- pres[, which.terms, drop = FALSE]
  }
  is.fac <- sapply(nmt, function(i){
      if (i %in% colnames(mf)){
        return(is.factor(mf[, i]))
      }
      cn <- grep(sprintf("(^%s$|^[a-zA-Z0-9]+[(][ ]*%s[, 0-9a-zA-Z\"\']+[)])", i, i), colnames(mf))
      if (length(cn)==0){
        stop(sprintf("Could not find '%s' in dataset", i))
      }else if (length(cn) > 1){
        cn <- cn[1]
        warning("More than one name match found")
      }
      return(is.factor(mf[, cn]))
    })
  
  nb.fig <- prod(par("mfcol"))
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  ylims <- ylim
  if (identical(ylims, "common")) {
    ylims <- if (!se) 
        range(tms, na.rm = TRUE)
      else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 * 
            2 * terms$se.fit, na.rm = TRUE)
    if (partial.resid) 
      ylims <- range(ylims, pres, na.rm = TRUE)
    if (rug) 
      ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
  }
  
  for (i in 1L:n.tms) {
    if (identical(ylim, "free")) {
      ylims <- range(tms[, i], na.rm = TRUE)
      if (se) 
        ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[, 
            i], tms[, i] - 1.05 * 2 * terms$se.fit[, i], 
          na.rm = TRUE)
      if (partial.resid) 
        ylims <- range(ylims, pres[, i], na.rm = TRUE)
      if (rug) 
        ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    }
    
    if (is.fac[i]) {
      ff <- mf[, nmt[i]]
      xx <- as.numeric(mf[, nmt[i]])
      xlims <- getXlims.factor(ff)
      plot.factor(i, 
        ff=ff,
        xx=xx,
        xlab=xlabs[i], ylab=ylabs[i],
        main=main[i],
        xlims = xlims)
    }
    else {
      xx <- carrier(cn[[i]])
      xlims <- getXlims.continuos(xx)
      plot.continuous(i, xx=xx, 
        xlab=xlabs[i], 
        ylab=ylabs[i],
        main=main[i],
        xlims = xlims)
      
    }
    if (partial.resid) {
      if (!is.fac[i] && !is.null(smooth)) {
        smooth(xx, pres[, i], lty = lty.smth, cex = cex, 
          pch = pch, col = col.res, col.smooth = col.smth, 
          span = span.smth)
      }
      else points(xx, pres[, i], cex = cex, pch = pch, 
          col = col.res)
    }
    
    if (rug){
      if (rug.type == "density") {
        plot.density(xx, xlims = xlims, ylims = ylims)
      }else {
        plot.rug(xx, xlims = xlims, ylims = ylims)
      }
      
    }
  }
  invisible(n.tms)
}
