\name{termplot2}
\alias{termplot2}
\title{A more advanced version of the original termplot funciton.}
\usage{
  termplot2(model, data = NULL,
    envir = environment(formula(model)),
    partial.resid = FALSE, rug = FALSE, terms = NULL,
    se = FALSE, xlabs = NULL, ylabs = NULL, main = NULL,
    col.term = 2, lwd.term = 1.5, col.se = "orange",
    lty.se = 2, lwd.se = 1, col.res = "gray", cex = 1,
    pch = par("pch"), col.smth = "darkred", lty.smth = 2,
    span.smth = 2/3,
    ask = dev.interactive() && nb.fig < n.tms,
    use.factor.levels = TRUE, smooth = NULL,
    ylim = "common", rug.type = "rug", yscale = "regular",
    col.dens = "#80808033", se.type = "line",
    density.proportion = 0.1, ...)
}
\arguments{
  \item{model}{fitted model object}

  \item{data}{data frame in which variables in model can be
  found}

  \item{envir}{environment in which variables in model can
  be found}

  \item{partial.resid}{logical; should partial residuals be
  plotted?}

  \item{rug}{add rugplots (jittered 1-d histograms) to the
  axes?}

  \item{terms}{which terms to plot (default NULL means all
  terms)}

  \item{se}{plot pointwise standard errors?}

  \item{xlabs}{vector of labels for the x axes}

  \item{ylabs}{vector of labels for the y axes}

  \item{main}{logical, or vector of main titles; if TRUE,
  the model's call is taken as main title, NULL or FALSE
  mean no titles}

  \item{col.term}{color for the ‘term curve’, see lines.}

  \item{lwd.term}{line width for the ‘term curve’, see
  lines.}

  \item{col.se}{colorfor the ‘twice-standard-error curve’
  when se = TRUE}

  \item{lty.se}{line type for the ‘twice-standard-error
  curve’ when se = TRUE}

  \item{lwd.se}{line width for the ‘twice-standard-error
  curve’ when se = TRUE}

  \item{col.res}{color when partial.resid = TRUE, see
  points.}

  \item{cex}{plotting character expansion when
  partial.resid = TRUE, see points.}

  \item{pch}{type for partial residuals, when partial.resid
  = TRUE, see points.}

  \item{col.smth}{Passed to smooth}

  \item{lty.smth}{Passed to smooth}

  \item{span.smth}{Passed to smooth}

  \item{ask}{logical; if TRUE, the user is asked before
  each plot, see par(ask=.).}

  \item{use.factor.levels}{hould x-axis ticks use factor
  levels or numbers for factor terms?}

  \item{smooth}{NULL or a function with the same arguments
  as panel.smooth to draw a smooth through the partial
  residuals for non-factor terms}

  \item{ylim}{an optional range for the y axis, or "common"
  when a range sufficient for all the plot will be
  computed, or "free" when limits are computed for each
  plot.}

  \item{rug.type}{if "density" rug or regular "rug" (i.e.
  jitter) should be used}

  \item{yscale}{if the scale should be "exponential" or
  "regular"}

  \item{col.dens}{the color of the density. Only used when
  rug.type = "density"}

  \item{se.type}{if the confidence interval should be two
  lines or a polygon}

  \item{density.proportion}{the height of the density plot}

  \item{...}{passed on to plot() function}
}
\value{
  number of plotted terms
}
\description{
  This function can plot polygons instead of lines for
  confidence intervals, density curve at the x-axis instead
  of just a jitter rug, identifies and warns for
  interaction terms. It also allows for rescaling to an
  exponential y-axis.
}
\author{
  max
}

