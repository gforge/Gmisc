#' Collection of functions for plotting relations, generating tables, and more.
#'
#' This is a collection of functions that I've found useful in my research.
#' The package is inspired by Frank Harrell's \pkg{Hmisc} package. The main focus
#' is on tables, plots, and \pkg{knitr}-integration.
#'
#' @section Awesome tables:
#'
#' The \code{\link{getDescriptionStatsBy}} is a straight forward function that
#' aims at helping you to generate descriptive table stratified by different
#' variables. In other words, the function returns everything you need for generating
#' a \emph{Table 1} ready for publication. This function is accompanied by the
#' \code{\link{describeMean}}, \code{\link{describeMedian}}, \code{\link{describeProp}},
#' and \code{\link{describeFactors}} functions.
#'
#' The \code{\link{mergeDesc}} allows you to merge a set of outputs \code{\link{getDescriptionStatsBy}}
#' into a \code{\link[htmlTable]{htmlTable}} with the rgroup arguments automatically
#' generated, see \code{vignette("descriptives", package = "Gmisc")} for a
#' detailed workflow description.
#'
#' @section Some fancy plots:
#'
#' The transition plot function, \code{\link{transitionPlot}}, is for
#' descriptive purposes. It tries to illustrate the size of change
#' between one state and the next, i.e. a transition. This is
#' basically a graph of based upon \code{table(var1, var2)}.
#'
#' The \href{https://en.wikipedia.org/wiki/Singular_value_decomposition}{Singular value decomposition}
#' is a common method for reducing the number of variables. Unfortunately
#' this compression can reduce the interpretability of the model. The \code{\link{getSvdMostInfluential}}
#' function tries to remedy that by identifying the most influential
#' elements from the \code{V}-matrix.
#'
#' @section Other stuff:
#'
#' The \code{\link{insertRowAndKeepAttr}} simply adds a row while remembering
#' all the attributes previously set by using the \code{\link{copyAllNewAttributes}}.
#' The \code{\link{mergeLists}} tries to merge lists that do not have identical
#' elements.
#'
#' @name Gmisc-package
#' @docType package
NULL
