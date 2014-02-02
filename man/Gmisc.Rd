\docType{package}
\name{Gmisc}
\alias{Gmisc}
\alias{Gmisc-package}
\title{A package for plotting, tables, and basic outputs.}
\description{
This is a collection of functions that I've found useful in
my research. The package is inspired by Frank Harrel's
Hmisc package. The main focus is on tables and plots.
}
\details{
For tables you'll find the convenient htmlTable that I have
used for advanced table layout. A major focus has been to
have it compatible with LibreOffice (you can copy/past from
there into word) as I generally want to be able to send my
documents to a journal in .doc/.docx format.

The plots are forestplot2 and transitionPlot. The
forestplot is a more general version of the original
\code{\link[rmeta]{forestplot}}() function aimed at using
the forestplot for more than just meta-analyses.

The transition plot is for descriptive purposes in order to
illustrate the size of change between one state and the
next. This is basically a graph of \code{table(var1,
var2)}. It uses the \code{\link{bezierArrowSmpl}}
alternatives for showing off elegant arrows that should
nicely bend into each box.

Additionally there is the
\code{\link{getSvdMostInfluential}} function that tries to
illustrate and use the Svd in order to select the ones
having most influence on the V-matrix.

Apart from those you will find a few handy functions for
formatting p-values, generating the basic stats for your
table 1, etc.
}

