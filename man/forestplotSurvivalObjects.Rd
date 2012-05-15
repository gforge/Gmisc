\name{forestplotSurvivalObjects}
\alias{forestplotSurvivalObjects}
\title{Plot survival objects}
\usage{
  forestplotSurvivalObjects(plot_title, survival.fit,
    increase.line_height = 1.2, lwd.forestlines = 2,
    clip = c(-Inf, Inf), lwd.xaxis = 2, lwd.zero = 2,
    col.zero = "lightgray", order.output = "P-value",
    skip.variables = NA, legend.title = "Type of analysis",
    legend.content = NA, legend.position = "topright",
    legend.inset = 0.05, add.empty.rows = NA,
    box.default.size = NA, cex = 1, bg = "transparent",
    ...)
}
\arguments{
  \item{plot_title}{The title of the plot}

  \item{survival.fit}{A survival regression model}

  \item{increase.line_height}{If you want to space the
  lines further apart}

  \item{lwd.forestlines}{The width of the forestplots
  confidence interval}

  \item{clip}{Lower and upper limits for clipping
  confidence intervals to arrows}

  \item{lwd.xaxis}{lwd for the xaxis}

  \item{lwd.zero}{lwd for the vertical line that gives the
  no-effect line}

  \item{col.zero}{The color of the zero effect line (HR=1)}

  \item{order.output}{The order in which to show the
  output}

  \item{skip.variables}{Which variables to use}

  \item{legend.title}{The title of the legend}

  \item{legend.content}{Content of the legend}

  \item{legend.position}{Position of the legend}

  \item{legend.inset}{The inset of the legend}

  \item{add.empty.rows}{Add empty rows}

  \item{box.default.size}{The size of the boxes indicating
  the estimate in the forestplot}

  \item{cex}{Font size expansion}

  \item{bg}{Background color}

  \item{...}{Passed to forestplot2()}
}
\value{
  Does not return anything

  TODO: Should probably look into this function and clean
  it up a little
}
\description{
  Plot survival objects
}
\author{
  max
}

