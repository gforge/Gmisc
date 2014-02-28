\name{prTpPlotBoxes}
\alias{prTpPlotBoxes}
\title{Plot boxes and arrows}
\usage{
prTpPlotBoxes(overlap_order, transition_flow, no_boxes, box_width, tot_spacing,
  txt, cex, prop_start_sizes, prop_end_sizes, box_prop,
  lwd_prop_total = lwd_prop_total, fill_start_clr, fill_end_clr,
  txt_start_clr, txt_end_clr, lwd = 2, line_col = "#000000", min_lwd,
  max_lwd, overlap_add_width, overlap_bg_clr, type_of_arrow, abs_arrow_width,
  arrow_clr, plot_arrows = TRUE, proportion = FALSE)
}
\arguments{
  \item{overlap_order}{The order in which the boxes are
  plotted}

  \item{transition_flow}{The transition flow matrix}

  \item{no_boxes}{Number of boxes to plot}

  \item{box_width}{The width of the boxes}

  \item{tot_spacing}{The total spacing between the boxes}

  \item{txt}{The text}

  \item{cex}{The font size}

  \item{prop_start_sizes}{The proportion of the different
  boxes to the left}

  \item{prop_end_sizes}{The proportion of the different
  boxes to the right}

  \item{box_prop}{The box proportions if any}

  \item{fill_start_clr}{The color of the boxes to the left}

  \item{fill_end_clr}{The color of the boxes to the right}

  \item{lwd}{The line width}

  \item{line_col}{The color of the line}

  \item{overlap_add_width}{The additional width the the
  arrow for the overlap}

  \item{overlap_bg_clr}{The color of the overlap}

  \item{type_of_arrow}{The type of arrow to be used}

  \item{abs_arrow_width}{The absolute width of the arrow}

  \item{plot_arrows}{If we are plotting shadow boxes then
  arrows should not be plotted and this should be set to
  \code{FALSE}}

  \item{proportion}{It there is a proportion}
}
\value{
\code{NuLL}
}
\description{
Outputs all the boxes and arrow
}
\author{
Max
}

