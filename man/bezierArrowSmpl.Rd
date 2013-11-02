\name{bezierArrowSmpl}
\alias{bezierArrowSmpl}
\title{A simple bezier arrow}
\usage{
  bezierArrowSmpl(x = c(0.2, 0.7, 0.3, 0.9),
    y = c(0.2, 0.2, 0.9, 0.9), width = 0.05,
    clr = "#000000", default.units = "npc",
    arrow = list(base = unit(0.1, "npc"), length = unit(0.1, "npc")),
    align_2_axis = TRUE, name = NULL, gp = gpar(),
    vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying
  x-locations of spline control points.}

  \item{y}{A numeric vector or unit object specifying
  y-locations of spline control points.}

  \item{width}{The width of the arrow, either a numeric
  single number or a unit. \strong{Note:} The arrow does
  not rely on lwd but on actual width.}

  \item{clr}{The color of the arrow.}

  \item{default.units}{A string indicating the default
  units to use if x or y are only given as numeric
  vectors.}

  \item{arrow}{This is a list with all the \strong{base}
  (width) and the desired \strong{length} for the arrow.
  \strong{Note:} This differs from the original
  \code{\link{bezierGrob}} function.}

  \item{align_2_axis}{Indicates if the arrow should be
  vertically/horizontally aligned. This is useful for
  instance if the arrow attaches to a box.}

  \item{name}{A character identifier.}

  \item{gp}{An object of class gpar, typically the output
  from a call to the function gpar.  This is basically a
  list of graphical parameter settings.}

  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  A grob of the class polygonGrob with attributes that
  correspond to the bezier points.
}
\description{
  This is an alternative to the grid packages
  \code{\link{bezierGrob}} with the advantage that it
  allows you to draw an arrow with a specific unit width.
  Note, it has only a end-arrow at this point.
}
\examples{
par_org <- par(ask=TRUE) 
# Settings
no_boxes <- 3
# Generate test setting
transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
transition_matrix[1,] <- 200*c(.5, .25, .25)
transition_matrix[2,] <- 540*c(.75, .10, .15)
transition_matrix[3,] <- 340*c(0, .2, .80)

grid.newpage()
transitionPlot(transition_matrix,  
  box_txt = c("First", "Second", "Third"),
  type_of_arrow = "simple",
  min_lwd = unit(1, "mm"),
  max_lwd = unit(6, "mm"),
  overlap_add_width = unit(1, "mm"))


# Setup proportions
box_prop <- cbind(c(1,0,0.5), c(.52,.2,.8))
# From the Set2 Colorbrewer
start_box_clr <- c("#8DA0CB", "#FC8D62")
# Darken the colors slightly
end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
  colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])
# Create a new grid
grid.newpage()
transitionPlot(transition_matrix, box_prop=box_prop,
  fill_start_box=start_box_clr, fill_end_box=end_box_clr, 
  txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
  box_txt = c("First", "Second", "Third"),
  type_of_arrow = "gradient",
  min_lwd = unit(1, "mm"),
  max_lwd = unit(10, "mm"),
  overlap_add_width = unit(1, "mm"))
par(par_org)
}
\author{
  max
}

