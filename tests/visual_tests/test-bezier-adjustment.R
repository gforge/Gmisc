library(grid)
library(magrittr)
explorBezier <- function(x, y, arrow_length, np = FALSE) {
  if (np) grid.newpage()
  grid.rect(gp = gpar(col = "grey"))
  drawPoints <- function(x, y, clr = "darkgrey", font_clr = "white") {
    for (i in 1:length(x)) {
      tg <- textGrob(i,
        x = x[i], y = y[i],
        gp = gpar(col = font_clr, fontface = "bold", fontsize = 10)
      )
      dim_size <- convertY(grobHeight(tg), unitTo = "mm", valueOnly = TRUE)
      dim_size <- c(
        dim_size,
        convertX(grobWidth(tg), unitTo = "mm", valueOnly = TRUE)
      )
      grid.circle(unit(x[i], "npc"), unit(y[i], "npc"),
        r = max(dim_size) / 2 * 1.7,
        default.units = "mm",
        gp = gpar(fill = clr, col = clr, alpha = .6)
      )
      grid.draw(tg)
    }
  }

  bp <- getBezierAdj4Arrw(
    x = x,
    y = y,
    arrow_length = arrow_length,
    length_out = 100
  )

  grid.lines(attr(bp, "true_bezier")$x,
    attr(bp, "true_bezier")$y,
    gp = gpar(col = "darkgreen", fill = "darkgreen", lwd = 2),
    arrow = arrow(type = "closed", length = unit(4, "mm")),
  )

  # Mark starting points
  drawPoints(x, y, clr = "darkgreen")

  grid.lines(bp$x,
    bp$y,
    gp = gpar(col = "darkblue", fill = "darkblue", lwd = 1),
    arrow = arrow(type = "closed", length = unit(4, "mm")),
  )

  # Mark starting points
  drawPoints(attr(bp, "spline_ctrl")$x,
    attr(bp, "spline_ctrl")$y,
    clr = "darkblue"
  )
}

# Do the left to right arrows
pushBzVp <- function(x, y, arrow_length, col, row) {
  pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
  prPushMarginViewport(unit(5, "mm"))
  explorBezier(x, y, arrow_length)
  upViewport(3)
}

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
pushBzVp(
  x = c(.1, .3, .7, .9),
  y = c(.95, .1, .95, .1),
  arrow_length = .1,
  col = 1, row = 1
)
pushBzVp(
  x = c(.1, .3, 1.1, .9),
  y = c(.95, .1, 1.1, .5),
  arrow_length = .1,
  col = 1, row = 2
)
pushBzVp(
  x = c(.1, .1, .7, .9),
  y = c(.95, .1, .95, .9),
  arrow_length = .1,
  col = 2, row = 1
)
pushBzVp(
  x = c(.1, .9, .1, .9),
  y = c(.1, .1, .9, .9),
  arrow_length = .1,
  col = 2, row = 2
)

# Check different arrow
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
pushBzVp(
  x = c(.1, .3, .7, .9),
  y = c(.95, .1, .95, .1),
  arrow_length = .2,
  col = 1, row = 1
)
pushBzVp(
  x = c(.1, .3, 1.1, .9),
  y = c(.95, .1, 1.1, .5),
  arrow_length = .2,
  col = 1, row = 2
)
pushBzVp(
  x = c(.1, .1, .7, .9),
  y = c(.95, .1, .95, .9),
  arrow_length = .2,
  col = 2, row = 1
)
pushBzVp(
  x = c(.1, .9, .1, .9),
  y = c(.1, .1, .9, .9),
  arrow_length = .2,
  col = 2, row = 2
)

# From right to left
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
pushBzVp(
  x = rev(c(.1, .3, .7, .9)),
  y = rev(c(.95, .1, .95, .1)),
  arrow_length = .2,
  col = 1, row = 1
)
pushBzVp(
  x = rev(c(.1, .3, 1.1, .9)),
  y = rev(c(.95, .1, 1.1, .5)),
  arrow_length = .2,
  col = 1, row = 2
)
pushBzVp(
  x = rev(c(.1, .1, .7, .9)),
  y = rev(c(.95, .1, .95, .9)),
  arrow_length = .2,
  col = 2, row = 1
)
pushBzVp(
  x = rev(c(.1, .9, .1, .9)),
  y = rev(c(.1, .1, .9, .9)),
  arrow_length = .2,
  col = 2, row = 2
)


# Really complex elements
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
pushBzVp(
  x = c(.1, .3, .1, .1, .7, .9),
  y = c(.95, .1, .1, .9, .95, .1),
  arrow_length = .2,
  col = 1, row = 1
)
pushBzVp(
  x = c(.9, .3, 1.1, .9),
  y = c(.95, .1, 1.1, .5),
  arrow_length = .2,
  col = 1, row = 2
)
pushBzVp(
  x = c(.05, .1, .7, .9, .8, .1),
  y = c(.95, .1, .9, .9, .1, .1),
  arrow_length = .2,
  col = 2, row = 1
)
pushBzVp(
  x = c(.1, 2, -1, .9),
  y = c(.1, .1, .9, .9),
  arrow_length = .2,
  col = 2, row = 2
)