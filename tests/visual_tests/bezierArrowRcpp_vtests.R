library(grid)
cnvrtAndOffset <- function(x, y, offset = 3) {
  org_unit <- attr(x, "unit")
  if (is.null(org_unit)) {
    org_unit <- "mm"
  }

  x <- convertX(x, unitTo = "mm", valueOnly = TRUE)
  y <- convertY(y, unitTo = "mm", valueOnly = TRUE)

  a <- calculateLinesAndArrow(x, y, offset = offset)

  lapply(a, function(side) {
    list(
      x = convertX(unit(side$x, "mm"), unitTo = org_unit),
      y = convertY(unit(side$y, "mm"), unitTo = org_unit)
    )
  })
}

getAngle <- function(x, y) {
  x <- convertX(x, unitTo = "mm", valueOnly = TRUE)
  y <- convertY(y, unitTo = "mm", valueOnly = TRUE)
  atan((y[-1] - y[-length(y)]) / (x[-1] - x[-length(x)]))
}
grid.newpage()
x <- unit(c(.1, .2, .5, .4, .2) * 10, units = "cm")
y <- unit(c(.1, .1, .5, .7, .6) * 10, units = "cm")
grid.lines(x = x, y = y, arrow = arrow(ends = "last"), gp = gpar(lwd = 2))

plotLines <- function(x, y) {
  a <- cnvrtAndOffset(x, y)
  with(
    a$right,
    grid.lines(x,
      y = y,
      gp = gpar(col = "darkred"), arrow = arrow(ends = "last")
    )
  )

  with(
    a$left,
    grid.lines(x,
      y = y,
      gp = gpar(col = "darkgreen"), arrow = arrow(ends = "last")
    )
  )
}

bp <- bezierPoints(bezierGrob(
  x = c(0.1, 0.5, 1.5, 0.2),
  y = c(0.5, 2, -1, 0.9)
))
grid.lines(bp$x, bp$y, gp = gpar(lwd = 2), arrow = arrow(ends = "last"))

scaleUnit <- function(u, scale) {
  org_unit <- attr(u, "unit")
  unit(convertUnit(u, unitTo = org_unit, valueOnly = TRUE) / scale, org_unit)
}
bp <- bezierPoints(bezierGrob(
  x = c(0.1, 0.5, 1.5, 0.2) * 100,
  y = c(0.5, 2, -1, 0.9) * 100
))

bp <- lapply(bp, function(x) scaleUnit(x, 100))

plotLines(bp$x, bp$y)

# Test polygon
grid.newpage()
a <- cnvrtAndOffset(bp$x, bp$y)
grid.polygon(x = unit.c(a$right$x, rev(a$left$x)), y = unit.c(a$right$y, rev(a$left$y)), gp = gpar(fill = "#550000"))


# Test angles by doing a "clock"
grid.newpage()

plotPair <- function(x, y) {
  if (!is.unit(x)) {
    x <- unit(x, units = "npc")
    y <- unit(y, units = "npc")
  }
  grid.lines(x = x, y = y, arrow = arrow(ends = "last"), gp = gpar(lwd = 2))
  plotLines(x, y)
}

plotPair(c(.6, .8), c(.5, .5))
plotPair(c(.4, .2), c(.5, .5))
plotPair(c(.5, .5), c(.4, .1))
plotPair(c(.5, .5), c(.6, .9))


plotPair(
  unit(c(0, 2), units = "cm") + unit(.6, "npc"),
  unit(c(0, 2), units = "cm") + unit(.6, "npc")
)
plotPair(
  unit(c(0, -2), units = "cm") + unit(.4, "npc"),
  unit(c(0, -2), units = "cm") + unit(.4, "npc")
)
plotPair(
  unit(c(0, 2), units = "cm") + unit(.6, "npc"),
  unit(c(0, -2), units = "cm") + unit(.4, "npc")
)
plotPair(
  unit(c(0, -2), units = "cm") + unit(.4, "npc"),
  unit(c(0, 2), units = "cm") + unit(.6, "npc")
)