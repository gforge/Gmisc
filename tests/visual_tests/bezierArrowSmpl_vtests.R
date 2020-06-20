library(magrittr)
library(grid)

grid.newpage()
# Ugly start - need to fix
tmp <- list(
  x = c(.1, .3, .2, .9),
  y = c(.2, .9, .1, .9)
)

bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw()

grid.newpage()
# A triangle before the arrow at the bottom
bezierArrowSmpl(
  x = c(0.25, 0.5, 0.5, 0.75), rez = 40,
  y = c(0.25, 0.5, 0.75, 0.75),
  width = unit(2, "cm"),
  arrow = list(length = 0.2)
) %>%
  grid.draw()

grid.newpage()
ctrl_pts <- data.frame(
  x = c(.5, .9, .1, .3),
  y = c(.6, .9, 0, .7)
)
lns <- gnrlBezierPoints(ctrl_pts)
grid.lines(lns[, 1], lns[, 2])
grid.points(ctrl_pts$x, ctrl_pts$y, default.units = "npc")
bezierArrowSmpl(ctrl_pts$x, ctrl_pts$y,
  width = unit(10, "pt"),
  align_2_axis = FALSE
) %>%
  grid.draw()


grid.newpage()
ctrl_pts <- data.frame(
  x = c(.5, .9, .9, .5, 0, .3),
  y = c(.6, .9, 0, .2, .9, .7)
)
lns <- gnrlBezierPoints(ctrl_pts)
grid.lines(lns[, 1], lns[, 2])
grid.points(ctrl_pts$x, ctrl_pts$y, default.units = "npc")
bezierArrowSmpl(ctrl_pts$x, ctrl_pts$y,
  width = unit(10, "pt"),
  align_2_axis = FALSE
) %>%
  grid.draw()

drawBzrBasics <- function(bzrFn, ...) {
  grid.newpage()
  tmp <- list(
    x = c(.1, .1, .9, .9),
    y = c(.8, .8, .8, .8)
  )
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()

  tmp$x <- rev(tmp$x)
  tmp$y <- tmp$y - .1
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()


  tmp <- list(
    x = c(.7, .7, .7, .7),
    y = c(.6, .6, .1, .1)
  )

  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()


  tmp <- list(
    x = c(.3, .3, .3, .3),
    y = c(.1, .1, .6, .6)
  )

  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()

  tmp <- list(
    x = c(.4, .4, .5, .5),
    y = c(.4, .4, .5, .5)
  )
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()

  tmp <- list(
    x = c(.4, .4, .5, .5) + .15,
    y = c(.5, .5, .4, .4)
  )
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()

  tmp <- list(
    x = c(.5, .5, .4, .4),
    y = c(.4, .4, .5, .5) - .2
  )
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()

  tmp <- list(
    x = c(.5, .5, .4, .4) + .15,
    y = c(.5, .5, .4, .4) - .2
  )
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw()
}

drawBzrBasics(bezierArrowSmpl, clr = "darkred", align_2_axis = FALSE, width = .02)
drawBzrBasics(bezierArrowSmpl, clr = "purple", align_2_axis = TRUE, width = .02)
drawBzrBasics(bezierArrowGradient,
  clr = "darkblue", grdt_clr = "yellow",
  width = .02,
  grdt_prop = .8,
  grdt_clr_prop = .5,
  grdt_decrease_prop = .5,
  arrow = list(length = unit(2, "cm")),
  align_2_axis = FALSE
)
drawBzrBasics(bezierArrowGradient,
  clr = "darkblue", grdt_clr = "yellow",
  width = .02,
  grdt_prop = .8,
  grdt_clr_prop = .5,
  grdt_decrease_prop = .5,
  arrow = list(length = unit(2, "cm")),
  align_2_axis = TRUE
)