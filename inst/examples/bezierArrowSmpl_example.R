par_org <- par(ask = TRUE)
library(grid)
pg <- bezierArrowSmpl(
  x = c(.2, .5, .5, .8),
  y = c(.2, .2, .8, .8),
  width = unit(6, "mm"),
  arrow = list(
    base = unit(.1, "npc"),
    length = unit(.1, "npc")
  )
)

plot.new()
grid.draw(pg)

plot.new()
pg <- bezierArrowSmpl(
  x = c(.2, .2, .8, .3),
  y = c(.3, .9, .8, .2)
)
grid.draw(pg)

plot.new()
pg <- bezierArrowSmpl(
  x = c(.3, .7, -.1, .25),
  y = c(.5, .1, .1, .5),
  align_2_axis = FALSE
)
grid.draw(pg)
par(par_org)