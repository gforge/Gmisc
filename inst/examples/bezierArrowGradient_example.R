par_org <- par(ask = TRUE)
library(grid)
plot.new()
gl <- bezierArrowGradient(
  x = c(.2, .5, .5, .8),
  y = c(.2, .2, .8, .8),
  width = unit(10, "mm"),
  arrow = list(
    base = unit(.1, "npc"),
    length = unit(.1, "npc")
  ),
  grdt_type = "triangle",
  grdt_prop = .5,
  grdt_decrease_prop = .5,
  grdt_clr_prop = .7,
  grdt_line_width = unit(.7, "cm"),
  grdt_clr = "#2F4F2F"
)
grid.draw(gl)

plot.new()
pg <- bezierArrowGradient(
  x = c(.3, .7, -.1, .1),
  y = c(.5, .1, -1, .6),
  width = unit(10, "pt"),
  grdt_type = "triangle",
  grdt_prop = .9,
  grdt_decrease_prop = .3,
  grdt_clr_prop = .7,
  grdt_line_width = unit(6, "pt"),
  grdt_clr = "#2F4F2F",
  align_2_axis = FALSE
)
grid.draw(pg)

par(par_org)