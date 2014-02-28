par_org <- par(ask=TRUE) 
library(grid)
gl <- bezierArrowGradient(x=c(.2, .5, .5, .8),
                          y=c(.2, .2, .8, .8),
                          width=unit(10, "mm"),
                          arrow = list(base=unit(.1, "npc"),
                                       length = unit(.1, "npc")),
                          grdt_type = "triangle",
                          grdt_decrease_prop = .3,
                          grdt_start_prop = .5,
                          grdt_clr_prop = .7,
                          grdt_line_width = unit(6, "pt"),
                          grdt_clr = "#2F4F2F")




plot.new()
grid.draw(gl)

pg <- bezierArrowGradient(x=c(.3,.7,-.1,.25),
                          y=c(.5,.1,.1,.5),
                          grdt_type = "triangle",
                          grdt_decrease_prop = .3,
                          grdt_start_prop = .5,
                          grdt_clr_prop = .7,
                          grdt_line_width = unit(6, "pt"),
                          grdt_clr = "#2F4F2F",
                          align_2_axis=FALSE)
plot.new()
grid.draw(pg)

par(par_org) 
