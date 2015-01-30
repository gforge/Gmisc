library(lineprof)
library(Gmisc)
library(grid)
grid.newpage()
lineprof({
  arrowGrob <- bezierArrowSmpl(x = c(.1,.3,.6,.9),
                                   y = c(0.2, 0.2, 0.9, 0.9))
}) -> lp
grid.draw(arrowGrob)

lineprof::shine(lp)
