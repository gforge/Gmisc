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

# Time before optimization: 0.197 seconds
# Time after optimization: 0.029 - 0.005 seconds

lineprof({
  arrowGrob <- bezierArrowGradient(x = c(.1,.3,.6,.9),
                                   y = c(0.2, 0.2, 0.9, 0.9))
}) -> lp

lineprof::shine(lp)

# Time before optimization: 0.284 seconds
# Time after optimization: 0.011 - 0.006 seconds