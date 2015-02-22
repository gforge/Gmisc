library(magrittr)
library(grid)

grid.newpage()
# Ugly start - need to fix
tmp <- list(x=c(.1, .3, .2, .9),
            y=c(.2, .9, .1, .9))

bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw


bezierArrowGradient(x = tmp$x+.05, y = tmp$y-.15) %>%
  grid.draw

bezierArrowGradient(x = tmp$x-.05, y = tmp$y+.15, align_2_axis = FALSE) %>%
  grid.draw

grid.newpage()
# A triangle before the arrow at the bottom
bezierArrowSmpl(x = c(0.25, 0.5, 0.5, 0.75), rez=4,
                y = c(0.25, 0.5, 0.75, 0.75),
                width = unit(5, "cm"),
                arrow = list(length = 0.2,
                             base = .5)) %>%
  grid.draw

drawBzrBasics <- function (bzrFn, ...) {
  grid.newpage()
  tmp <- list(x=c(.1, .1, .9, .9),
              y=c(.8, .8, .8, .8))
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw

  tmp$x <- rev(tmp$x)
  tmp$y <- tmp$y - .1
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw


  tmp <- list(x=c(.7, .7, .7, .7),
              y=c(.6, .6, .1, .1))

  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw


  tmp <- list(x=c(.3, .3, .3, .3),
              y=c(.1, .1, .6, .6))

  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw

  tmp <- list(x=c(.4, .4, .5, .5),
              y=c(.4, .4, .5, .5))
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw

  tmp <- list(x=c(.4, .4, .5, .5) + .15,
              y=c(.5, .5, .4, .4))
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw

  tmp <- list(x=c(.5, .5, .4, .4),
              y=c(.4, .4, .5, .5) - .2)
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw

  tmp <- list(x=c(.5, .5, .4, .4) + .15,
              y=c(.5, .5, .4, .4) - .2)
  ag <- bzrFn(x = tmp$x, y = tmp$y, ...) %>%
    grid.draw
}

drawBzrBasics(bezierArrowSmpl, clr = "darkred")
drawBzrBasics(bezierArrowSmpl, clr = "purple", align_2_axis = TRUE)
drawBzrBasics(bezierArrowGradient, clr = "darkblue", grdt_clr = "yellow", grdt_clr_prop = .1, grdt_decrease_prop = .1)
