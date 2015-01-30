library(magrittr)
library(grid)

# Ugly start - need to fix
tmp <- list(x=c(.1, .3, .2, .9),
            y=c(.2, .9, .1, .9))

bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw


bezierArrowGradient(x = tmp$x, y = tmp$y-.1) %>%
  grid.draw

grid.newpage()
tmp <- list(x=c(.1, .1, .9, .9),
            y=c(.8, .8, .8, .8))
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw

tmp$x <- rev(tmp$x)
tmp$y <- tmp$y - .1
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw


tmp <- list(x=c(.7, .7, .7, .7),
            y=c(.6, .6, .1, .1))

ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw


tmp <- list(x=c(.3, .3, .3, .3),
            y=c(.1, .1, .6, .6))

ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw

tmp <- list(x=c(.4, .4, .5, .5),
            y=c(.4, .4, .5, .5))
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw

tmp <- list(x=c(.4, .4, .5, .5) + .15,
            y=c(.5, .5, .4, .4))
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw

tmp <- list(x=c(.5, .5, .4, .4),
            y=c(.4, .4, .5, .5) - .2)
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw

tmp <- list(x=c(.5, .5, .4, .4) + .15,
            y=c(.5, .5, .4, .4) - .2)
ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y) %>%
  grid.draw
