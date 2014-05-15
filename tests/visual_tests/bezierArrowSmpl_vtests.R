# Ugly start - need to fix
tmp <- list(x=c(.1,.3,.2,.9),
            y=c(0.2, 0.9, 0.1, 0.9))

ag <- bezierArrowSmpl(x = tmp$x, y = tmp$y)

grid.newpage()
grid.draw(ag)
grid.bezier(x = tmp$x,
            y = tmp$y, gp=gpar(col="red"))

