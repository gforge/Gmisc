library(grid)
library(magrittr)
printXY <- function(x, y, x_span, y_span, x_min, y_min,
                    new_page = TRUE, col = "black", voffset = 1, text = TRUE) {
  if (missing(x_span)) {
    x_span <- (max(x) - min(x))
  }
  if (missing(y_span)) {
    y_span <- (max(y) - min(y))
  }
  if (missing(x_min)) {
    x_min <- min(x)
  }
  if (missing(y_min)) {
    y_min <- min(y)
  }

  x <- (x - x_min) * .9 / x_span + .05
  y <- (y - y_min) * .9 / y_span + .05
  if (new_page) grid.newpage()
  grid.lines(x, y, gp = gpar(col = col))
  for (i in 1:length(x)) {
    col <- colorRampPalette(c("lightblue", "red"))(length(x))[i]
    grid.points(x = x, y = y, default.units = "npc", gp = gpar(col = col))
    if (text) grid.text(i, x = x[i], y = y[i] + .05 * voffset, default.units = "npc")
  }

  return(list(
    x_span = x_span,
    y_span = y_span,
    x_min = x_min,
    y_min = y_min
  ))
}
x <- c(
  0.1, 0.2, 0.3, 0.4,
  .2, .1, .3, .4,
  0.5, 0.6, 0.7, 0.8,
  .6, .5, .7,
  .8, .9
)
y <- c(
  0.1, 0.2, 0.3, 0.4,
  .4, .3, .2, .2,
  0.5, 0.6, 0.8, 0.9,
  .9, .7, .65,
  .6, .5
)

range <- printXY(x, y)


# Output from visual studio test
x <- c(
  0.1, 0.2, 0.22, 0.23, 0.25,
  0.27, 0.3, 0.4, 0.5, 0.6,
  0.62, 0.63, 0.65, 0.67, 0.7,
  0.8, 0.9
)

y <- c(
  0.1, 0.2, 0.22, 0.22, 0.22,
  0.22, 0.2, 0.2, 0.5, 0.6,
  0.63, 0.65, 0.66, 0.66, 0.65,
  0.6, 0.5
)

printXY(x, y,
  x_span = range$x_span,
  y_span = range$y_span,
  x_min = range$x_min,
  y_min = range$y_min,
  new_page = FALSE, col = "red", voffset = -1
)

############
# New test #
############

x <- c(0.28, 0.306, 0.33, 0.353, 0.374, 0.393, 0.41, 0.426, 0.441, 0.454, 0.466, 0.477, 0.487, 0.496, 0.503, 0.51, 0.516, 0.521, 0.526, 0.53, 0.532, 0.534, 0.536, 0.535, 0.534, 0.53, 0.523, 0.516, 0.509, 0.505, 0.504, 0.505, 0.507, 0.512, 0.518, 0.525, 0.534, 0.544, 0.556, 0.568, 0.604, 0.75)
y <- c(0.056, 0.068, 0.081, 0.093, 0.105, 0.117, 0.129, 0.14, 0.151, 0.161, 0.171, 0.181, 0.191, 0.2, 0.209, 0.217, 0.225, 0.232, 0.239, 0.245, 0.25, 0.254, 0.256, 0.256, 0.254, 0.249, 0.244, 0.239, 0.236, 0.235, 0.234, 0.235, 0.235, 0.236, 0.237, 0.238, 0.239, 0.241, 0.243, 0.245, 0.068, 0.352)
printXY(x[19:36], y[19:36])