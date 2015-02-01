library(grid)
library(magrittr)
explorBezier <- function (x, y) {
  grid.newpage()
  arrow_len = .2
  spl_len <- sqrt((tail(x, 1) - tail(x, 2)[1])^2 +
                    (tail(y, 1) - tail(y, 2)[1])^2)
  if (spl_len < arrow_len * 2){
    mult <- 1
  }else{
    mult <- spl_len/(arrow_len * 2)
  }
  for (i in 1:length(x)){
    grid.circle(x[i], y[i], r = .01, gp=gpar(fill="grey", col="grey"))
  }
  l <- gnrlBezierPoints(x = x, y = y)
  grid.lines(l$x, l$y, gp = gpar(col = "darkgreen", lwd=2), arrow=arrow())
  
  l$distance <- 
    with(l, 
         sqrt((x - tail(x, 1))^2 + (y - tail(y, 1))^2))
  
  cut_point <- which.min(abs(l$distance - arrow_len))
  
  grid.circle(l$x[cut_point], l$y[cut_point], r = .01, gp=gpar(fill="black"))
  
  dx <- tail(x, 1) - l$x[cut_point]
  dy <- tail(y, 1) - l$y[cut_point]
  if (dx > dy){
    if (dx > 0){
      retain <- x < l$x[cut_point]
    }else if (dx < 0){
      retain <- x > l$x[cut_point]
    }
  }else{
    if (dy > 0){
      retain <- y > l$y[cut_point]
    }else if (dx < 0){
      retain <- y < l$y[cut_point]
    }
  }

  x <- x[retain]
  y <- y[retain]
  
  x[length(x)] <- l$x[cut_point] - dx*mult
  y[length(y)] <- l$y[cut_point] - dy*mult
  if (length(x) >= 3){
    x[length(x) - 1] <-  x[length(x) - 1] - 
      (x[length(x) - 1] - x[length(x) - 2])/5
    y[length(y) - 1] <- y[length(y) - 1] - 
      (y[length(y) - 1] - y[length(y) - 2])/5
  }
  
  x <- c(x, l$x[cut_point])
  y <- c(y, l$y[cut_point])
  
  for (i in 1:length(x)){
    grid.circle(x[i], y[i], r = .01, gp=gpar(fill="red", col="red"))
    grid.text(i, x[i] + .02, y[i] + .02)
  }
  
  l <- gnrlBezierPoints(x = x, y = y)
  grid.lines(l$x, l$y, gp = gpar(col = "orange", lwd=2), arrow=arrow())
}


explorBezier(c(.1, .3, .7, .9),
             c(.95, .1, .95, .1))
explorBezier(c(.1, .3, .7, .9),
             c(.95, .1, .95, .5))
explorBezier(c(.1, .3, .7, .9),
             c(.95, .1, .95, .9))

explorBezier(rev(c(.1, .3, .7, .9)),
             c(.95, .1, .95, .9))
