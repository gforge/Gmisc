#' Gets the x-ticks in a formatted version. This is since I'm not always
#' that fond of just pretty(1:10/5). In exponential form the ticks are
#' determined from the 2-base, meaning that you get an intuitive feeling
#' for when the value is doubled.
#' 
#' @param low lower bound 
#' @param high upper bound
#' @param clip if the ci are clipped
#' @param exp If the value should be in exponential form (default)
#' @return Returns a vector with the ticks apropriate
#' 
#' @example examples/getXTicks_example.R
#' @export 
#' 
#' @author max
getXTicks <- function(low, 
  high, 
  clip = c(-Inf, Inf),
  exp = FALSE){
  # Get the right ticks
  lowest <- max(min(low, na.rm=TRUE), clip[1])
  highest <- min(max(high, na.rm=TRUE), clip[2])
  if (exp == FALSE){
    resolution <- highest-lowest
    if (resolution > 6){
      lowest <- floor(lowest)
      highest <- ceiling(highest)
      xticks <- seq(from=lowest, to=highest, by=1)
    }else{
      lowest <- floor(lowest*2)/2
      highest <- ceiling(highest*2)/2
      xticks <- seq(from=lowest, to=highest, by=.5)
    }
  }else{
    xticks <- c()
    if (lowest < 1)
      xticks <- append(xticks, rev(2^seq(from=0, to=log2(min(lowest)), by=-1)))
    
    if (highest > 1)
      xticks <- unique(append(xticks, 2^seq(from=0, to=log2(max(highest)), by=1)))
  }
  
  return(xticks)
}