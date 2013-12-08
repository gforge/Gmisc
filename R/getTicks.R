#' Ticks for plot axis
#' 
#' Gets the ticks in a formatted version. This is since I'm not always
#' that fond of just pretty(1:10/5). In exponential form the ticks are
#' determined from the 2-base, meaning that you get an intuitive feeling
#' for when the value is doubled.
#' 
#' @param low lower bound, can be a single number or a vector
#' @param high upper bound - optional, you can just have all data in the low variable 
#' @param clip if the ci are clipped
#' @param exp If the value should be in exponential form (default)
#' @param digits Number of digits - used in exp mode
#' @return Returns a vector with the ticks appropriate
#' 
#' @example examples/getTicks_example.R
#' 
#' @export 
#' 
#' @author max
getTicks <- function(low, 
  high = low, 
  clip = c(-Inf, Inf),
  exp = FALSE,
  digits = 0){
  
  # Get the right ticks
  lowest <- max(min(low, na.rm=TRUE), clip[1])
  bottom <- floor(lowest*2)/2
  if (bottom == 0 & exp){
    bottom <- 2^(round(log2(lowest)*2)/2)
  }
  
  highest <- min(max(high, na.rm=TRUE), clip[2])
  roof <- ceiling(highest*2)/2
  if (exp == FALSE){
    resolution <- roof-bottom
    if (resolution > 6){
      bottom <- floor(bottom)
      roof <- ceiling(roof)
      xticks <- seq(from=bottom, to=roof, by=1)
    }else if(highest - lowest < .5){
      bottom <- floor(lowest*2*5)/2/5
      roof <- ceiling(highest*2*5)/2/5
      xticks <- seq(from=bottom, to=roof, by=.5/10)
    }else{
      bottom <- floor(bottom*2)/2
      roof <- ceiling(roof*2)/2
      xticks <- seq(from=bottom, to=roof, by=.5)
    }
  }else{
    xticks <- c()
    increase_by <- 1
    if (abs(bottom-roof) < 3){
      # A regular log. scale wont really work :-(
      #return(seq(from=bottom, to=roof, by=0.5))
      increase_by <- 0.5
    }
    
    if (bottom < 1)
      xticks <- append(xticks, round(rev(2^seq(from=0, to=log2(bottom), by=-increase_by)), digits+1))
    
    if (roof > 1)
      xticks <- unique(append(xticks, round(2^seq(from=0, to=log2(roof), by=increase_by), digits)))
  }
  
  # Choose the one thats numberwise the closest
  if (abs(lowest-xticks[1]) >= abs(lowest-xticks[2]))
    xticks <- xticks[-1]
  
  if (abs(highest-xticks[length(xticks)]) >= abs(highest-xticks[length(xticks)-1]))
    xticks <- xticks[-length(xticks)]
  
  return(xticks)
}
