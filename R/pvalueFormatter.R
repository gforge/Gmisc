
#' Formats the p-values
#' 
#' Gets formatted p-values. For instance
#' you often want 0.1234 to be 0.12 while also
#' having two values up until a limit,
#' i.e. 0.01234 should be 0.012 while
#' 0.001234 should be 0.001. Furthermore you
#' want to have < 0.001 as it becomes ridiculous
#' to report anything below that value.
#'  
#' @param pvalues The p-values
#' @param two_dec.limit The limit for showing two decimals 
#' @param sig.limit The significance limit for < sign
#' @param html If the less than sign should be < or &lt;
#'  as needed for html output.
#' @return vector 
#'
#' @example pvalueFormatter(c(0.10234,0.010234, 0.0010234, 0.000010234)) 
#' @author Max
#' @export
pvalueFormatter <- function(pvalues, two_dec.limit = 10^-2, 
    sig.limit = 10^-4, html=TRUE){
  if (is.logical(html))
    html <- ifelse(html, "&lt; ", "< ")
  sapply(pvalues, function(x, two_dec.limit, sig.limit, lt_sign){
      if (is.na(as.numeric(x))){
        warning("The value: '", x, "' is non-numeric and pvalueFormatter",
                " can't therfore handle it")
        return (x)
      }
      
      if (x < sig.limit)
        return(sprintf("%s%s", lt_sign, format(sig.limit, scientific=FALSE))) 
  
      if (x > two_dec.limit)
        return(format(x, 
                      digits=2, 
                      nsmall=-floor(log10(x))+1))
      
      return(format(x, digits=1, scientific=FALSE))
    }, sig.limit=sig.limit, 
    two_dec.limit = two_dec.limit, 
    lt_sign = html)
}