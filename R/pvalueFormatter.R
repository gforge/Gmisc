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
#' @param two_dec_lim The limit for showing two decimals. E.g.
#'  the p-value may be 0.056 and we may want to keep the two decimals in order
#'  to emphasize the proximity to the all-mighty 0.05 p-value and set this to
#'  \eqn{10^-2}. This allows that a value of 0.0056 is rounded to 0.006 and this
#'  makes intuitive sense as the 0.0056 level as this is well below
#'  the 0.05 value and thus not as interesting to know the exact proximity to
#'  0.05. \emph{Disclaimer:} The 0.05-limit is really silly and debated, unfortunately
#'  it remains a standard and this package tries to adapt to the current standards in order
#'  to limit publication associated issues.
#' @param sig_lim The significance limit for < sign
#' @param html If the less than sign should be < or &lt;
#'  as needed for html output.
#' @param ... Currently only used for generating warnings of deprecated call
#'  parameters.
#' @return vector
#'
#' @examples pvalueFormatter(c(0.10234,0.010234, 0.0010234, 0.000010234))
#' @export
pvalueFormatter <- function(pvalues,
                            two_dec_lim = 10^-2,
                            sig_lim = 10^-4,
                            html=TRUE, ...){

  if ("sig.limit" %in% names(list(...))){
    sig_lim <- sig.limit
    warning("Deprecated: sig.limit argument is now sig_lim as of ver. 1.0")
  }

  if ("two_dec.limit" %in% names(list(...))){
    two_dec_lim <- two_dec.limit
    warning("Deprecated: two_dec.limit argument is now two_dec_lim as of ver. 1.0")
  }

  if (is.logical(html))
    html <- ifelse(html, "&lt; ", "< ")
  sapply(pvalues, function(x, two_dec_lim, sig_lim, lt_sign){
      if (is.na(as.numeric(x))){
        warning("The value: '", x, "' is non-numeric and pvalueFormatter",
                " can't therfore handle it")
        return (x)
      }

      if (x < sig_lim)
        return(sprintf("%s%s", lt_sign, format(sig_lim, scientific=FALSE)))

      if (x > two_dec_lim)
        return(format(x,
                      digits=2,
                      nsmall=-floor(log10(x))+1))

      return(format(x, digits=1, scientific=FALSE))
    }, sig_lim=sig_lim,
    two_dec_lim = two_dec_lim,
    lt_sign = html)
}