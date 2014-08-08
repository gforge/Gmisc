#' SI or English formatting of an integer
#'
#' English uses ',' between every 3 numbers while the
#' SI format recommends a ' ' if x > 10^4. The scientific
#' form 10e+? is furthermore avoided.
#'
#' @param x The integer variable
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator.
#' @param html If the format is used in html context
#'  then the space should be a non-breaking space, \code{&nbsp;}
#' @param ... Passed to \code{\link[base]{format}}
#' @return \code{string}
#'
#' @examples
#' outputInt(123456)
#'
#' @export
outputInt <- function(x, language = "en", html = TRUE, ...){
  if (length(x) > 1){
    ret <- sapply(x, outputInt, language=language, html=TRUE)
    if (is.matrix(x)){
      ret <- matrix(ret, nrow=nrow(x))
      rownames(ret) <- rownames(x)
      colnames(ret) <- colnames(x)
    }
    return(ret)
  }
  if (abs(x - round(x)) > .Machine$double.eps^0.5 &&
        !"nsmall" %in% names(list(...)))
    warning("The function can only be served integers, '", x, "' is not an integer.",
            " There will be issues with decimals being lost if you don't add the nsmall parameter.")

  if (language == "en")
    return(format(x, big.mark=",", scientific=FALSE, ...))

  if(x >= 10^4)
    return(format(x,
                  big.mark=ifelse(html, "&nbsp;", " "),
                  scientific=FALSE, ...))

  return(format(x, scientific=FALSE, ...))
}
