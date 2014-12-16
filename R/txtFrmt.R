#' A helper function for html/LaTeX line formatting
#'
#' This function helps you to do a multiline
#' table header in both html and in LaTeX. In
#' html this isn't that tricky, you just use
#' the <br /> command but in LaTeX I often find
#' myself writing vbox/hbox stuff and therefore
#' I've created this simple helper function
#'
#' @param ... The lines that you want to be joined
#' @param html If HTML compatible output should be used. If \code{FALSE}
#'  it outputs LaTeX formatting
#' @return string
#'
#' @examples
#' splitLines4Table("hello", "world")
#' splitLines4Table("hello", "world", html=TRUE)
#' splitLines4Table("hello", "world", list("A list", "is OK"))
#'
#'
#' @export
splitLines4Table <- function(..., html = TRUE){
  strings <- c()
  for (i in list(...)){
    if (is.list(i)){
      for(c in i)
        strings <- append(strings, i)
    }else{
      strings <- append(strings, i)
    }
    
  }
  if (length(strings) < 2)
    return(strings)
  
  ret <- ifelse(html, "", "\\vbox{")
  first <- TRUE
  for (line in strings){
    line <- as.character(line)
    if (first)
      ret <- paste0(ret, ifelse(html, line, sprintf("\\hbox{\\strut %s}", line)))
    else
      ret <- paste0(ret, ifelse(html, sprintf("<br />%s", line), sprintf("\\hbox{\\strut %s}", line)))
    first <- FALSE
  }
  ret <- ifelse(html, ret, paste0(ret, "}"))
  
  return(ret)
}
