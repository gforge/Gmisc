#' Formatter wrapper for \code{\link[rmarkdown]{html_document}} facilitate easier LibreOffice porting
#'
#' This function adds the option of having adaptations needed for seemless integration with
#' word processor for importing html-documents. The advantage of html documents is
#' the ability to create advanced formatting frequently needed in publications
#' and that is available in the \code{\link{htmlTable}} function. You can view
#' \url{http://gforge.se/2014-07/fast-track-publishing-using-rmarkdown}{the series}
#' for more details regarding how to achieve
#' fast-track-publishing (ftp) together with knitr.
#'
#' @param ... Passed onto \code{\link[rmarkdown]{html_document}}.
#' @param css Assumes that you have the ftp-specific \code{custom.css} file in
#'  the same folder as the knitr document from the Github
#'  \href{https://github.com/gforge/ftp/blob/master/custom.css}{gforge/ftp repository}.
#'  This can be over-ridden by providing your css files.
#' @param self_contained Overrides the default \code{TRUE} for
#'  \code{\link[rmarkdown]{html_document}} to \code{FALSE} as
#'  LibreOffice can't hangs on long lines such as the base64
#'  images included in the self-contained version.
#' @param h1_style You can choose any css style formatting here that you
#'  want to be applied to all h1 elements. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @param other_h_style This is the formatting applied to any other
#'  h elements not included to the first. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @export
#' @author Max Gordon
#'
#' @examples
#' # Possible yaml configuration at the top of the Rmd doc
#' \dontrun{
#' ---
#' title: "Test"
#' author: "MG"
#' date: "Sunday, July 13, 2014"
#' output:
#'   Gmisc::rmd_ftp
#' ---
#' }
#' @importFrom rmarkdown html_document
ftp_document <- function(...,
                         css = "custom.css",
                         self_contained = FALSE,
                         h1_style="margin: 24pt 0pt 0pt 0pt;",
                         other_h_style="margin: 10pt 0pt 0pt 0pt;") {

  if (css == "custom.css" &&
        !file.exists("custom.css")){
    alt_css <- list.files(pattern = ".css$")
    if (length(alt_css) > 0){
      alt_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.",
                        " You may want to have a YAML section that looks something like:",
                        "\n---",
                        "\noutput:",
                        "\n  Gmisc::ftp_document:",
                        "\n    css: \"", paste(alt_css, collapse = "\", \""), "\"",
                        "\n---")
    }else{
      alt_css <- ""
    }
    stop("You should have the custom.css in the same folder (", getwd(),") as you have your .Rmd-file",
         " or this fast-track-publishing document formatter won't work properly.",
         alt_css,
         "\n - You can find the recommended file here: https://raw.githubusercontent.com/gforge/ftp/master/custom.css\n")
  }

  # call the base html_document function
  output_ret_val <-
    rmarkdown::html_document(...,
                             css = css,
                             self_contained=self_contained)

  output_ret_val$post_processor_old <-
    output_ret_val$post_processor

  output_ret_val$post_processor <-
    post_processor <- function(metadata, input_file, output_file, clean, verbose,
                               old_post_processor =  output_ret_val$post_processor_old) {
      # Call the original post-processor in order to limit the changes that this function
      # has on the original functionality
      output_file <-
        old_post_processor(
          metadata = metadata,
          input_file = input_file,
          output_file = output_file,
          clean = clean,
          verbose = verbose
        )

      # read the output file
      output_str <- readLines(output_file, warn = FALSE, encoding = "UTF-8")

      # Annoyingly it seems that Libre Office currently
      # 'forgets' the margin properties of the headers,
      # we therefore substitute these with a element specific
      # style option that works. Perhaps not that pretty but
      # it works and can be tweaked for most things.
      output_str <-
        prFtpHeaderStyle(output_str,
                         h1_style=h1_style,
                         other_h_style=h1_style)

      output_str <-
        prFtpScriptRemoval(output_str)


      output_str <-
        prFtpOtherRemoval(output_str)

      writeLines(output_str, output_file, useBytes = TRUE)
      return(output_file)
    }

  return(output_ret_val)
}

#' Helper to ftp_document
#'
#' @param output_str The output string from readLines()
#' @param h1_style You can choose any css style formatting here that you
#'  want to be applied to all h1 elements. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @param other_h_style This is the formatting applied to any other
#'  h elements not included to the first. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @return string
prFtpHeaderStyle <- function(output_str, h1_style, other_h_style){
  gsub(
    paste0('<h([0-9]+)',
           '(| ',
           '([ ]*class="[^"]+"',
           '|[ ]*id="[^"]+")+',
           ')[ ]*>'),
    paste0('<h\\1\\2 style="', other_h_style, '">'),
    gsub(
      paste0('<h1+',
             '(| ',
             '([ ]*class="[^"]+"',
             '|[ ]*id="[^"]+")+',
             ')[ ]*>'),
      paste0('<h1\\1 style="', h1_style, '">'),
      output_str
    )
  )
}

#' Removes the <script>*</scrip>
#'
#' @param output_str The input from readLines()
#' @return string Returns without the script elements
prFtpScriptRemoval <- function(output_str){
  start_scripts <- grep("<script", output_str)
  end_scripts <- grep("</script", output_str)
  rows_2_exclude <-
    unlist(lapply(1:length(start_scripts), FUN = function(x) -1*start_scripts[x]:end_scripts[x]))
  return(output_str[rows_2_exclude])
}


#' Removes other unwanted lines
#'
#' @param output_str The input from readLines()
#' @return string Returns without the script elements
prFtpOtherRemoval <- function(output_str){
  lines_2_remove <-
    c("<meta http-equiv=\"Content-Style-Type\" content=\"text/css\" />", # validator complains
      "<!-- dynamically load mathjax for compatibility with --self-contained -->" # Invalid formatting --s
      )
  for (line in lines_2_remove){
    rm_line <- grep(sprintf("^%s$", line),
                    output_str)
    if (length(rm_line) == 1)
      output_str <- output_str[-rm_line]
  }

  return(output_str)
}