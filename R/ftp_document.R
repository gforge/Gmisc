#' Formatter wrapper for \code{\link[rmarkdown]{html_document}} facilitate easier LibreOffice porting
#'
#' This function adds the option of having adaptations needed for seemless integration with
#' word processor for importing html-documents. The advantage of html documents is
#' the ability to create advanced formatting frequently needed in publications
#' and that is available in the \code{\link{htmlTable}} function. You can view
#' the series on \url{http://gforge.se} for more details regarding how to achieve
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
#' @param LibreOffice_adapt \code{TRUE} if the function should adapt the html
#'  for optimal compatibility with LibreOffice.
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
                         LibreOffice_adapt = TRUE,
                         h1_style="margin: 24pt 0pt 0pt 0pt;",
                         other_h_style="margin: 10pt 0pt 0pt 0pt;") {

  if (css == "custom.css" &&
        !file.exists("custom.css")){
    stop("You should have the custom.css in the same folder as you have your .Rmd-file",
         " or this ftp won't work properly.",
         " You can find the file here: https://raw.githubusercontent.com/gforge/ftp/master/custom.css")
  }

  # call the base html_document function
  output_ret_val <-
    rmarkdown::html_document(...,
                             css = css,
                             self_contained=self_contained)

  if (LibreOffice_adapt){
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

        writeLines(output_str, output_file, useBytes = TRUE)
        return(output_file)
      }
  }

  return(output_ret_val)
}