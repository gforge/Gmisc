#' Formatter wrapper for \code{\link[rmarkdown]{html_document}} facilitate easier LibreOffice porting
#'
#' This function adds the option of having adaptations needed for seemless integration with
#' word processor for importing html-documents. The advantage of html documents is
#' the ability to create advanced formatting frequently needed in publications
#' and that is available in the \code{\link{htmlTable}} function. You can view
#' the series on \url{http://gforge.se} for more details regarding how to achieve
#' fast-track-publishing (ftp) together with knitr.
#'
#' @param ... Passed onto \code{\link[rmarkdow]{html_document}}.
#' @param LibreOffice_adapt \code{TRUE} if the function should adapt the html
#'  for optimal compatibility with LibreOffice.
#' @param h1_style You can choose any css style formatting here that you
#'  want to be applied to all h1 elements. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @param other_h_style This is the formatting applied to any other
#'  h elements not included to the first. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @return String
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
#'   Gmisc::rmd_ftp:
#'     css: custom.css
#' ---
#' }
rmd_ftp <- function(...,
                    LibreOffice_adapt = TRUE,
                    h1_style="margin: 24pt 0pt 0pt 0pt;",
                    other_h_style="margin: 10pt 0pt 0pt 0pt;") {

  # get the locations of resource files located within the package
  css <- system.file("reports/styles.css", package = "mypackage")
  header <- system.file("reports/quarterly/header.html", package = "mypackage")

  # call the base html_document function
  output_ret_val <-
    rmarkdown::html_document(...)

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