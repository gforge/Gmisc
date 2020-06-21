#' Formatter wrapper for \code{\link[rmarkdown]{html_document}}, facilitates easier porting to docx
#'
#' This function adds the option of having adaptations needed for seemless integration with
#' MS Word for importing html-documents in the .docx-format. The advantage of html documents is
#' the ability to create advanced formatting frequently needed in medical publications
#' and that is available in the \code{\link[htmlTable]{htmlTable}} function. You can view
#' \href{http://gforge.se/2014-07/fast-track-publishing-using-rmarkdown}{the series}
#' for more details regarding how to achieve
#' fast-track-publishing (ftp) together with knitr.
#'
#' If you want to get equations into Word the currently best way is to
#' use the \code{\link[rmarkdown]{word_document}} format.
#'
#' @param ... Passed onto \code{\link[rmarkdown]{html_document}}.
#' @param self_contained Overrides the default \code{TRUE} for
#'  \code{\link[rmarkdown]{html_document}} to \code{FALSE} as
#'  LibreOffice hangs on long lines such as the base64
#'  images included in the self-contained version.
#' @param mathjax The advanced mathjax does not work with with
#'  Word/LibreOffice.
#' @param theme No theme should be used for the output as the
#'  custom CSS should take care of everything.
#' @param highlight By default turn off highlighting as scripts
#'  are difficult to import. This does though work somewhat OK when
#'  copy-pasting from the web-browser.
#' @param css The CSS if other that the default within the package
#' @param h1_style You can choose any css style formatting here that you
#'  want to be applied to all h1 elements. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @param other_h_style This is the formatting applied to any other
#'  h elements not included to the first. Note: this is only applied
#'  if LibreOffice_adapt is \code{TRUE}.
#' @param remove_scripts \code{TRUE} if <script></script> tags are
#'  to be removed. These are usually not compatible with Word-processors
#'  and should therefore in most cases be stripped from the document.
#' @param force_captions Since out.width and out.height remove the option of
#'  having captions this allows a workaround through some processing
#'  via the XML-package
#' @param css_max_width The max width of the body element. Defaults to "40em"
#'  if not specified. Any CSS-compliant width format works.
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @export
#' @author Max Gordon
#'
#' @examples
#' # Possible yaml configuration at the top of the Rmd doc
#' \dontrun{
#' ---
#' title: "Test"
#' author: "Max Gordon"
#' output:
#'   Gmisc::docx_document
#' ---
#' }
#' @importFrom rmarkdown html_document
docx_document <- function(...,
                          # Default options for ftp
                          self_contained = FALSE,
                          mathjax = NULL,
                          theme = NULL,
                          highlight = NULL,
                          # docx_specific
                          css = "rmarkdown/docx.css",
                          h1_style = "margin: 24pt 0pt 0pt 0pt;",
                          other_h_style = "margin: 10pt 0pt 0pt 0pt;",
                          remove_scripts = TRUE,
                          force_captions = FALSE,
                          css_max_width) {

  if (css == "rmarkdown/docx.css") {
    css <- system.file(css, package = "Gmisc")
    if (css == "")
      stop("Error locating the docx.css that should be included in the Gmisc package")
    if (!self_contained) {
      file.copy(from = css, to = "docx.css", overwrite = TRUE)
      css <- "docx.css"
      if (!missing(css_max_width)) {
        css <- prSetMaxWidth(max_width = css_max_width,
                             css_file = css)
      }
    } else {
      if (!missing(css_max_width)) {
        tmp_css <- tempfile()
        file.copy(from = css, to = tmp_css, overwrite = TRUE)
        css <- prSetMaxWidth(max_width = css_max_width,
                             css_file = tmp_css)
      }
    }
  } else if (!all(sapply(css, file.exists))) {
    alt_css <- list.files(pattern = ".css$")
    if (length(alt_css) > 0) {
      alt_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.",
                        " You may want to have a YAML section that looks something like:",
                        "\n---",
                        "\noutput:",
                        "\n  Gmisc::docx_document:",
                        "\n    css: \"", paste(alt_css, collapse = "\", \""), "\"",
                        "\n---")
    } else {
      alt_css <- ""
    }

    stop("You should have the default css for optimal docx compatibility.",
         " The one or more of the css-file(s) that you've specified can't be identified.",
         " The file(s) '", paste(css[!sapply(css, file.exists)],
                                collapse = "', '"), "'",
         " can't be found from the directory '", getwd(), "'",
         " - i.e. the directory where you have your .Rmd-file",
         alt_css)
  }

  # call the base html_document function
  output_ret_val <-
    html_document(...,
                  css = css,
                  mathjax = mathjax,
                  theme = theme,
                  highlight = highlight,
                  self_contained = self_contained)

  output_ret_val$post_processor_old <-
    output_ret_val$post_processor

  output_ret_val$post_processor <-
    post_processor <- function(metadata, input_file, output_file, clean, verbose,
                               old_post_processor = output_ret_val$post_processor_old) {
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
                         h1_style = h1_style,
                         other_h_style = h1_style)

      if (remove_scripts) {
        output_str <-
          prFtpScriptRemoval(output_str)


        output_str <-
          prFtpOtherRemoval(output_str)
      }

      output_str <-
        prFtpOtherChanges(output_str)

      writeLines(output_str, output_file, useBytes = TRUE)

      if (force_captions) {
        prCaptionFix(output_file)
      }
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
#' @keywords internal
prFtpHeaderStyle <- function(output_str, h1_style, other_h_style) {
  gsub(
    paste0('<h([0-9]+)',
           '(| ',
           '([ ]*class = "[^"]+"',
           '|[ ]*id = "[^"]+")+',
           ')[ ]*>'),
    paste0('<h\\1\\2 style = "', other_h_style, '">'),
    gsub(
      paste0('<h1+',
             '(| ',
             '([ ]*class = "[^"]+"',
             '|[ ]*id = "[^"]+")+',
             ')[ ]*>'),
      paste0('<h1\\1 style = "', h1_style, '">'),
      output_str
    )
  )
}

#' Removes the <script>*</scrip>
#'
#' @param output_str The input from readLines()
#' @return string Returns without the script elements
#' @keywords internal
prFtpScriptRemoval <- function(output_str) {
  start_scripts <- grep("<script", output_str)
  end_scripts <- grep("</script", output_str)
  if (length(start_scripts) == 0)
    return(output_str)

  rows_2_exclude <-
    unlist(lapply(1:length(start_scripts), FUN = function(x) -1*start_scripts[x]:end_scripts[x]))
  return(output_str[rows_2_exclude])
}


#' Removes other unwanted lines
#'
#' @param output_str The input from readLines()
#' @return string Returns without the unwanted lines
#' @keywords internal
prFtpOtherRemoval <- function(output_str) {
  lines_2_remove <-
    c(# html-validator complains
      "<meta http-equiv = \"Content-Style-Type\" content = \"text/css\" />",

      # Invalid formatting --s although this shouldn't be in the doc. start with
      "<!-- dynamically load mathjax for compatibility with --self-contained -->"
      )
  for (line in lines_2_remove) {
    rm_line <- grep(sprintf("^%s$", line),
                    output_str)
    if (length(rm_line) == 1)
      output_str <- output_str[-rm_line]
  }

  return(output_str)
}

#' Changes lines for XML-conformity
#'
#' @param output_str The input from readLines()
#' @return string Returns with changes
#' @keywords internal
prFtpOtherChanges <- function(output_str) {
  lines_2_change <-
    c(`<meta charset = "utf-8">` = '<meta charset = "utf-8" />')
  for (line in names(lines_2_change)) {
    ch_line <- grep(sprintf("^%s$", line),
                    output_str)
    if (length(ch_line) == 1)
      output_str[ch_line] <- lines_2_change[line]
  }

  return(output_str)
}

#' Fixes the caption for elements without caption
#'
#' @param outFile The name of the file
#' @return outFile The name of the file
#' @keywords internal
#' @import XML
prCaptionFix <- function(outFile) {
  # Encapsulate within a try since there is a high risk of error
  tryCatch({
    # Read in the full file through the html parser
    tmp <- XML::htmlParse(outFile, encoding = "utf-8", replaceEntities = FALSE)

    # The caption-less images are currently located under a p-element instead of a div
    caption_less_images <- xpathApply(tmp, "/html/body//p/img")
    for (i in 1:length(caption_less_images)) {
      old_node <- xmlParent(caption_less_images[[i]])
      img_clone <- xmlClone(caption_less_images[[i]])
      new_node <- newXMLNode("div",
                             img_clone,
                             newXMLNode("p",
                                        xmlAttrs(img_clone)["title"],
                                        attrs = c(class = "caption")),
                             attrs = c(class = "figure"))
      replaceNodes(oldNode = old_node,
                   newNode = new_node)
    }

    saveXML(tmp, encoding = "utf-8", file = outFile)
  }, error = function(err) warning("Could not force captions - error occurred: '", err, "'"))

  return(outFile)
}

#' Updates the css max-width
#'
#' Reads the file, changes the max-width
#' for the body element.
#'
#' @param max_width The new max-width css setting
#' @param css_file The CSS file name
#' @return \code{string} The file name
#' @keywords internal
prSetMaxWidth <- function(max_width, css_file) {
  content <- readLines(css_file)
  content <-
    sub(pattern = "max-width: 40em; /* Body */",
        replacement = sprintf("max-width: %s; /* Body */",
                               max_width),
        x = content,
        fixed = TRUE)
  writeLines(text = content,
             con = css_file)
  return(css_file)
}