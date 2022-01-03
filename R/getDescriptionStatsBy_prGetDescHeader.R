#' Retrieve basic description stats by header
#'
#' Helper for [getDescriptionStatsBy] that retrieves the basic
#' header names.
#'
#' @param already_table_format Just a boolean as we use this in the total column
#'
#' @return A vector with basic headers
#' @inheritParams getDescriptionStatsBy
prGetDescHeader <- function(by, html, header_count, already_table_format = FALSE) {
  if (already_table_format) {
    tbl_cnt <- by
  } else {
    tbl_cnt <- table(by)
  }
  if (is.null(header_count) || identical(header_count, FALSE)) {
    return(names(tbl_cnt))
  }

  if (is.character(header_count)) {
    if (!grepl("%s", header_count, fixed = TRUE)) {
      stop(
        "Your header_count must accept a string character",
        " or it will fail to add the count, i.e. use the ",
        " format: 'Text before %s text after'"
      )
    }
    cnt_str <-
      sprintf(
        header_count,
        txtInt(tbl_cnt)
      )
  } else {
    cnt_str <- paste("No.", txtInt(tbl_cnt))
  }

  return(mapply(txtMergeLines,
                names(tbl_cnt),
                cnt_str,
                html = html
  ))
}
