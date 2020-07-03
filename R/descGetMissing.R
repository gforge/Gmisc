#' Get statistics for missing data
#'
#' This function calculates the amount of missing per row for \code{\link{describeMean}},
#' \code{\link{describeMedian}} and custom description functions.
#' It will return invisibly when no missing values are present.
#'
#' @inheritParams describeMean
#' @return \code{vector} A vector with the missing estimate
#' @export
descGetMissing <- function(x,
                           html = TRUE,
                           number_first = TRUE,
                           percentage_sign = TRUE,
                           language = "en",
                           useNA.digits = 1,
                           ...) {
  if (!any(is.na(x))) return(invisible())
  df_arg_list <- list(x = is.na(x),
                      html = html,
                      number_first = number_first,
                      percentage_sign = percentage_sign,
                      language = language,
                      digits = useNA.digits)
  dot_args <- list(...)
  for (n in names(dot_args)) {
    if (!n %in% names(df_arg_list)) {
      df_arg_list[[n]] <- dot_args[[n]]
    }
  }
  missing <- fastDoCall(describeFactors, df_arg_list)
  return(missing["TRUE",])
}
