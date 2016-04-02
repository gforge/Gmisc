#' Get missingness statistics
#'
#' This function calculates the missingness row for \code{\link{describeMean}},
#' \code{\link{describeMedian}} and custom description functions.
#'
#' @inheritParams describeMean
#' @return \code{vector} A vector with the missing estimate
#' @keywords internal
descGetMissing <- function (x,
                              html,
                              number_first,
                              percentage_sign,
                              language,
                              useNA.digits,
                              dot_args) {
  df_arg_list <- list(x = is.na(x),
                      html = html,
                      number_first = number_first,
                      percentage_sign=percentage_sign,
                      language = language,
                      digits = useNA.digits)
  for (n in names(dot_args)){
    if (!n %in% names(df_arg_list)){
      df_arg_list[[n]] <- dot_args[[n]]
    }
  }
  missing <- fastDoCall(describeFactors, df_arg_list)
  return(missing["TRUE", ])
}