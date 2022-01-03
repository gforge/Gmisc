#' Helper to [getDescriptionStatsBy()]
#'
#' @inheritParams getDescriptionStatsBy
#'
#' @return A [base::by] list
prFactorDescs <- function(x,
                          by,
                          factor_fn,
                          hrzl_prop,
                          html,
                          digits,
                          digits.nonzero,
                          numbers_first,
                          useNA,
                          useNA.digits,
                          percentage_sign,
                          missing_value,
                          names_of_missing) {
  if (hrzl_prop) {
    t <- by(x, by,
            FUN = factor_fn,
            html = html,
            digits = digits,
            digits.nonzero = digits.nonzero,
            number_first = numbers_first,
            useNA = useNA,
            useNA.digits = useNA.digits,
            horizontal_proportions = table(x, useNA = useNA),
            percentage_sign = percentage_sign
    )
  } else {
    t <- by(x, by,
            FUN = factor_fn,
            html = html,
            digits = digits,
            digits.nonzero = digits.nonzero,
            number_first = numbers_first,
            useNA = useNA,
            useNA.digits = useNA.digits,
            percentage_sign = percentage_sign
    )
  }
  missing_t <- sapply(t, is.null)
  if (any(missing_t)) {
    substitute_t <- rep(missing_value, length(t[!missing_t][[1]]))
    names(substitute_t) <- names(t[!missing_t][[1]])
    for (i in seq_along(t[missing_t])) {
      t[missing_t][[i]] <- substitute_t
    }
  }

  if (all(unlist(sapply(t, is.na))) & !is.null(names_of_missing)) {
    substitute_t <- rep(missing_value, length(names_of_missing))
    names(substitute_t) <- names_of_missing
    substitute_list <- vector("list", length = length(t))
    names(substitute_list) <- names(t)
    for (i in seq_along(substitute_list)) {
      substitute_list[[i]] <- substitute_t
    }
    t <- substitute_list
  }

  return(t)
}
