#' Helper to [getDescriptionStatsBy()]
#'
#' @inheritParams getDescriptionStatsBy
#' @param name The name of the row
#'
#' @return A [base::by] list
prPropDescs <- function(x,
                        by,
                        name,
                        default_ref,
                        prop_fn,
                        html,
                        digits,
                        digits.nonzero,
                        numbers_first,
                        useNA,
                        useNA.digits,
                        percentage_sign,
                        missing_value,
                        names_of_missing,
                        NEJMstyle) {
  default_ref <- prDescGetAndValidateDefaultRef(x, default_ref)

  t <- by(x,
          by,
          FUN = prop_fn,
          html = html,
          digits = digits,
          digits.nonzero = digits.nonzero,
          number_first = numbers_first,
          useNA = useNA,
          useNA.digits = useNA.digits,
          default_ref = default_ref,
          percentage_sign = percentage_sign
  )

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

  # Check that we're dealing with only one row
  if (unique(sapply(t, length)) == 1) {
    if (is.factor(x)) {
      factor_name <- levels(x)[default_ref]
    } else {
      factor_name <- levels(factor(x))[default_ref]
    }
    # Set the rowname to a special format
    # if there was missing and this is an matrix
    # then we should avoid using this format
    name <- glue("{capitalize(factor_name)} {tolower(name)}")
  }

  if (NEJMstyle) {
    # LaTeX needs an escape before %
    # or it marks the rest of the line as
    # a comment. This is not an issue with
    # html (markdown)
    percent_sign <- ifelse(html, "%", "\\%")

    if (numbers_first) {
      name <- glue("{name} - no ({percent_sign})")
    } else {
      name <- glue("{name} - {percent_sign} (no)")
    }
  }

  # If this is the only row then set that row to the current name
  if (length(t[[1]]) == 1) {
    names(t[[1]][1]) <- name
  }

  return(t)
}
