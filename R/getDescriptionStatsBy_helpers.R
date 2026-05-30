#' Get statistics according to the type
#'
#' A simple function applied by the \code{\link{getDescriptionStatsBy}}
#' for the total column.
#'
#' @return A matrix or a vector depending on the settings
#'
#' @inheritParams getDescriptionStatsBy
#' @keywords internal
#' @importFrom stats na.omit
prGetStatistics <- function(x,
                            show_perc = FALSE,
                            html = TRUE,
                            digits = 1,
                            digits.nonzero = NA,
                            numbers_first = TRUE,
                            useNA = c("ifany", "no", "always"),
                            useNA.digits = digits,
                            show_all_values = FALSE,
                            continuous_fn = describeMean,
                            factor_fn = describeFactors,
                            prop_fn = factor_fn,
                            percentage_sign = TRUE,
                            default_ref = NULL) {
  # All the describe functions have the same interface
  # so it is useful to gather all the arguments here
  describe_args <- list(
    x = x,
    html = html,
    digits = digits,
    digits.nonzero = digits.nonzero,
    number_first = numbers_first,
    useNA = useNA,
    useNA.digits = useNA.digits,
    percentage_sign = percentage_sign,
    default_ref = default_ref
  )

  if (is.factor(x) ||
    is.logical(x) ||
    is.character(x)) {
    if ((is.factor(x) &&
      length(levels(x)) == 2) ||
      (!is.factor(x) &&
        length(unique(na.omit(x))) == 2)) {
      if (show_perc) {
        total_table <- fastDoCall(prop_fn, describe_args)
      } else {
        total_table <- table(x, useNA = useNA)
        names(total_table)[is.na(names(total_table))] <- "Missing"
        # Choose only the reference level
        if (show_all_values == FALSE) {
          total_table <- total_table[names(total_table) %in%
            c(levels(as.factor(x))[1], "Missing")]
        }
      }
    } else {
      if (show_perc) {
        total_table <- fastDoCall(factor_fn, describe_args)
      } else {
        total_table <- table(x, useNA = useNA) %>%
          txtInt()
        names(total_table)[is.na(names(total_table))] <- "Missing"
      }
    }
  } else {
    total_table <- fastDoCall(continuous_fn, describe_args)

    # If a continuous variable has two rows then it's assumed that the second is the missing
    if (length(total_table) == 2 &&
      show_perc == FALSE) {
      total_table[2] <- sum(is.na(x))
    }
  }
  return(total_table)
}

#' A helper function for the description stats
#'
#' @param x The variable of interest with the levels
#' @param default_ref The default reference, either first,
#'  the level name or a number within the levels. If left out
#'  it defaults to the first value.
#' @return \code{integer} The level number of interest
#'
#' @keywords internal
prDescGetAndValidateDefaultRef <- function(x, default_ref) {
  if (is.null(default_ref)) {
    # Use by default the first factor element (all chars are converted to a factor prior calculation)
    return(1)
  }

  if (is.character(default_ref)) {
    if (default_ref %in% levels(x)) {
      return(which(default_ref == levels(x)))
    }

    stop(
      "You have provided an invalid default reference, '",
      default_ref, "' can not be found among: ", paste(levels(x), collapse = ", ")
    )
  }

  if (!default_ref %in% 1:length(levels(x))) {
    stop(
      "You have provided an invalid default reference,",
      " it is ", default_ref, " while it should be between 1 and ", length(levels(x)),
      " as this is only used for factors."
    )
  }

  return(default_ref)
}
