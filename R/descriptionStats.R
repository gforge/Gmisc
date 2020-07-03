#' Describe the mean
#'
#' A function that returns a description of a continuous variable using
#' the mean together with the standard deviation. The standard deviation
#' is used as it is "industry standard" to use mean with standard deviation
#' and not because it's the only option.
#'
#' @param x The variable that you want the statistics for
#' @param digits The number of decimals used
#' @param digits.nonzero The number of decimals used for values that are close to zero
#' @param html If HTML compatible output should be used. If \code{FALSE}
#'  it outputs LaTeX formatting
#' @param number_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#'  This is only used together with the useNA variable.
#' @param useNA This indicates if missing should be added as a separate
#'  row below all other. See \code{\link[base]{table}} for \code{useNA}-options.
#'  \emph{Note:} defaults to ifany and not "no" as \code{\link[base]{table}} does.
#' @param useNA.digits The number of digits to use for the
#'  missing percentage, defaults to the overall \code{digits}.
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default \% if you so wish by setting this variable. Note, this is
#'  only used when combined with the missing information.
#' @param plusmin_str Provide if you want anything other than the plus minus sign
#'  suited for the given output format.
#' @param language The ISO-639-1 two-letter code for the language of
#'  interest. Currently only english is distinguished from the ISO
#'  format using a ',' as the separator in the \code{\link{txtInt}}
#'  function.
#' @param ... Passed on to \code{\link{describeFactors}}
#' @return \code{string} Returns a string formatted for either LaTeX or HTML
#'
#' @seealso \code{\link{getDescriptionStatsBy}}
#'
#' @examples
#' describeMean(1:10)
#' describeMean(c(1:10, NA), useNA = "always")
#' describeMean(c(1:10, NA), useNA = "no")
#' @family descriptive functions
#' @importFrom stats sd
#' @export
describeMean <- function(x,
                         html = TRUE,
                         digits = 1,
                         digits.nonzero = NA,
                         number_first = TRUE,
                         useNA = c("ifany", "no", "always"),
                         useNA.digits = digits,
                         percentage_sign = TRUE,
                         plusmin_str,
                         language = "en",
                         ...) {
  dot_args <- list(...)

  # Warnings due to interface changes in 1.0
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }

  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }

  useNA <- match.arg(useNA)

  if (missing(plusmin_str)) {
    if (html) {
      plusmin_str <- "&plusmn;"
    } else {
      plusmin_str <- "\\pm"
    }
  }

  ret <- c(sprintf(
    "%s (%s%s)",
    txtRound(mean(x, na.rm = T), digits = digits, digits.nonzero = digits.nonzero),
    plusmin_str,
    txtRound(sd(x, na.rm = T), digits = digits, digits.nonzero = digits.nonzero)
  ))

  # LaTeX complains if any formula isn't encapsulated within $ signs
  if (html == FALSE) {
    ret <- sprintf("$%s$", ret)
  }

  if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 0) {
    ret <- rbind(
      ret,
      descGetMissing(
        x = x,
        html = html,
        number_first = number_first,
        percentage_sign = percentage_sign,
        language = language,
        useNA.digits = useNA.digits,
        digits.nonzero = digits.nonzero,
        dot_args = dot_args
      )
    )
    rownames(ret) <- c("Mean (SD)", "Missing")
  } else if (useNA == "always") {
    if (percentage_sign == TRUE) {
      percentage_sign <- ifelse(html, "%", "\\%")
    } else if (is.character(percentage_sign) == FALSE) {
      percentage_sign <- ""
    }

    empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"), percentage_sign)
    ret <- rbind(ret, rep(empty, times = NCOL(ret)))
    rownames(ret) <- c("Mean (SD)", "Missing")
  } else {
    names(ret) <- "Mean (SD)"
  }

  return(ret)
}


#' A function that returns a description median that contains
#' the interquartile range or the full range
#'
#' @param iqr If interquartile range should be used
#' @return \code{string} A string formatted for either LaTeX or HTML
#'
#' @inheritParams describeMean
#' @seealso \code{\link{getDescriptionStatsBy}}
#'
#' @examples
#' describeMedian(1:10)
#' describeMedian(c(1:10, NA), useNA = "ifany")
#' @family descriptive functions
#' @importFrom stats median quantile
#' @export
describeMedian <- function(x,
                           iqr = TRUE,
                           html = TRUE,
                           digits = 1,
                           digits.nonzero = NA,
                           number_first = TRUE,
                           useNA = c("ifany", "no", "always"),
                           useNA.digits = digits,
                           percentage_sign = TRUE,
                           language = "en",
                           ...) {
  dot_args <- list(...)

  # Warnings due to interface changes in 1.0
  if ("show_missing_digits" %in% names(dot_args)) {
    show_missing.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now show_missing.digits as of ver. 1.0")
  }

  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }

  useNA <- match.arg(useNA)

  if (iqr) {
    range_quantiles <- c(1 / 4, 3 / 4)
  } else {
    range_quantiles <- c(0, 1)
  }

  ret <- sprintf(
    "%s (%s - %s)",
    txtRound(median(x, na.rm = TRUE), digits = digits, digits.nonzero = digits.nonzero),
    txtRound(quantile(x, probs = range_quantiles[1], na.rm = TRUE), digits = digits, digits.nonzero = digits.nonzero),
    txtRound(quantile(x, probs = range_quantiles[2], na.rm = TRUE), digits = digits, digits.nonzero = digits.nonzero)
  )

  if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 0) {
    ret <- rbind(
      ret,
      descGetMissing(
        x = x,
        html = html,
        number_first = number_first,
        percentage_sign = percentage_sign,
        language = language,
        useNA.digits = useNA.digits,
        digits.nonzero = digits.nonzero,
        dot_args = dot_args
      )
    )

    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing"
    )
  } else if (useNA == "always") {
    if (percentage_sign == TRUE) {
      percentage_sign <- ifelse(html, "%", "\\%")
    } else if (is.character(percentage_sign) == FALSE) {
      percentage_sign <- ""
    }

    empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"), percentage_sign)
    ret <- rbind(ret, rep(empty, times = NCOL(ret)))
    rownames(ret) <- c(
      ifelse(iqr, "Median (IQR)", "Median (range)"),
      "Missing"
    )
  } else {
    names(ret) <- ifelse(iqr, "Median (IQR)", "Median (range)")
  }

  return(ret)
}

#' A function that returns a description proportion that contains
#' the number and the percentage
#'
#' @return \code{string} A string formatted for either LaTeX or HTML
#'
#' @inheritParams describeMean
#' @inheritParams prDescGetAndValidateDefaultRef
#' @examples
#' describeProp(factor(sample(50, x = c("A", "B", NA), replace = TRUE)))
#' @family descriptive functions
#' @export
describeProp <- function(x,
                         html = TRUE,
                         digits = 1,
                         digits.nonzero = NA,
                         number_first = TRUE,
                         useNA = c("ifany", "no", "always"),
                         useNA.digits = digits,
                         default_ref,
                         percentage_sign = TRUE,
                         language = "en",
                         ...) {
  dot_args <- list(...)

  # Warnings due to interface changes in 1.0
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }

  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }

  useNA <- match.arg(useNA)

  default_ref <- prDescGetAndValidateDefaultRef(x, default_ref)

  # If we're to use the horizontal proportions then
  # it's better to report the variable as a factor
  # instead of a single proportion.
  # When we have missing it also gets more difficult
  # to just report one percentage as it suddenly uncertain
  # for what percentage the number applies to
  if ("horizontal_proportions" %in% names(dot_args) ||
    (useNA == "ifany" &&
      any(is.na(x))) ||
    useNA == "always") {
    df_arg_list <- list(
      x = x,
      html = html,
      number_first = number_first,
      percentage_sign = percentage_sign,
      language = language,
      digits = digits,
      digits.nonzero = digits.nonzero,
      useNA = useNA,
      useNA.digits = useNA.digits
    )
    for (n in names(dot_args)) {
      if (!n %in% names(df_arg_list)) {
        df_arg_list[[n]] <- dot_args[[n]]
      }
    }
    return(fastDoCall(describeFactors, df_arg_list))
  }

  if (!is.factor(x)) {
    x <- factor(x)
  }

  no <- sum(x == levels(x)[default_ref], na.rm = T)

  # Don't count missing since those are treated as factors if any
  percent <- 100 * no / length(x[is.na(x) == FALSE])

  oi_args <- list(
    x = no,
    language = language,
    html = html
  )
  for (n in names(dot_args)) {
    if (!n %in% names(oi_args)) {
      oi_args[[n]] <- dot_args[[n]]
    }
  }
  no <- fastDoCall(txtInt, oi_args)

  # LaTeX treats % as comments unless it's properly escaped
  if (percentage_sign == TRUE) {
    percentage_sign <- ifelse(html, "%", "\\%")
  } else if (!is.character(percentage_sign)) {
    percentage_sign <- ""
  }

  percentage_str <- paste0(txtRound(percent, digits = digits, digits.nonzero = digits.nonzero), percentage_sign)
  if (number_first) {
    ret <- sprintf(
      "%s (%s)",
      no,
      percentage_str
    )
  } else {
    ret <- sprintf(
      "%s (%s)",
      percentage_str,
      no
    )
  }

  return(ret)
}


#' Describes factor variables
#'
#' A function that returns a description of proportions in a
#' factor that contains the number of times a level occurs and the percentage
#'
#' @param ... Passed on to \code{\link{txtInt}}
#' @param horizontal_proportions Is only active if useNA since this is
#'  the only case of a proportion among continuous variables. This is default NULL and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then supply the
#'  function with the total number in each group, i.e. if done in a by
#'  manner as in \code{\link{getDescriptionStatsBy}} it needs to provide
#'  the number before the by() command.
#' @return A string formatted for printing either latex by  HTML
#'
#' @inheritParams describeMean
#' @seealso \code{\link{getDescriptionStatsBy}}
#' @family descriptive functions
#'
#' @examples
#' set.seed(1)
#' describeFactors(sample(50, x = c("A", "B", "C"), replace = TRUE))
#'
#' n <- 500
#' my_var <- factor(sample(size = n, x = c("A", "B", "C", NA), replace = TRUE))
#' my_exp <- rbinom(n = n, size = 1, prob = 0.2)
#' total <- table(my_var, useNA = "ifany")
#' by(my_var,
#'   INDICES = my_exp,
#'   FUN = describeFactors,
#'   useNA = "ifany",
#'   horizontal_proportions = total
#' )
#' @export
describeFactors <- function(x,
                            html = TRUE,
                            digits = 1,
                            digits.nonzero = NA,
                            number_first = TRUE,
                            useNA = c("ifany", "no", "always"),
                            useNA.digits = digits,
                            horizontal_proportions,
                            percentage_sign = TRUE,
                            language = "en",
                            ...) {
  dot_args <- list(...)

  # Warnings due to interface changes in 1.0
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }

  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- convertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }

  useNA <- match.arg(useNA)

  # Get basic data
  table_results <- table(x, useNA = useNA)

  # Check if we should relate to an external total
  if (!missing(horizontal_proportions)) {

    # Error check the horizontal_proportions variable
    # First check that it's a table or at least a vector
    if (is.numeric(horizontal_proportions) == FALSE ||
      (!inherits(horizontal_proportions, "table") &&
        is.vector(horizontal_proportions) == FALSE)) {
      stop(
        "You have not provided a proper table/vector variable for the function,",
        " the class you've given is: '", paste(class(horizontal_proportions), collapse = " & "), "'",
        ", instead of numeric vector or table"
      )
    }

    # The original table should always be longer than the subtable
    if (length(horizontal_proportions) < length(table_results)) {
      stop(
        "There is a length discrepancy between the number of groups in the given sample",
        " and the reference sample, ",
        length(horizontal_proportions), " < ", length(table_results), "!"
      )
    }

    # All variables should exist in the horizontal table
    if (all(names(table_results) %in% names(horizontal_proportions)) == FALSE) {
      stop(
        "Your results contain results that are not in ",
        "the reference horizontal_proportions: ",
        paste(names(table_results)[names(table_results) %in% names(horizontal_proportions)],
          collapse = ", "
        )
      )
    }

    # Create a equal length array with the same order
    if (length(horizontal_proportions) > length(table_results)) {
      # There seems to be a few variables more in the horizontal version and
      # then we need to fill in those values with 0
      # Initiate an empty array
      tmp <- rep(0, times = length(horizontal_proportions))
      # The array should have the names of the results
      # to be able to reference them
      names(tmp) <- names(horizontal_proportions)
      # Iterate and copy the values
      for (n in names(tmp)) {
        if (n %in% names(table_results)) {
          tmp[n] <- table_results[n]
        }
      }
      # Reset the table_results to our new array
      table_results <- tmp
      class(tmp) <- "table"
    }

    # Initiate an empty array
    percentages <- rep(NA, times = length(horizontal_proportions))
    # The array should have the names of the results
    # to be able to reference them
    names(percentages) <- names(horizontal_proportions)
    for (n in names(horizontal_proportions)) {
      # Strange things happen with NA
      # Need to find that element in a slightly odd way
      if (is.na(n)) {
        percentages[is.na(names(percentages))] <-
          table_results[is.na(names(table_results))] / horizontal_proportions[is.na(names(horizontal_proportions))] * 100
      } else {
        percentages[n] <- table_results[n] / horizontal_proportions[n] * 100
      }
    }
  } else {
    percentages <- table_results / sum(table_results) * 100
  }

  # Format the values
  sa_args <- list(
    X = table_results,
    FUN = txtInt,
    language = language,
    html = html
  )
  for (n in names(dot_args)) {
    if (!n %in% names(sa_args)) {
      sa_args[[n]] <- dot_args[[n]]
    }
  }
  values <- fastDoCall(sapply, sa_args)

  # LaTeX treats % as comments unless it's properly escaped
  if (percentage_sign == TRUE) {
    percentage_sign <- ifelse(html, "%", "\\%")
  } else if (is.character(percentage_sign) == FALSE) {
    percentage_sign <- ""
  }

  # The is.na(...) is a little overkill
  percentage_str <- paste0(txtRound(percentages,
    digits = ifelse(is.na(names(table_results)), useNA.digits, digits),
    digits.nonzero = digits.nonzero
  ), percentage_sign)

  if (number_first) {
    ret <- matrix(sprintf("%s (%s)", values, percentage_str), ncol = 1)
  } else {
    ret <- matrix(sprintf("%s (%s)", percentage_str, values), ncol = 1)
  }

  rn <- names(table_results)
  rn[is.na(rn)] <- "Missing"
  rownames(ret) <- rn
  return(ret)
}