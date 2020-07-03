#' Creating of description statistics
#'
#' A function that returns a description statistic that can be used
#' for creating a publication "table 1" when you want it by groups.
#' The function identifies if the variable is a continuous, binary
#' or a factored variable. The format is inspired by NEJM, Lancet &
#' BMJ.
#'
#' @section Customizing statistics:
#'
#' You can specify what function that you want for statistic by providing a function
#' that takes two arguments \code{x} and \code{by} and returns a p-value. There are
#' a few functions already prepared for this see \code{\link{getPvalAnova}},
#' \code{\link{getPvalChiSq}}
#' \code{\link{getPvalFisher}}
#' \code{\link{getPvalKruskal}}
#' \code{\link{getPvalWilcox}}.
#' The default functions used are \code{getPvalFisher} and \code{getPvalWilcox} (unless the by
#' argument has more than three unique levels where it defaults to \code{getPvalAnova}).
#'
#' If you want the function to select functions depending on the type of input
#' you can provide a list with the names \code{'continuous'}, \code{'proportion'}, \code{'factor'} and
#' the function will choose accordingly. If you fail to define a certain category
#' it will default to the above.
#'
#' You can also use a custom function that returns a string with the attribute \code{'colname'}
#' set that will be appended to the results instead of the p-value column.
#'
#' @param x The variable that you want the statistics for
#' @param by The variable that you want to split into different
#'  columns
#' @param digits The number of decimals used
#' @param digits.nonzero The number of decimals used for values that are close to zero
#' @param html If HTML compatible output should be used. If \code{FALSE}
#'  it outputs LaTeX formatting
#' @param NEJMstyle Adds - no (\%) at the end to proportions
#' @param numbers_first If the number should be given or if the percentage
#'  should be presented first. The second is encapsulated in parentheses ().
#' @param useNA This indicates if missing should be added as a separate
#'  row below all other. See \code{\link[base]{table}} for \code{useNA}-options.
#'  \emph{Note:} defaults to ifany and not "no" as \code{\link[base]{table}} does.
#' @param useNA.digits The number of digits to use for the
#'  missing percentage, defaults to the overall \code{digits}.
#' @param continuous_fn The method to describe continuous variables. The
#'  default is \code{\link{describeMean}}.
#' @param prop_fn The method used to describe proportions, see \code{\link{describeProp}}.
#' @param factor_fn The method used to describe factors, see \code{\link{describeFactors}}.
#' @param statistics Add statistics, fisher test for proportions and Wilcoxon
#'  for continuous variables. See details below for more customization.
#' @param statistics.two_dec_lim The limit for showing two decimals. E.g.
#'  the p-value may be 0.056 and we may want to keep the two decimals in order
#'  to emphasize the proximity to the all-mighty 0.05 p-value and set this to
#'  \eqn{10^-2}. This allows that a value of 0.0056 is rounded to 0.006 and this
#'  makes intuitive sense as the 0.0056 level as this is well below
#'  the 0.05 value and thus not as interesting to know the exact proximity to
#'  0.05. \emph{Disclaimer:} The 0.05-limit is really silly and debated, unfortunately
#'  it remains a standard and this package tries to adapt to the current standards in order
#'  to limit publication associated issues.
#' @param statistics.sig_lim The significance limit for < sign, i.e. p-value 0.0000312
#'  should be < 0.0001 with the default setting.
#' @param statistics.suppress_warnings Hide warnings from the statistics function.
#' @param show_all_values This is by default false as for instance if there is
#'  no missing and there is only one variable then it is most sane to only show
#'  one option as the other one will just be a complement to the first. For instance
#'  sex - if you know gender then automatically you know the distribution of the
#'  other sex as it's 100 \% - other \%. To choose which one you want to show then
#'  set the \code{default_ref} parameter.
#' @param hrzl_prop This is default FALSE and indicates
#'  that the proportions are to be interpreted in a vertical manner.
#'  If we want the data to be horizontal, i.e. the total should be shown
#'  and then how these differ in the different groups then set this to TRUE.
#' @param add_total_col This adds a total column to the resulting table.
#'  You can also specify if you want the total column "first" or "last"
#'  in the column order.
#' @param total_col_show_perc This is by default true but if
#'  requested the percentages are suppressed as this sometimes may be confusing.
#' @param use_units If the Hmisc package's units() function has been employed
#'  it may be interesting to have a column at the far right that indicates the
#'  unit measurement. If this column is specified then the total column will
#'  appear before the units (if specified as last). You can also set the value to
#'  \code{"name"} and the units will be added to the name as a parenthesis,
#'  e.g. Age (years).
#' @param percentage_sign If you want to suppress the percentage sign you
#'  can set this variable to FALSE. You can also choose something else that
#'  the default \% if you so wish by setting this variable.
#' @param header_count Set to \code{TRUE} if you want to add a header count,
#'  e.g. Smoking; No. 25 observations, where there is a new line after the
#'  factor name. If you want a different text for the second line you can
#'  specifically use the \code{\link[base]{sprintf}} formatting, e.g. "No. \%s patients".
#' @param missing_value Value that is substituted for empty cells. Defaults to "-"
#' @param names_of_missing Optional character vector containing the names of returned statistics,
#'  in case all returned values for a given \code{by} level are missing. Defaults to NULL
#' @param ... Currently only used for generating warnings of deprecated call
#'  parameters.
#' @return Returns a vector if vars wasn't specified and it's a
#'  continuous or binary statistic. If vars was a matrix then it
#'  appends the result to the end of that matrix. If the x variable
#'  is a factor then it does not append and you get a warning.
#'
#' @inheritParams prDescGetAndValidateDefaultRef
#' @example inst/examples/getDescriptionStatsBy_example.R
#'
#' @family descriptive functions
#'
#' @importFrom Hmisc label label<- units capitalize
#' @importFrom stats na.omit
#'
#' @export
getDescriptionStatsBy <- function(x,
                                  by,
                                  digits = 1,
                                  digits.nonzero = NA,
                                  html = TRUE,
                                  numbers_first = TRUE,
                                  statistics = FALSE,
                                  statistics.sig_lim = 10^-4,
                                  statistics.two_dec_lim = 10^-2,
                                  statistics.suppress_warnings = TRUE,
                                  useNA = c("ifany", "no", "always"),
                                  useNA.digits = digits,
                                  continuous_fn = describeMean,
                                  prop_fn = describeProp,
                                  factor_fn = describeFactors,
                                  show_all_values = FALSE,
                                  hrzl_prop = FALSE,
                                  add_total_col,
                                  total_col_show_perc = TRUE,
                                  use_units = FALSE,
                                  default_ref,
                                  NEJMstyle = FALSE,
                                  percentage_sign = TRUE,
                                  header_count,
                                  missing_value = "-",
                                  names_of_missing = NULL,
                                  ...) {
  API_changes <-
    c(
      show_missing_digits = "show_missing.digits",
      show_missing = "useNA",
      sig.limit = "statistics.sig_lim",
      two_dec.limit = "statistics.two_dec_lim"
    )
  dots <- list(...)
  fenv <- environment()
  for (i in 1:length(API_changes)) {
    old_name <- names(API_changes)[i]
    new_name <- API_changes[i]
    if (old_name %in% names(dots)) {
      if ("name" %in% class(fenv[[new_name]])) {
        fenv[[new_name]] <- dots[[old_name]]
        dots[[old_name]] <- NULL
        warning(
          "Deprecated: '", old_name, "'",
          " argument is now '", new_name, "'",
          " as of ver. 1.0"
        )
      } else {
        stop(
          "You have set both the old parameter name: '", old_name, "'",
          " and the new parameter name: '", new_name, "'."
        )
      }
    }
  }

  useNA <- match.arg(useNA)
  if (!is.na(digits.nonzero)) {
    if (!is.numeric(digits.nonzero)
    || floor(digits.nonzero) != digits.nonzero
    ) {
      stop("The digits.nonzero should be an integer, you provided: ", digits.nonzero)
    }
    if (digits.nonzero < digits) {
      stop("The digits.nonzero must be smaller than digits")
    }
  }

  if (!is.function(statistics)) {
    if (is.list(statistics) ||
      (statistics != FALSE)) {
      if (is.list(statistics)) {
        types <- c(
          "continuous",
          "proportion",
          "factor"
        )
        if (any(!names(statistics) %in% types)) {
          stop(
            "If you want to provide custom functions for generating statistics",
            " you must either provide a function or a list with the elements:",
            " '", paste(types, collapse = "', '"), "'"
          )
        }

        if (is.numeric(x) &&
          length(unique(x)) != 2) {
          statistics <- statistics[["continuous"]]
        } else if (length(unique(x)) == 2) {
          if ("proportion" %in% names(statistics)) {
            statistics <- statistics[["proportion"]]
          } else {
            statistics <- statistics[["factor"]]
          }
        } else {
          statistics <- statistics[["factor"]]
        }

        if (is.character(statistics)) {
          statistics <- get(statistics)
        }
      }

      if (!is.function(statistics)) {
        if (length(unique(x)) == 2) {
          statistics <- getPvalFisher
        } else if (is.numeric(x)) {
          if (length(unique(by)) == 2) {
            statistics <- getPvalWilcox
          } else {
            statistics <- getPvalAnova
          }
        } else {
          statistics <- getPvalFisher
        }
      }
    }
  }

  if (is.function(statistics)) {
    if (statistics.suppress_warnings) {
      pval <- suppressWarnings(
        statistics(
          x = x,
          by = by
        )
      )
    } else {
      pval <-
        statistics(
          x = x,
          by = by
        )
    }
  }

  # Always have a total column if the description statistics
  # are presented in a horizontal fashion
  if (missing(add_total_col) &&
    hrzl_prop) {
    add_total_col <- TRUE
  }

  if (is.null(x)) {
    stop(
      "You haven't provided an x-value to do the statistics by.",
      " This error is most frequently caused by referencing an old",
      " variable name that doesn't exist anymore"
    )
  }
  if (is.null(by)) {
    stop(
      "You haven't provided a by-value to do the statistics by.",
      " This error is most frequently caused by referencing an old",
      " variable name that doesn't exist anymore"
    )
  }

  # If there is a label for the variable
  # that one should be used otherwise go
  # with the name of the variable
  if (label(x) == "") {
    name <- paste0(deparse(substitute(x)), collapse = "")
  } else {
    name <- label(x)
  }

  if (is.logical(x)) {
    x <- factor(x, levels = c(TRUE, FALSE))
  } else if (is.character(x) && any(table(x, by) == 0)) {
    x <- factor(x)
  }

  # Check missing -
  # Send a warning, since the user might be unaware of this
  # potentially disturbing fact. The dataset should perhaps by
  # subsetted by is.na(by) == FALSE
  if (any(is.na(by))) {
    warning(
      sprintf("Your 'by' variable has %d missing values", sum(is.na(by))),
      "\n   The corresponding 'x' and 'by' variables are automatically removed"
    )
    x <- x[!is.na(by)]
    if (inherits(x, "factor")) {
      x <- factor(x)
    }
    by <- by[!is.na(by)]
    if (inherits(by, "factor")) {
      by <- factor(by)
    }
  }

  if (useNA == "ifany" &&
    any(is.na(x))) {
    useNA <- "always"
  }

  # If all values are to be shown then simply use
  # the factors function, provided describeProp was specified
  if (show_all_values & deparse(substitute(prop_fn)) == "describeProp") {
    prop_fn <- describeFactors
  }

  addEmptyValuesToMakeListCompatibleWithMatrix <- function(t) {
    # Convert the list into a list with vectors instead of matrices
    for (n in names(t)) {
      if (is.matrix(t[[n]])) {
        tmp_names <- rownames(t[[n]])
        t[[n]] <- as.vector(t[[n]])
        names(t[[n]]) <- tmp_names
      }
    }

    # TODO: This function does not respect the order in
    # the factored variable. This could potentially be
    # a problem although probably more theoretical
    all_row_names <- c()
    for (n in names(t)) {
      all_row_names <- union(all_row_names, names(t[[n]]))
    }

    # No rownames exist, this occurs often
    # when there is only one row and that row doesn't
    # have a name
    if (is.null(all_row_names)) {
      return(t)
    }

    # The missing NA element should always be last
    if (any(is.na(all_row_names))) {
      all_row_names <- append(all_row_names[is.na(all_row_names) == FALSE], NA)
    }

    ret <- list()
    for (n in names(t)) {
      # Create an empty array
      ret[[n]] <- rep(missing_value, times = length(all_row_names))
      names(ret[[n]]) <- all_row_names
      # Loop and add all the values
      for (nn in all_row_names) {
        if (nn %in% names(t[[n]])) {
          if (is.na(nn)) {
            ret[[n]][is.na(names(ret[[n]]))] <- t[[n]][is.na(names(t[[n]]))]
          } else {
            ret[[n]][nn] <- t[[n]][nn]
          }
        }
      }
    }

    return(ret)
  }

  if (is.numeric(x)) {
    # If the numeric has horizontal_proportions then it's only so in the
    # missing category
    if (hrzl_prop) {
      t <- by(x, by,
        FUN = continuous_fn, html = html,
        digits = digits,
        digits.nonzero = digits.nonzero,
        number_first = numbers_first,
        useNA = useNA,
        useNA.digits = useNA.digits,
        horizontal_proportions = table(is.na(x), useNA = useNA),
        percentage_sign = percentage_sign
      )
    } else {
      t <- by(x, by,
        FUN = continuous_fn, html = html,
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

    if (length(t[[1]]) != 1) {
      fn_name <- deparse(substitute(continuous_fn))
      if (fn_name == "describeMean") {
        names(t[[1]][1]) <- "Mean"
      } else if (fn_name == "describeMedian") {
        names(t[[1]][1]) <- "Median"
      } else {
        names(t[[1]][1]) <- fn_name
      }
    }
  } else if ((!is.factor(x) &&
    length(unique(na.omit(x))) == 2) ||
    (is.factor(x) &&
      length(levels(x)) == 2) &&
      hrzl_prop == FALSE) {
    default_ref <- prDescGetAndValidateDefaultRef(x, default_ref)

    t <- by(x, by,
      FUN = prop_fn, html = html,
      digits = digits,
      digits.nonzero = digits.nonzero,
      number_first = numbers_first,
      useNA = useNA,
      useNA.digits = useNA.digits,
      default_ref = default_ref, percentage_sign = percentage_sign
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
      # Set the rowname to a special format
      # if there was missing and this is an matrix
      # then we should avoid using this format
      name <- sprintf(
        "%s %s",
        capitalize(levels(x)[default_ref]),
        tolower(name)
      )
    }

    if (NEJMstyle) {
      # LaTeX needs an escape before %
      # or it marks the rest of the line as
      # a comment. This is not an issue with
      # html (markdown)
      percent_sign <- ifelse(html, "%", "\\%")

      if (numbers_first) {
        name <- sprintf("%s - no (%s)", name, percent_sign)
      } else {
        name <- sprintf("%s - %s (no)", name, percent_sign)
      }
    }

    # If this is the only row then set that row to the current name
    if (length(t[[1]]) == 1) {
      names(t[[1]][1]) <- name
    }
  } else {
    # Make sure that the total isn't using proportions (happens with hrzl_prop)
    prop_fn <- factor_fn
    if (hrzl_prop) {
      t <- by(x, by,
        FUN = factor_fn, html = html,
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
        FUN = factor_fn, html = html,
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
  }

  # Convert the list into a matrix compatible format
  t <- addEmptyValuesToMakeListCompatibleWithMatrix(t)
  # Convert into a matrix
  results <- matrix(unlist(t), ncol = length(t))


  getHeader <- function(tbl_cnt, header_count, html) {
    if (missing(header_count) ||
      identical(header_count, FALSE)) {
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

  cn <- getHeader(table(by), header_count, html)


  # Add the proper rownames
  if ("matrix" %in% class(t[[1]])) {
    rownames(results) <- rownames(t[[1]])
  } else {
    rownames(results) <- names(t[[1]])
  }

  # This is an effect from the numeric variable not having
  # a naming function
  if (is.null(rownames(results)) && nrow(results) == 1) {
    rownames(results) <- name
  }

  if (!missing(add_total_col) &&
    add_total_col != FALSE) {
    total_table <- prGetStatistics(x[is.na(by) == FALSE],
      numbers_first = numbers_first,
      show_perc = total_col_show_perc,
      show_all_values = show_all_values,
      useNA = useNA,
      useNA.digits = useNA.digits,
      html = html,
      digits = digits,
      continuous_fn = continuous_fn,
      factor_fn = factor_fn,
      prop_fn = prop_fn,
      percentage_sign = percentage_sign
    )

    if (!is.matrix(total_table)) {
      total_table <- matrix(total_table, ncol = 1, dimnames = list(names(total_table)))
    }

    if (nrow(total_table) != nrow(results)) {
      stop(
        "There is a discrepancy in the number of rows in the total table",
        " and the by results: ", nrow(total_table), " total vs ", nrow(results), " results",
        "\n Rows total:", paste(rownames(total_table), collapse = ", "),
        "\n Rows results:", paste(rownames(results), collapse = ", ")
      )
    }

    cn_tot <- getHeader(
      c(Total = length(x[is.na(by) == FALSE])),
      header_count, html
    )
    if (add_total_col != "last") {
      results <- cbind(total_table, results)
      cn <- c(cn_tot, cn)
    } else {
      results <- cbind(results, total_table)
      cn <- c(cn, cn_tot)
    }
  }

  if (isTRUE(use_units)) {
    if (units(x) != "") {
      unitcol <- rep(sprintf("%s", units(x)), times = NROW(results))
      unitcol[rownames(results) == "Missing"] <- ""
    } else {
      unitcol <- rep("", times = NROW(results))
    }
    if (length(unitcol) != nrow(results)) {
      stop(
        "There is an discrepancy in the number of rows in the units",
        " and the by results: ", length(unitcol), " units vs ", nrow(results), " results",
        "\n Units:", paste(unitcol, collapse = ", "),
        "\n Rows results:", paste(rownames(results), collapse = ", ")
      )
    }
    results <- cbind(results, unitcol)
    cn <- c(cn, "units")
  } else if (use_units == "name") {
    if (units(x) != "") {
      name <- sprintf("%s (%s)", name, units(x))
    }
  }

  if (is.function(statistics)) {
    if (is.numeric(pval) &&
      pval <= 1 &&
      pval >= 0) {
      pval <- txtPval(pval,
        lim.sig = statistics.sig_lim,
        lim.2dec = statistics.two_dec_lim,
        html = html
      )
      results <- cbind(results, c(pval, rep("", nrow(results) - 1)))
      cn <- c(cn, "P-value")
    } else if (is.character(pval) && !is.null(attr(pval, "colname"))) {
      results <- cbind(results, c(pval, rep("", nrow(results) - 1)))
      cn <- c(cn, attr(pval, "colname"))
    } else {
      stop("Your statistics function should either return a numerical value from 0 to 1 or a character with the attribute 'colname'")
    }
  }

  colnames(results) <- cn

  # Even if one row has the same name this doesn't matter
  # at this stage as it is information that may or may
  # not be used later on
  label(results) <- name

  return(results)
}
