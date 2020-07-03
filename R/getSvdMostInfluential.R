#' Gets the maximum contributor variables from svd()
#'
#' This function is inspired by Jeff Leeks Data Analysis course where
#' he suggests that one way to use the \code{\link[base]{svd}} is to look
#' at the most influential rows for first columns in the V matrix.
#'
#' This function expands on that idea and adds the option of choosing
#' more than just the most contributing variable for each row. For instance
#' two variables may have a major impact on a certain component where the second
#' variable has 95% of the maximum variables value, since this variable is probably
#' important in that particular component it makes sense to include it
#' in the selection.
#'
#' It is of course useful when you have many continuous variables and you want
#' to determine a subgroup to look at, i.e. finding the needle in the haystack.
#'
#' @param mtrx A matrix or data frame with the variables. Note: if it contains missing
#'  variables make sure to impute prior to this function as the \code{\link[base]{svd}} can't
#'  handle missing values.
#' @param quantile The SVD D-matrix gives an estimate for the amount that is explained.
#'  This parameter is used for selecting the columns that have that quantile
#'  of explanation.
#' @param similarity_threshold A quantile for how close other variables have to be in value to
#'  maximum contributor of that particular column. If you only want the maximum value
#'  then set this value to 1.
#' @param plot_selection As this is all about variable exploring it is often interesting
#'  to see how the variables were distributed among the vectors
#' @param plot_threshold The threshold of the plotted bars, measured as
#'  percent explained by the D-matrix. By default it is set to 0.05.
#' @param varnames A vector with alternative names to the colnames
#' @return Returns a list with vector with the column numbers
#'  that were picked in the "most_influential" variable and the
#'  svd caluclation in the "svd"
#'
#' @example inst/examples/getSvdMostInfluential_example.R
#'
#' @import lattice
#' @importFrom grDevices colorRampPalette grey
#' @importFrom graphics par
#' @export
getSvdMostInfluential <- function(mtrx,
                                  quantile,
                                  similarity_threshold,
                                  plot_selection      = TRUE,
                                  plot_threshold      = 0.05,
                                  varnames            = NULL) {

  if (any(is.na(mtrx)))
    stop("Missing values are not allowed in this function. Impute prior to calling this function (try with mice or similar package).")

  if (quantile < 0 || quantile > 1)
    stop("The quantile mus be between 0-1")

  if (similarity_threshold < 0 || similarity_threshold > 1)
    stop("The similarity_threshold mus be between 0-1")

  svd_out <- svd(scale(mtrx))
  perc_explained <- svd_out$d^2/sum(svd_out$d^2)
  # Select the columns that we want to look at
  cols_expl <- which(cumsum(perc_explained) <= quantile)

  # Select the variables of interest
  getMostInfluentialVars <- function() {
    vars <- list()
    for (i in 1:length(perc_explained)) {
      v_abs <- abs(svd_out$v[,i])
      maxContributor <- which.max(v_abs)
      similarSizedContributors <- which(v_abs >= v_abs[maxContributor]*similarity_threshold)
      if (any(!similarSizedContributors %in% maxContributor)) {
        maxContributor <- similarSizedContributors[order(v_abs[similarSizedContributors], decreasing = TRUE)]
      }
      vars[[length(vars) + 1]] <- maxContributor
    }
    return(vars)
  }
  vars <- getMostInfluentialVars()

  plotSvdSelection <- function() {
    if (plot_threshold < 0 || plot_threshold > 1)
      stop("The plot_threshold must be between 0-1")

    if (plot_threshold > similarity_threshold)
      stop(paste0("You can't plot less that you've chosen - it makes no sense",
              " - the plot (", plot_threshold, ")",
              " >",
              " similarity (", similarity_threshold, ")"))

    # Show all the bars that are at least at the threshold
    # level and group the rest into a "other category"
    bar_count <- length(perc_explained[perc_explained >= plot_threshold]) + 1
    if (bar_count > length(perc_explained)) {
      bar_count <- length(perc_explained)
      plot_percent <- perc_explained
    }else{
      plot_percent <- rep(NA, times = bar_count)
      plot_percent <- perc_explained[perc_explained >= plot_threshold]
      plot_percent[bar_count] <- sum(perc_explained[perc_explained < plot_threshold])
    }

    # Create transition colors
    selected_colors <- colorRampPalette(c("darkgreen", "#FFFFFF"))(bar_count + 2)[cols_expl]
    nonselected_colors <- colorRampPalette(c("darkgrey", "#FFFFFF"))(bar_count + 2)[(max(cols_expl) + 1):bar_count]

    max_no_print <- 4
    names <- unlist(lapply(vars[1:bar_count], FUN = function(x) {
              if (is.null(varnames)) {
                varnames <- colnames(mtrx)
              }
              if (length(x) > max_no_print)
                ret <- paste(c(varnames[1:(max_no_print - 1)],
                        sprintf("+ %d other", length(x) + 1 - max_no_print)), collapse = "\n")
              else
                ret <- paste(varnames[x], collapse = "\n")

              return(ret)
            }))
    rotation <- 45 + (max(unlist(lapply(vars[1:bar_count],
                      function(x) {
                        min(length(x), max_no_print)
                      }))) - 1)*(45/max_no_print)

    if (bar_count < length(perc_explained)) {
      names[bar_count] <- "Other"
      nonselected_colors[length(nonselected_colors)] <- grey(.5)
    }

    las <- 2
    m <- par(mar = c(8.1, 4.1, 4.1, 2.1))
    on.exit(par(mar = m))

    p1 <- barchart(plot_percent * 100 ~ 1:bar_count,
        horiz = FALSE,
        ylab = "Percentage explained (%D)",
        main = "SVD - the maximum contributors defined by V column",
        xlab = "Pattern contributing variables",
        col = c(selected_colors, nonselected_colors),
        key = list(text = list(c("Selected", "Not selected")),
            rectangles = list(col = c("darkgreen", "#777777"))),
        scales = list(x = list(rot = rotation, labels = names)))
    print(p1)
  }

  if (plot_selection)
    plotSvdSelection()

  ret <- list(most_influential = unique(unlist(vars[cols_expl])),
      svd = svd_out)
  return(ret)
}
