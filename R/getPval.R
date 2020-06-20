#' P-value extractors for \code{\link{getDescriptionStatsBy}}
#'
#' These functions are the base functions for getting the description
#' p-values. You can provide your own functions but all functions should
#' take two arguments and return a p-value (numeric, non-formatted)
#'
#' @param x The main variable of interest
#' @param by The variable for the stratification
#' @return \code{numeric} Returns the p-value from that particular test
#' @rdname getPval
#' @export
#' @family descriptive functions
#'
#' @section getPvalWilcox:
#'
#' Performs a two-sample two-sided Wilcoxon test (also known as the Mann-Whitney test),
#' see \code{\link[stats]{wilcox.test}}.
#'
#' @examples
#' set.seed(123)
#' getPvalFisher(
#'   sample(letters[1:3], size = 100, replace = TRUE),
#'   sample(LETTERS[1:3], size = 100, replace = TRUE)
#' )
#' getPvalWilcox(
#'   rnorm(100),
#'   sample(LETTERS[1:2], size = 100, replace = TRUE)
#' )
#' @importFrom stats wilcox.test
getPvalWilcox <- function(x, by) {
  wilcox.test(x ~ by, alternative = "two.sided")$p.value
}

#' @section getPvalAnova:
#'
#' Performs a standard Analysis of Variance model
#' through \code{\link[stats]{anova}(\link[stats]{lm}(x ~ by))}
#'
#' @rdname getPval
#' @import magrittr
#' @importFrom stats lm anova
#' @export
getPvalAnova <- function(x, by) {
  out <- lm(x ~ by) %>%
    anova()
  out[["Pr(>F)"]][[1]]
}

#' @rdname getPval
#'
#' @section getPvalFisher:
#' Performs Fisher's exact test through the \code{\link[stats]{fisher.test}}.
#'
#' @importFrom stats fisher.test
#' @export
getPvalFisher <- function(x, by) {
  # This is a quick fix in case of large dataset
  workspace <- 10^5
  if (length(x) * length(levels(x)) > 10^4) {
    workspace <- 10^7
  }

  # Large statistics tend to be very heavy and therefore
  # i need to catch errors in fisher and redo by simulation
  tryCatch(
    {
      fisher.test(x, by, workspace = workspace)$p.value
    },
    error = function(err) {
      warning("Simulating p-value for fisher due to high computational demands on the current varible")
      fisher.test(x, by, simulate.p.value = TRUE, B = 10^5)$p.value
    }
  )
}

#' @section getPvalChiSq:
#'
#' Performs a standard Chi-Squares analysis
#' through \code{\link[stats]{chisq.test}}
#'
#' @rdname getPval
#' @importFrom stats chisq.test
#' @export
getPvalChiSq <- function(x, by) {
  chisq.test(x, by)$p.value
}


#' @section getPvalKruskal:
#'
#' Performs a  Kruskal-Wallis rank sum test
#' through \code{\link[stats]{kruskal.test}}
#'
#' @importFrom stats kruskal.test
#' @rdname getPval
#' @export
getPvalKruskal <- function(x, by) {
  kruskal.test(x, by)$p.value
}