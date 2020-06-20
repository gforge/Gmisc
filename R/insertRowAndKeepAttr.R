#' Insert a row into a matrix
#'
#' Inserts a row and keeps the attributes \code{\link{copyAllNewAttributes}}
#'
#' @param m matrix
#' @param r row number where the new row should be inserted
#' @param v optional values for the new row
#' @param rName optional character string: the name of the new row.
#' @return \code{matrix} Returns a matrix with one more row than the provided matrix \code{m}
#'
#' @examples
#' test <- matrix(1:4, ncol = 2)
#' attr(test, "wow") <- 1000
#' test <- insertRowAndKeepAttr(test, 2)
#' print(attr(test, "wow"))
#' @export
#' @author Max Gordon, Arne Henningsen
insertRowAndKeepAttr <- function(m, r, v = NA, rName = "") {
  if (!inherits(m, "matrix")) {
    stop("argument 'm' must be a matrix")
  }
  if (r == as.integer(r)) {
    r <- as.integer(r)
  }
  else {
    stop("argument 'r' must be an integer")
  }
  if (length(r) != 1) {
    stop("argument 'r' must be a scalar")
  }
  if (r < 1) {
    stop("argument 'r' must be positive")
  }
  if (r > nrow(m) + 1) {
    stop(
      "argument 'r' must not be larger than the number of rows",
      " of matrix 'm' plus one"
    )
  }
  if (!is.character(rName)) {
    stop("argument 'rName' must be a character string")
  }
  if (length(rName) != 1) {
    stop("argument 'rName' must be a be a single character string")
  }
  nr <- nrow(m)
  nc <- ncol(m)
  rNames <- rownames(m)
  if (is.null(rNames) & rName != "") {
    rNames <- rep("", nr)
  }
  if (r == 1) {
    m2 <- rbind(matrix(v, ncol = nc), m)
    if (!is.null(rNames)) {
      rownames(m2) <- c(rName, rNames)
    }
  }
  else if (r == nr + 1) {
    m2 <- rbind(m, matrix(v, ncol = nc))
    if (!is.null(rNames)) {
      rownames(m2) <- c(rNames, rName)
    }
  }
  else {
    m2 <- rbind(
      m[1:(r - 1), , drop = FALSE], matrix(v, ncol = nc),
      m[r:nr, , drop = FALSE]
    )
    if (!is.null(rNames)) {
      rownames(m2) <- c(rNames[1:(r - 1)], rName, rNames[r:nr])
    }
  }

  return(copyAllNewAttributes(from = m, to = m2))
}