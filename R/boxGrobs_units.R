#' Calculate midpoint between two units
#'
#' Calculates the midpoint between two units. If the arguments are
#' numeric they are converted to npc units.
#'
#' @param u1 The first unit
#' @param u2 The second unit
#' @return A unit object
#' @export
#' @examples
#' MidDistanceX(unit(.1, "npc"), unit(.8, "npc"))
MidDistanceX <- function(u1, u2) {
  if (!is.unit(u1)) u1 <- unit(u1, "npc")
  if (!is.unit(u2)) u2 <- unit(u2, "npc")
  (u1 + u2) * 0.5
}

#' @rdname MidDistanceX
#' @export
MidDistanceY <- MidDistanceX
