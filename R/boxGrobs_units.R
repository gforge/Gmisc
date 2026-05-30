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
#' MidDistanceX(grid::unit(.1, "npc"), grid::unit(.8, "npc"))
MidDistanceX <- function(u1, u2) {
  if (!grid::is.unit(u1)) u1 <- grid::unit(u1, "npc")
  if (!grid::is.unit(u2)) u2 <- grid::unit(u2, "npc")
  (u1 + u2) * 0.5
}

#' @rdname MidDistanceX
#' @export
MidDistanceY <- MidDistanceX
