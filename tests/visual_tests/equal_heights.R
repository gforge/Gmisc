# Visual test: equalizeHeights()
#
# Run this script from the package root:
#   source("tests/visual_tests/equal_heights.R")
#
# What to look for:
#   Top half (BEFORE): the two "groups2" boxes have very different heights
#     because one has much more text — mirroring the issue reporter's flowchart.
#   Bottom half (AFTER): equalizeHeights(subelement = "groups2") sets both
#     boxes to the height of the taller one, giving a uniform row.

library(Gmisc)
library(grid)
library(glue)

fc <- flowchart(
  rando = glue("Randomised N = 100"),
  groups = list(
    glue("Group 1\nn = 50"),
    glue("Group 2\nn = 50")
  ),
  groups2 = list(
    glue("Excluded\nn = 1"),
    glue("Excluded\nn = 2\n\nThe quick brown fox\njumped over the lazy dog\n\n\n")
  )
) |>
  spread(axis = "y") |>
  spread(subelement = "groups",  axis = "x") |>
  spread(subelement = "groups2", axis = "x")

grid.newpage()

# Title
grid.text(
  "equalizeHeights(subelement = 'groups2')",
  x = 0.5, y = 0.97,
  gp = gpar(fontsize = 13, fontface = "bold")
)

# -- Before --------------------------------------------------------------------
grid.text(
  "Before - groups2 boxes differ in height:",
  x = 0.03, y = 0.91, just = "left",
  gp = gpar(fontsize = 9, col = "grey30")
)
pushViewport(viewport(x = 0.5, y = 0.70, width = 0.92, height = 0.38))
print(fc)
popViewport()

# -- After ---------------------------------------------------------------------
grid.text(
  "After - both groups2 boxes share the height of the taller one:",
  x = 0.03, y = 0.48, just = "left",
  gp = gpar(fontsize = 9, col = "grey30")
)
fc2 <- fc |> equalizeHeights(subelement = "groups2")
pushViewport(viewport(x = 0.5, y = 0.24, width = 0.92, height = 0.44))
print(fc2)
popViewport()
