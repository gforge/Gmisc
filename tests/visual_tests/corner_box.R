# Visual test: fixed corner radii via box_fn_args
#
# Run this script from the package root:
#   source("tests/visual_tests/corner_box.R")
#
# What to look for:
#   Row 1 (DEFAULT): now also uses r = unit(5, "pt") as the new built-in default --
#     both boxes have identical corner radii right out of the box.
#   Row 2 (EXPLICIT): same result spelled out explicitly per-box via
#     box_fn_args = list(r = unit(5, "pt")).
#   Row 3 (OLD DEFAULT): what the old snpc default looked like -- the large box
#     has noticeably rounder corners than the small one.

library(Gmisc)
library(grid)

grid.newpage()

# Title
grid.text(
  "Fixed corner radii - default is now r = unit(5, 'pt')",
  x = 0.5, y = 0.97,
  gp = gpar(fontsize = 13, fontface = "bold")
)

small_label <- "Small box"
big_label   <- paste0("Large box\n", paste(rep("The quick brown fox jumped over the lazy dog", 2), collapse = "\n"))

# -- Row 1: new default (5pt) ---------------------------------------------------
grid.text(
  "Default (r = unit(5, 'pt')) - identical corners regardless of size:",
  x = 0.03, y = 0.88, just = "left",
  gp = gpar(fontsize = 9, col = "grey30")
)
grid.draw(boxGrob(small_label, x = unit(0.22, "npc"), y = unit(0.77, "npc")))
grid.draw(boxGrob(big_label,   x = unit(0.70, "npc"), y = unit(0.77, "npc")))

# -- Row 2: explicit per-box override -------------------------------------------
grid.text(
  "Explicit (box_fn_args = list(r = unit(5, 'pt'))) - same effect:",
  x = 0.03, y = 0.57, just = "left",
  gp = gpar(fontsize = 9, col = "grey30")
)
r_fixed <- list(r = unit(5, "pt"))
grid.draw(boxGrob(small_label, x = unit(0.22, "npc"), y = unit(0.46, "npc"), box_fn_args = r_fixed))
grid.draw(boxGrob(big_label,   x = unit(0.70, "npc"), y = unit(0.46, "npc"), box_fn_args = r_fixed))

# -- Row 3: old snpc default (for comparison) -----------------------------------
grid.text(
  "Old default (r = unit(0.1, 'snpc')) - corners scaled with box size:",
  x = 0.03, y = 0.27, just = "left",
  gp = gpar(fontsize = 9, col = "grey30")
)
grid.draw(boxGrob(small_label, x = unit(0.22, "npc"), y = unit(0.16, "npc"), box_fn_args = list(r = unit(0.1, "snpc"))))
grid.draw(boxGrob(big_label,   x = unit(0.70, "npc"), y = unit(0.16, "npc"), box_fn_args = list(r = unit(0.1, "snpc"))))
