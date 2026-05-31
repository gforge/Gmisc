library(grid)
grid.newpage()

# Create a set of boxes
start <- boxGrob("Top", x = .5, y = .8)
end <- boxGrob("Bottom", x = .5, y = .2)
side <- boxPropGrob("Side", "Left", "Right", prop = .3, x = .2, y = .8)
exclude <- boxGrob("Exclude:\n - Too sick\n - Prev. surgery", x = .8, y = .5, just = "left")

# We can chain the spread operations from a flowchart object
flowchart(
  start = start,
  middle = list(side, exclude),
  end = end
) |>
  spread(axis = "y") |>
  spread(axis = "x", subelement = "middle", from = 0.2, to = 0.8)

# Use device-level paging in interactive sessions so users can inspect the first plot
# This will prompt before creating the next page; we restore the previous setting afterwards
if (interactive()) {
  old_ask <- grDevices::devAskNewPage(TRUE)
  on.exit(grDevices::devAskNewPage(old_ask), add = TRUE)
  grid.newpage() # will prompt on interactive devices
} else {
  # non-interactive: just start a fresh page for the next example
  grid.newpage()
}

# Example: spread a nested subelement by deep path and print sequentially
flowchart(
  anchor = boxGrob("Anchor", x = .5, y = .15),
  grp = list(middle = list(side, exclude))
) |>
  spread(axis = "x", subelement = c("grp", "middle"), from = 0.2, to = 0.8)
