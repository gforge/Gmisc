library(grid)
grid.newpage()

# Create a set of boxes
start <- boxGrob("Top", x = .5, y = .8)
end <- boxGrob("Bottom", x = .5, y = .2)
side <- boxPropGrob("Side", "Left", "Right", prop = .3, x = .2, y = .8)
exclude <- boxGrob("Exclude:\n - Too sick\n - Prev. surgery", x = .8, y = .5, just = "left")

# We can chain the spread operations
boxes <- spreadVertical(
  start = start,
  middle = list(side, exclude),
  end = end
) |>
  spreadHorizontal(.subelement = "middle", .from = 0.2, .to = 0.8)

# Now we can print them all at once
boxes
