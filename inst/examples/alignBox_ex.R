library(grid)
grid.newpage()

# Create a reference box
box <- boxGrob("A cool reference box",
  x = .5, y = .8,
  box_gp = gpar(fill = "#ADB5C7")
)

# Create a group of boxes to align
boxes <- list(
  another_box = boxGrob("A horizontal box", x = .1, y = .5),
  yet_another_box = boxGrob("Another horizontal box", x = .8, y = .3)
)

# Align the group and then individual boxes within that group
aligned_boxes <- boxes |>
  alignHorizontal(reference = box, .position = "right") |>
  alignVertical(reference = .5, .position = "center")

# Print the reference and the aligned boxes
box
aligned_boxes
