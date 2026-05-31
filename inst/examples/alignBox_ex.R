library(grid)
grid.newpage()

# Create a reference box
box <- boxGrob("A cool reference box",
  x = .5, y = .8,
  box_gp = gpar(fill = "#ADB5C7")
)

# Create a group of boxes to align
boxes <- flowchart(
  another_box = boxGrob("A horizontal box", x = .1, y = .5),
  yet_another_box = boxGrob("Another horizontal box", x = .8, y = .3)
)

# Align the group in pipe style
aligned_boxes <- boxes |>
  align(axis = "x", reference = box, position = "right") |>
  align(axis = "y", reference = .5, position = "center")

# Example: align a nested element inside a complex list using a deep path
complex_list <- flowchart(
  arms = list(
    early = list(boxGrob("Early", x = .2, y = .4)),
    late = list(boxGrob("Late", x = .8, y = .2))
  ),
  detail = list(
    list(boxGrob("D_early", x = .1, y = .6)),
    list(boxGrob("D_late", x = .9, y = .1))
  )
)

# Align the first detail element to the early arm by deep path
complex_list <- complex_list |>
  align(
    axis = "x",
    reference = c("arms", "early"),
    position = "center",
    subelement = c("detail", 1)
  )

# Print the reference and the aligned boxes
box
aligned_boxes
complex_list
