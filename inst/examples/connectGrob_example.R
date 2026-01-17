library(grid)
grid.newpage()

# Initiate the boxes that we want to connect
boxes <- list(
  start = boxGrob("Top", x = .5, y = .8),
  end = boxGrob("Bottom", x = .5, y = .2),
  side = boxPropGrob("Side", "Left", "Right", prop = .3, x = .2, y = .8),
  exclude = boxGrob("Exclude:\n - Too sick\n - Prev. surgery", x = .8, y = .5, just = "left")
)

# Connect the boxes and print/plot them
connectGrob(boxes$start, boxes$end, "vertical")
connectGrob(boxes$start, boxes$side, "horizontal")
connectGrob(boxes$start, boxes$exclude, "L")

# We can also connect to/from lists
side_boxes <- list(
  left = boxGrob("Left", x = attr(boxes$side, "coords")$left_x, y = .5),
  right = boxGrob("Right", x = attr(boxes$side, "coords")$right_x, y = .5)
)

connectGrob(boxes$side, side_boxes$left, "v", "l")
connectGrob(boxes$side, side_boxes$right, "v", "r")

# Fan-in center example: multiple starts into one center bus and single trunk
connectGrob(list(boxes$start, boxGrob("S2", x = .3, y = .7), boxGrob("S3", x = .7, y = .7)), boxes$end, type = "fan_in_center")

# Print the boxes
boxes
side_boxes
