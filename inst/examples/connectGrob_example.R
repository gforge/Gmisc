library(grid)

# In interactive sessions, pause between pages produced by this example.
if (interactive()) {
  old_ask <- grDevices::devAskNewPage(TRUE)
  on.exit(grDevices::devAskNewPage(old_ask), add = TRUE)
}

grid.newpage()

# Build a flowchart object
boxes <- flowchart(
  start = boxGrob("Top", x = .5, y = .8),
  end = boxGrob("Bottom", x = .5, y = .2),
  side = boxPropGrob("Side", "Left", "Right", prop = .3, x = .2, y = .8),
  exclude = boxGrob("Exclude:\n - Too sick\n - Prev. surgery", x = .8, y = .5, just = "left")
)

# Connect using the pipe-friendly S3 API
# Note: type = "vertical" connects top/bottom faces with a straight segment;
# if x positions differ, the segment may be diagonal.
boxes <- boxes |>
  connect(from = "start", to = "end", type = "vertical") |>
  connect(from = "start", to = "side", type = "horizontal") |>
  connect(from = "start", to = "exclude", type = "L")

print(boxes)

# Start a fresh page for split-box connector examples.
grid.newpage()

# We can also connect to/from lists
side_boxes <- list(
  left = boxGrob("Left", x = attr(boxes$side, "coords")$left_x, y = .5),
  right = boxGrob("Right", x = attr(boxes$side, "coords")$right_x, y = .5)
)

connectGrob(boxes$side, side_boxes$left, "v", "l")
connectGrob(boxes$side, side_boxes$right, "v", "r")

# Start a fresh page for fan-in example.
grid.newpage()

# Fan-in center example: multiple starts into one center bus and single trunk
flowchart(
  start = boxes$start,
  S2 = boxGrob("S2", x = .3, y = .7),
  S3 = boxGrob("S3", x = .7, y = .7),
  end = boxes$end
) |>
  connect(from = c("start", "S2", "S3"), to = "end", type = "fan_in_center") |>
  print()

# Start a fresh page for axis-preserving connector examples.
grid.newpage()

# vertical_axis is the axis-preserving alternative: it preserves each source's
# x-position when landing on the boundary of a wide shared target.
flowchart(
  A = boxGrob("A", x = .25, y = .75),
  B = boxGrob("B", x = .50, y = .75),
  C = boxGrob("C", x = .75, y = .75),
  target = boxGrob("Shared target", x = .5, y = .2, width = unit(.75, "npc"))
) |>
  connect(from = c("A", "B", "C"), to = "target", type = "vertical_axis") |>
  print()

grid.newpage()

# horizontal_axis is the axis-preserving alternative: it preserves each
# source's y-position when landing on the boundary of a tall shared target.
flowchart(
  A = boxGrob("A", x = .20, y = .25),
  B = boxGrob("B", x = .20, y = .50),
  C = boxGrob("C", x = .20, y = .75),
  target = boxGrob("Shared\ntarget", x = .8, y = .5, height = unit(.75, "npc"))
) |>
  connect(from = c("A", "B", "C"), to = "target", type = "horizontal_axis") |>
  print()

# Start a fresh page for spread/assignment example.
grid.newpage()

# When using spread/align, use the returned objects for connectors
visits <- flowchart(
  visit1 = boxGrob("Visit 1", x = .1, y = .35),
  visit2 = boxGrob("Visit 2", x = .2, y = .35),
  visit3 = boxGrob("Visit 3", x = .8, y = .35)
)

# Incorrect pattern (no assignment): spread result is discarded
visits |>
  spread(axis = "x", from = .05, to = .95, type = "between")

# This connector uses original coordinates because 'visits' was unchanged
connectGrob(visits$visit1, visits$visit2, type = "horizontal")

# Correct pattern: assign returned boxes and connect those
visits_spread <- visits |>
  spread(axis = "x", from = .05, to = .95, type = "between")
connectGrob(visits_spread$visit1, visits_spread$visit2, type = "horizontal")

# Print the boxes
boxes
side_boxes
visits
visits_spread
