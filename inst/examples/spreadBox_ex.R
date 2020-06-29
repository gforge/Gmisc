library(grid)
grid.newpage()

box1 <- boxGrob("B1", x = .2, y = .8)
box2 <- boxGrob("B2\n\n\neach\nbox\neven\nspace\nbetween", x = .2, y = .8)
box3 <- boxGrob("B3", x = .2, y = .8)
box4 <- boxGrob("B4", x = .2, y = .8)
box5 <- boxGrob("B5", x = .2, y = .8)

spread_boxes <- spreadVertical(box1,
                               box2,
                               box3,
                               a = box4,
                               box5, 
                               .type = "between")
for (b in spread_boxes) {
  print(b)
}

box1 <- boxGrob("B1\n\nanother group\ncenter oriented", x = .6, y = .8)
box2 <- boxGrob("B2", x = .6, y = .8)
box3 <- boxGrob("B3", x = .6, y = .8)
box4 <- boxGrob("B4", x = .6, y = .8)
box5 <- boxGrob("B5", x = .6, y = .8)

spread_boxes <- spreadVertical(box1,
                               box2,
                               box3,
                               a = box4,
                               box5, 
                               .type = "center")
for (b in spread_boxes) {
  print(b)
}