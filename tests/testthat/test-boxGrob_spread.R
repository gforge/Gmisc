library(testthat)

test_that("Box spread vertically with center", {
  box1 <- boxGrob("B1", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3\ntricky", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)

  spread_boxes <- spreadVertical(box1,
                                 box2,
                                 box3,
                                 b = box4,
                                 c = box5, 
                                 .type = "center")
  sapply(spread_boxes, 
         function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    expect_equivalent(.2)

  sapply(spread_boxes, 
         function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    length() %>% 
    expect_equivalent(length(spread_boxes))
  convertY(coords(spread_boxes[[1]])$top, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 1,
                      tolerance = 1e-3)
  
  convertY(coords(tail(spread_boxes, 1)[[1]])$bottom, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 0,
                      tolerance = 1e-3)
  
  expect_equivalent(distance(spread_boxes[1:2], type = "v", center = TRUE),
                    distance(spread_boxes[2:3], type = "v", center = TRUE))

  expect_equivalent(distance(spread_boxes[3:4], type = "v", center = TRUE),
                    distance(spread_boxes[4:5], type = "v", center = TRUE))
})

test_that("Box spread vertically with between", {
  box1 <- boxGrob("B1", x = .2, y = .8)
  box2 <- boxGrob("B2\nmulti\n\n\nline", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5\ntricky", x = .2, y = .8)
  
  spread_boxes <- spreadVertical(box1,
                                 box2,
                                 box3,
                                 b = box4,
                                 c = box5, 
                                 .type = "between")
  sapply(spread_boxes, 
         function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    expect_equivalent(.2)
  
  sapply(spread_boxes, 
         function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    length() %>% 
    expect_equivalent(length(spread_boxes))
  convertY(coords(spread_boxes[[1]])$top, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 1,
                      tolerance = 1e-3)
  
  convertY(coords(tail(spread_boxes, 1)[[1]])$bottom, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 0,
                      tolerance = 1e-3)
  
  expect_equivalent(distance(spread_boxes[1:2], type = "v"),
                    distance(spread_boxes[2:3], type = "v"))
  
  expect_equivalent(distance(spread_boxes[3:4], type = "v"),
                    distance(spread_boxes[4:5], type = "v"))
})

test_that("Box spread horizontally with center", {
  box1 <- boxGrob("B1 some long text", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)
  
  spread_boxes <- spreadHorizontal(box1,
                                   a = box2,
                                   box3,
                                   b = box4,
                                   box5, 
                                   .type = "center")
  sapply(spread_boxes, 
         function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    expect_equivalent(.8)
  
  sapply(spread_boxes, 
         function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    length() %>% 
    expect_equivalent(length(spread_boxes))
  
  convertX(coords(spread_boxes[[1]])$left, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 0,
                      tolerance = 1e-3)
  
  convertX(coords(tail(spread_boxes, 1)[[1]])$right, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 1,
                      tolerance = 1e-3)
  
  expect_equivalent(distance(spread_boxes[1:2], type = "h", center = TRUE),
                    distance(spread_boxes[2:3], type = "h", center = TRUE))
  
  expect_equivalent(distance(spread_boxes[3:4], type = "h", center = TRUE),
                    distance(spread_boxes[4:5], type = "h", center = TRUE))
})


test_that("Box spread horizontally with between", {
  box1 <- boxGrob("B1 some long text", x = .2, y = .8)
  box2 <- boxGrob("B2", x = .2, y = .8)
  box3 <- boxGrob("B3", x = .2, y = .8)
  box4 <- boxGrob("B4", x = .2, y = .8)
  box5 <- boxGrob("B5", x = .2, y = .8)
  
  spread_boxes <- spreadHorizontal(box1,
                                   a = box2,
                                   box3,
                                   b = box4,
                                   box5, 
                                   .type = "between")
  sapply(spread_boxes, 
         function(b) coords(b)$y %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    expect_equivalent(.8)
  
  sapply(spread_boxes, 
         function(b) coords(b)$x %>% convertX(unitTo = "npc", valueOnly = TRUE)) %>% 
    unique() %>% 
    length() %>% 
    expect_equivalent(length(spread_boxes))
  
  convertX(coords(spread_boxes[[1]])$left, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 0,
                      tolerance = 1e-3)
  
  convertX(coords(tail(spread_boxes, 1)[[1]])$right, unitTo = "npc", valueOnly = TRUE) %>% 
    expect_equivalent(expected = 1,
                      tolerance = 1e-3)
  
  expect_equivalent(distance(spread_boxes[1:2], type = "h"),
                    distance(spread_boxes[2:3], type = "h"))
  
  expect_equivalent(distance(spread_boxes[3:4], type = "h"),
                    distance(spread_boxes[4:5], type = "h"))
})

