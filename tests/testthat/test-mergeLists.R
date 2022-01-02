library("testthat")
context("mergeLists")

test_that("Basic merge", {
  v1 <- list(a = c(3, 2, 1))
  v2 <- list(a = 3)
  merged <- mergeLists(v1, v2)
  expect_equal(length(merged$a), 4)
  expect_equal(all(merged$a %in% 1:3), TRUE)

  v1 <- list(a = c(3, 2, 1), b = 1)
  v2 <- list(a = 3, b = list("incompatible"))
  expect_error(mergeLists(v1, v2),
               regexp = "incompatible",
               info = "Can't merge a list item with a non-list item")

  v1 <- list("a" = c(1, 2), b = "test 1", sublist = list(one = 20:21, two = 21:22))
  v2 <- list("a" = c(3, 4), b = "test 2", sublist = list(one = 10:11, two = 11:12, three = 1:2))
  merged <- mergeLists(v1, v2)
  expect_equal(dim(merged$a), NULL,
    info = "Two vectors should be appended to eachother"
  )
  expect_equal(merged$a[1:2], v1$a,
    info = "The first vector should in the beginning"
  )
  expect_equal(merged$a[3:4], v2$a,
    info = "The second vector should be in the end"
  )
  expect_equal(length(merged$a), 4,
    info = "Two vectors of length 2 should be bound by c generating a 4 length vector"
  )

  expect_equal(length(merged$b), 2,
    info = "Two elements should be c() together into a length of 2"
  )
  expect_equal(merged$b[1], v1$b,
    info = "The first element should be in the first position"
  )
  expect_equal(merged$b[2], v2$b,
    info = "The second element should be in the second position"
  )

  expect_equal(length(merged$sublist), 3,
    info = "Two sub-lists should merge and all elements should be included"
  )

  expect_equal(merged$sublist$one[1:2], v1$sublist$one,
    info = "The vector merge should work with sublists as well"
  )
  expect_equal(merged$sublist$one[3:4], v2$sublist$one,
    info = "The vector merge should work with sublists as well"
  )
  expect_equal(merged$sublist$two[1:2], v1$sublist$two,
    info = "The vector merge should work with sublists as well"
  )
  expect_equal(merged$sublist$two[3:4], v2$sublist$two,
    info = "The vector merge should work with sublists as well"
  )
  expect_equal(merged$sublist$three[3:4], v2$sublist$three,
    info = "The vector merge should work with sublists as well"
  )
  expect_true(all(is.na(merged$sublist$three[1:2])),
    info = "The matrix should be empty at the row for the one without those elements"
  )
})

test_that("Tricky merge", {
  v1 <- list(a = 1)
  v2 <- list(a = matrix(c(3, 4), nrow = 1))
  expect_error(mergeLists(v1, v2),
    info = "It shouldn't work if the elements don't match"
  )

  v1 <- list(a = rbind(c(1, 2), c(11, 12)))
  v2 <- list(a = c(3, 4))
  merged <- mergeLists(v1, v2)
  expect_equal(nrow(merged$a), 3,
    info = "If you have one matrix and one vector that have compatible lengths they should match"
  )

  expect_equal(merged$a[1:2, ], v1$a,
    info = "There is something strange with the matrix + vector match"
  )
  expect_equal(merged$a[3, ], v2$a,
    info = "There is something strange with the matrix + vector match"
  )

  v1 <- list(a = rbind(c(1, 2), c(11, 12), c(13, 14)))
  v2 <- list(a = c(3, 4, 5))
  expect_error(mergeLists(v1, v2),
    info = "It shouldn't work if the elements don't match"
  )

  v1 <- list(a = rbind(c(1, 2), c(11, 12), c(13, 14)))
  v2 <- list(a = rbind(c(3, 4), c(31, 32)))
  merged <- mergeLists(v1, v2)
  expect_equal(merged$a[1:3, ], v1$a)
  expect_equal(merged$a[4:5, ], v2$a)
})
