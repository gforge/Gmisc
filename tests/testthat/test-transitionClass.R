library(testthat)
context("Transition class")

test_that("Check initialization, copy, and dimensions", {
  expect_error(getRefClass("Transition")$new(),
    regexp = "transition matrix"
  )

  trn_mtrx <- matrix(1, ncol = 2, nrow = 2, dimnames = list(LETTERS[1:2], letters[1:2]))
  a <- getRefClass("Transition")$new(trn_mtrx)
  expect_equal(length(a$transitions), 1)
  expect_equal(
    a$getDim(),
    dim(trn_mtrx)
  )

  expect_equal(
    a$noCols(),
    length(a$fill_clr)
  )
  expect_equal(
    a$noCols(),
    length(a$txt_clr)
  )
  expect_equal(
    a$noCols(),
    length(a$box_txt)
  )

  expect_error(a$addTransitions(trn_mtrx))

  trn_mtrx_2 <- matrix(1, ncol = 2, nrow = 2, dimnames = list(letters[1:2], LETTERS[3:4]))
  a$addTransitions(trn_mtrx_2)
  expect_equal(
    a$noCols(),
    3
  )

  trn_mtrx_3 <- matrix(1, ncol = 2, nrow = 2, dimnames = list(LETTERS[3:4], LETTERS[3:4]))
  b <- a$copy()
  a$addTransitions(trn_mtrx_3)
  expect_equal(
    b$getDim(),
    c(2, 3)
  )
  expect_equal(
    length(b$transitions),
    2
  )
  expect_equal(
    length(a$transitions),
    3
  )
})

test_that("Check box size", {
  trn_mtrx <- matrix(1:4, ncol = 2)
  rownames(trn_mtrx) <- c("Test A", "Test B")
  colnames(trn_mtrx) <- c("Test A", "Test B")
  a <- getRefClass("Transition")$new(trn_mtrx)
  expect_error(a$boxSizes())
  expect_equal(
    a$boxSizes(1),
    rowSums(trn_mtrx)
  )
  expect_equal(
    a$boxSizes(2),
    colSums(trn_mtrx)
  )

  trn_mtrx <- array(1:8, dim = c(2, 2, 2), dimnames = list(
    c("Test A", "Test B"),
    c("Test A", "Test B"),
    c("Test A", "Test B")
  ))
  a <- getRefClass("Transition")$new(trn_mtrx)
  expect_equivalent(
    a$boxSizes(1),
    rowSums(trn_mtrx[, , 1]) + rowSums(trn_mtrx[, , 2])
  )
  expect_equivalent(
    attr(a$boxSizes(1), "prop"),
    rowSums(trn_mtrx[, , 1]) / (rowSums(trn_mtrx[, , 1]) + rowSums(trn_mtrx[, , 2]))
  )
  expect_equivalent(
    a$boxSizes(2),
    colSums(trn_mtrx[, , 1]) + colSums(trn_mtrx[, , 2])
  )
  expect_equivalent(
    attr(a$boxSizes(2), "prop"),
    colSums(trn_mtrx[, , 1]) / (colSums(trn_mtrx[, , 1]) + colSums(trn_mtrx[, , 2]))
  )
})

test_that("Check advanced matrix dimensions", {
  # Setup test-data
  set.seed(1)
  library(magrittr)
  n <- 100
  data <-
    data.frame(
      Sex = sample(c("Male", "Female"),
        size = n,
        replace = TRUE,
        prob = c(.4, .6)
      ),
      Charnley_class = sample(c("A", "B", "C"),
        size = n,
        replace = TRUE
      )
    )
  getProbs <- function(Chrnl_name) {
    prob <- data.frame(
      A = 1 / 6 +
        (data$Sex == "Male") * .25 +
        (data$Sex != "Male") * -.25 +
        (data[[Chrnl_name]] %in% "B") * -.5 +
        (data[[Chrnl_name]] %in% "C") * -2,
      B = 2 / 6 +
        (data$Sex == "Male") * .1 +
        (data$Sex != "Male") * -.05 +
        (data[[Chrnl_name]] == "C") * -2,
      C = 3 / 6 +
        (data$Sex == "Male") * -.25 +
        (data$Sex != "Male") * .25
    )

    # Remove negative probabilities
    t(apply(prob, 1, function(x) {
      if (any(x < 0)) {
        x <- x - min(x) + .05
      }
      x
    }))
  }

  Ch_classes <- c("Charnley_class")
  Ch_classes %<>% c(sprintf("%s_%dyr", Ch_classes, c(1, 2, 6)))
  for (i in 1:length(Ch_classes)) {
    if (i == 1) {
      next
    }

    data[[Ch_classes[i]]] <-
      apply(getProbs(Ch_classes[i - 1]), 1, function(p) {
        sample(c("A", "B", "C"),
          size = 1,
          prob = p
        )
      }) %>%
      factor(levels = c("A", "B", "C"))
  }

  test_3D <- with(data, table(Charnley_class, Charnley_class_1yr, Sex))
  transitions <- getRefClass("Transition")$new(test_3D)

  expect_equivalent(
    transitions$getDim(),
    dim(test_3D)
  )

  expect_equivalent(
    dim(transitions$fill_clr[[1]]),
    c(transitions$noRows(1), 2)
  )
  expect_equivalent(
    dim(transitions$txt_clr[[1]]),
    c(
      transitions$noRows(1),
      2
    )
  )

  expect_equivalent(
    sapply(transitions$box_txt, length),
    transitions$noRows()
  )

  add_3D <- with(data, table(Charnley_class_1yr, Charnley_class_2yr, Sex))
  transitions$addTransitions(add_3D)

  expect_equal(
    dim(transitions$fill_clr[[1]]),
    c(
      transitions$noRows(1),
      2
    )
  )
  expect_equal(
    dim(transitions$txt_clr[[1]]),
    c(
      transitions$noRows(1),
      2
    )
  )

  expect_equivalent(
    sapply(transitions$box_txt, length),
    transitions$noRows()
  )


  data$Charnley_class_6yr[data$Charnley_class_6yr == "A"] <- "B"
  add_3D <- with(data, table(Charnley_class_2yr, Charnley_class_6yr, Sex))
  transitions$addTransitions(add_3D)

  expect_equal(
    transitions$noCols(),
    4
  )
})