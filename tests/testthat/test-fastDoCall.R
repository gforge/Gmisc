context("Check fastDoCall")

test_that("Compatibility with do.call examples", {
  expect_equal(
    fastDoCall("complex", list(imag = 1:3)),
    do.call("complex", list(imag = 1:3))
  )

  ## if we already have a list (e.g. a data frame)
  ## we need c() to add further arguments
  tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
  expect_equal(
    fastDoCall("paste", c(tmp, sep = "")),
    do.call("paste", c(tmp, sep = ""))
  )

  # Does not work: do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)

  ## examples of where objects will be found.
  A <- 2
  f <- function(x) x^2
  env <- new.env()
  assign("A", 10, envir = env)
  assign("f", f, envir = env)
  f <- function(x) x
  expect_equal(
    fastDoCall("f", list(A)),
    do.call("f", list(A))
  )
  expect_equal(
    fastDoCall("f", list(A), envir = env),
    do.call("f", list(A), envir = env)
  )
  expect_equal(
    fastDoCall(f, list(A), envir = env),
    do.call(f, list(A), envir = env)
  )
  expect_equal(
    fastDoCall("f", list(quote(A)), envir = env),
    do.call("f", list(quote(A)), envir = env)
  )

  expect_equal(
    fastDoCall(f, list(quote(A)), envir = env),
    do.call(f, list(quote(A)), envir = env)
  )
  expect_equal(
    fastDoCall("f", list(as.name("A")), envir = env),
    do.call("f", list(as.name("A")), envir = env)
  )

  expect_equal(
    fastDoCall("base:::range.default", list(1:4)),
    c(1, 4)
  )
  expect_equal(
    fastDoCall(base:::range.default, list(1:4)),
    do.call(base:::range.default, list(1:4))
  )

  # Test for passing a data.frame
  expect_equal(fastDoCall(order, datasets::iris),
    do.call(order, datasets::iris),
    info = "Failed to accept data.frame passed by ::"
  )
})