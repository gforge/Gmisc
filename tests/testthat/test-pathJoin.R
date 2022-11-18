library('testthat')
context('pathJoin')

test_that("pathJoin basics", {
  expect_equal(pathJoin("one", "two/three", "four"),
               file.path("one", "two/three", "four"))
  expect_equal(pathJoin("one", "two//three/", "four"),
               file.path("one", "two/three", "four"))
})

test_that("join single param", {
  expect_equal(pathJoin(LETTERS[1:3]),
               do.call(file.path, as.list(LETTERS[1:3])))
})

test_that("pathJoin with vector", {
  expect_equal(pathJoin("base/", c("//sub1", "sub2")),
               c("base/sub1", "base/sub2"))

  expect_equal(pathJoin(c("base1/", "base2"), c("//sub1", "sub2")),
               c("base1/sub1", "base2/sub2"))

  expect_equal(pathJoin(c("base1/", "base2"), c("//sub1", "sub2"), c("final")),
               c("base1/sub1/final", "base2/sub2/final"))

  expect_error(pathJoin(LETTERS[1:2], LETTERS[1:3]))
  expect_error(pathJoin(LETTERS[1:2], LETTERS[1:3], LETTERS[1]))
})

test_that("Works on a data.frame", {
  base_dir <- "/home/tester/images"
  out <- data.frame(filename = c("file1.png", "file2.png", "file3.png")) |>
    dplyr::mutate(full_path = pathJoin(base_dir, filename))
  expect_equal(out$full_path,
               c("file1.png", "file2.png", "file3.png") |> sapply(\(x) file.path(base_dir, x),
                                                                  USE.NAMES = FALSE))
})
