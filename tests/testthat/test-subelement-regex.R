library(testthat)
library(grid)

# Helper: build a small flowchart with groups* and excl* elements
make_fc <- function() {
  flowchart(
    groups1 = list("A", "B"),
    groups2 = list("C", "D"),
    excl1   = list("X"),
    excl2   = list("Y"),
    other   = list("Z")
  ) |>
    spread(axis = "y", margin = unit(0.02, "npc")) |>
    spread(subelement = "groups1", axis = "x", margin = unit(.05, "npc")) |>
    spread(subelement = "groups2", axis = "x", margin = unit(.05, "npc"))
}

# ── regex("^groups") selects all groups* names ────────────────────────────────

test_that("regex('^groups') equalizes all groups* boxes", {
  fc  <- make_fc()
  out <- equalizeWidths(fc, subelement = stringr::regex("^groups"))

  # Both groups1 and groups2 boxes should share the same width
  all_paths <- list(c("groups1", 1), c("groups1", 2), c("groups2", 1), c("groups2", 2))
  widths_mm <- vapply(all_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))

  # excl* and other boxes must be untouched (different width)
  excl1_w <- convertWidth(coords(out$excl1[[1]])$width, "mm", valueOnly = TRUE)
  expect_false(isTRUE(all.equal(excl1_w, widths_mm[1], tolerance = 1e-6)))
})

# ── regex("^excl") selects only excl* names ───────────────────────────────────

test_that("regex('^excl') equalizes only excl* boxes", {
  fc  <- make_fc()
  # Pre-widen groups* boxes so they are clearly wider than excl* boxes
  fc  <- equalizeWidths(fc, subelement = stringr::regex("^groups"), width = unit(60, "mm"))

  out <- equalizeWidths(fc, subelement = stringr::regex("^excl"))

  excl_paths <- list(c("excl1", 1), c("excl2", 1))
  widths_mm <- vapply(excl_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))

  # groups* must be untouched (still 60mm)
  g1_w <- convertWidth(coords(out$groups1[[1]])$width, "mm", valueOnly = TRUE)
  expect_equal(g1_w, 60, tolerance = 1e-6)
  # excl boxes are auto-sized and clearly narrower than 60mm
  expect_lt(widths_mm[1], 60 - 1)
})

# ── bare "^groups" remains a literal path, not a regex ───────────────────────

test_that("bare '^groups' string is a literal path and errors when not found", {
  fc <- make_fc()
  # "^groups" is not a name in fc, so it should error
  expect_error(
    equalizeWidths(fc, subelement = "^groups"),
    regexp = "not found"
  )
})

# ── bare "groups1" preserves existing literal behavior ───────────────────────

test_that("bare 'groups1' still selects the groups1 list element", {
  fc  <- make_fc()
  out <- equalizeWidths(fc, subelement = "groups1", width = unit(40, "mm"))

  widths_mm <- vapply(out$groups1, function(b) {
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - 40) < 1e-6))

  # groups2 width must differ
  g2_w <- convertWidth(coords(out$groups2[[1]])$width, "mm", valueOnly = TRUE)
  expect_false(isTRUE(all.equal(g2_w, 40, tolerance = 1e-6)))
})

# ── mixed list: regex + literal ───────────────────────────────────────────────

test_that("mixed list list(regex('^groups'), 'other') selects both", {
  fc  <- make_fc()
  out <- equalizeWidths(fc, subelement = list(stringr::regex("^groups"), "other"))

  all_paths <- list(
    c("groups1", 1), c("groups1", 2),
    c("groups2", 1), c("groups2", 2),
    c("other", 1)
  )
  widths_mm <- vapply(all_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))
})

# ── nested path vectors preserved: c("groups1", 1) stays one path ────────────

test_that("c('groups1', 1) as a single path works unchanged", {
  fc  <- make_fc()
  out <- equalizeWidths(fc, subelement = c("groups1", 1), width = unit(35, "mm"))

  w <- convertWidth(coords(out$groups1[[1]])$width, "mm", valueOnly = TRUE)
  expect_equal(w, 35, tolerance = 1e-6)

  # Second box in groups1 must NOT be changed
  w2 <- convertWidth(coords(out$groups1[[2]])$width, "mm", valueOnly = TRUE)
  expect_false(isTRUE(all.equal(w2, 35, tolerance = 1e-6)))
})

test_that("list(c('groups1', 1), 'other') mixes nested path and literal", {
  fc  <- make_fc()
  out <- equalizeWidths(fc, subelement = list(c("groups1", 1), c("other", 1)))

  w1 <- convertWidth(coords(out$groups1[[1]])$width, "mm", valueOnly = TRUE)
  wo <- convertWidth(coords(out$other[[1]])$width,   "mm", valueOnly = TRUE)
  expect_equal(w1, wo, tolerance = 1e-6)
})

# ── no regex match emits a warning ───────────────────────────────────────────

test_that("regex selector matching no names warns", {
  fc <- make_fc()
  expect_warning(
    equalizeWidths(fc, subelement = stringr::regex("^gruops")),
    regexp = "matched no top-level subelement names"
  )
})

# ── ignore_case option is respected ──────────────────────────────────────────

test_that("regex('^GROUPS', ignore_case = TRUE) matches lowercase names", {
  fc  <- make_fc()
  out <- expect_no_warning(
    equalizeWidths(fc, subelement = stringr::regex("^GROUPS", ignore_case = TRUE))
  )

  all_paths <- list(c("groups1", 1), c("groups1", 2), c("groups2", 1), c("groups2", 2))
  widths_mm <- vapply(all_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))

  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))
})

# ── NA names are skipped, not turned into NA paths ───────────────────────────

test_that("NA element names are ignored and do not produce NA paths", {
  fc <- make_fc()

  # Inject an NA name into the list — the regex should skip it (str_detect returns
  # NA for NA inputs, which the resolver converts to FALSE), so the remaining
  # valid 'groups*' names are still matched without error or warning.
  names(fc)[length(names(fc))] <- NA_character_

  # Should not crash: the NA name is skipped (str_detect returns NA -> FALSE),
  # and the valid groups* names are still matched and equalized normally.
  out <- expect_no_warning(
    equalizeWidths(fc, subelement = stringr::regex("^groups"))
  )
  expect_true(is.list(out))

  # The groups* boxes should have been equalized (same width)
  all_paths <- list(c("groups1", 1), c("groups1", 2), c("groups2", 1), c("groups2", 2))
  widths_mm <- vapply(all_paths, function(p) {
    b <- Gmisc:::get_list_element_by_path(out, p)
    convertWidth(coords(b)$width, "mm", valueOnly = TRUE)
  }, numeric(1))
  expect_true(all(abs(widths_mm - widths_mm[1]) < 1e-6))
})
