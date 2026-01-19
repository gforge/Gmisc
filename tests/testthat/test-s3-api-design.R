library(testthat)

test_that("Flowchart S3 API structure", {
  # This test defines the expected behavior for the specific S3 API
  # requested: list() |> align() |> connect() |> print()

  # Mock components
  b1 <- boxGrob("Start")
  b2 <- boxGrob("End")

  # 1. Base list
  l <- list(start = b1, end = b2)
  expect_type(l, "list")

  # 2. align() generic
  # Should upgrade to Gmisc_list_of_boxes and set alignment
  l_aligned <- align(l, axis = "y")
  expect_s3_class(l_aligned, "Gmisc_list_of_boxes")

  # 3. connect() generic
  # Should NOT return a grob, but the list with connection metadata attached
  # The connectGrob uses coords which align sets.
  # So we connect AFTER alignment.
  l_conn <- connect(l_aligned, from = "start", to = "end", type = "vertical")
  expect_s3_class(l_conn, "Gmisc_list_of_boxes")
  conn_attr <- attr(l_conn, "connections")
  expect_type(conn_attr, "list")
  expect_equal(length(conn_attr), 1)

  # Check that the connection object (grob) is stored
  expect_s3_class(conn_attr[[1]], "connect_boxes")

  # 4. spread() generic
  # Should also accept a list and return Gmisc_list_of_boxes
  l_spread <- spread(l, axis = "x")
  expect_s3_class(l_spread, "Gmisc_list_of_boxes")
})

test_that("Class duplication check (user concern)", {
  # Mimicking the behavior of potential class addition
  obj <- list(a = 1)
  class(obj) <- c("Gmisc_list_of_boxes", "list")

  # If we extend it again
  obj2 <- prExtendClass(obj, "Gmisc_list_of_boxes")
  # Should be unique
  expect_equal(class(obj2), c("Gmisc_list_of_boxes", "list"))

  # If we use structure() blindly (which some code might do)
  obj3 <- structure(obj, class = c("Gmisc_list_of_boxes", class(obj)))
  # This WOULD duplicate
  expect_equal(class(obj3), c("Gmisc_list_of_boxes", "Gmisc_list_of_boxes", "list"))

  # The fix is to ensure all internal helpers use prExtendClass or unique()
})

test_that("Invalid input validation", {
  b1 <- boxGrob("Start")

  # Invalid list content
  bad_list <- list(start = b1, bad = "not a box")

  expect_error(align(bad_list, axis = "y"), "not valid boxes")
  expect_error(spread(bad_list, axis = "y"), "not valid boxes")
  expect_error(connect(bad_list, from = "start", to = "bad"), "not valid boxes")

  # Recursive invalid content
  recursive_bad <- list(start = b1, group = list(ok = b1, bad = 12345))
  expect_error(align(recursive_bad, axis = "y"), "group\\$bad")

  # Valid list content
  good_list <- list(start = b1, valid = list(b1)) # Nested list allowed handled by prConvert
  # Note: prConvertListToBoxList allows lists as elements.

  # Should not error
  expect_error(align(good_list, axis = "y"), NA)
})

test_that("S3 Mutations: move, append", {
  b1 <- boxGrob("Start")
  b2 <- boxGrob("End")

  l <- list(start = b1) |> align()
  expect_true(is.list(l))
  expect_true(inherits(l, "Gmisc_list_of_boxes"))

  message("L is list: ", is.list(l))
  message("L class: ", paste(class(l), collapse = ", "))

  # Append
  l_appended <- append(l, list(end = b2))
  expect_s3_class(l_appended, "Gmisc_list_of_boxes")
  expect_equal(length(l_appended), 2)
  expect_named(l_appended, c("start", "end"))

  # Ensure attributes preserved (align sets align_list attr? no, just coords in boxes)
  # But connections should be preserved
  attr(l, "connections") <- list("fake_conn")

  l_appended_conn <- append(l, list(end = b2))
  expect_false(is.null(attr(l_appended_conn, "connections")))

  # Move
  # Testing the move generic wrapper around moveBox
  l_moved <- move(l, x = .5, y = .5, subelement = "start")
  expect_s3_class(l_moved, "Gmisc_list_of_boxes")
  expect_false(is.null(attr(l_moved, "connections")))

  # Verify coords changed (moveBox works)
  # We can't easily check internal coords without checking the grob, but we trust moveBox.
  # Just checking structure here.
})

test_that("S3 Mutations: insert", {
  b1 <- boxGrob("Start", x = .1, y = .5)
  b2 <- boxGrob("End", x = .9, y = .5)

  l <- list(start = b1, end = b2) |> align()

  # Valid insertion between
  b_mid <- boxGrob("Middle")
  l_ins <- insert(l, b_mid, after = "start")

  # Check list structure
  expect_equal(length(l_ins), 3)
  expect_equal(names(l_ins), c("start", "", "end"))

  # Check midpoint calculation
  # b1 x=.1, b2 x=.9 -> mid should be .5
  # Note: we need to allow for some floating point tolerance and unit conversion
  mid_box <- l_ins[[2]]
  mid_coords <- coords(mid_box)

  # In test environment, grid units might need simple conversion verification
  # Since moveBox sets absolute viewport position.
  # We assume prConvertAllToNpc or similar helps, but moveBox sets "npc" units usually?
  # Actually moveBox(space="absolute") uses units provided.

  # Just verify x is approx 0.5
  x_val <- convertX(mid_coords$x, "npc", valueOnly = TRUE)
  expect_equal(x_val, 0.5, tolerance = 0.01)

  # Test insertion before
  l_ins2 <- insert(l, b_mid, before = "end")
  expect_equal(length(l_ins2), 3)
  expect_equal(convertX(coords(l_ins2[[2]])$x, "npc", valueOnly = TRUE), 0.5, tolerance = 0.01)

  # Test appending using insert (fail to calc midpoint, but inserts)
  # If we insert after end, prev=b2, next=NULL.
  # Code says: do nothing to coords.
  l_ins3 <- insert(l, b_mid, after = "end")
  expect_equal(length(l_ins3), 3)
})

test_that("Complex chaining example", {
  # Mock components
  org_cohort <- boxGrob("Stockholm", x = .5, y = .9)
  eligible <- boxGrob("Eligible", x = .5, y = .8)
  included <- boxGrob("Included", x = .5, y = .7)
  grp_a <- boxGrob("A", x = .4, y = .5)
  grp_b <- boxGrob("B", x = .6, y = .5)
  excluded <- boxGrob("Excluded", x = .8, y = .8) # to move

  # We use the pipeline exactly as user described (mocking spread functions slightly if needed, or using real ones)
  # Requires namespace access to internal S3 methods if not loaded, but test_file loads pkg

  res <- list(
    org_cohort = org_cohort,
    eligible = eligible,
    included = included,
    grps = list(grp_a, grp_b)
  ) |>
    spread(axis = "y") |>
    spread(subelement = "grps", axis = "x") |>
    insert(list(excluded = excluded), after = "eligible", name = "excluded") |>
    move(name = "excluded", x = .8) |>
    connect("org_cohort", "eligible", type = "vert") |>
    connect("eligible", "included", type = "vert") |>
    connect("included", "grps", type = "N") |>
    connect("eligible", "excluded", type = "L", label = "Excluded")

  expect_s3_class(res, "Gmisc_list_of_boxes")
  expect_true("excluded" %in% names(res))
  expect_equal(length(attr(res, "connections")), 4)

  # Verify move
  # moveBox logic with absolute space typically sets x to the value provided.
  ex_box <- res$excluded
  ex_coords <- coords(ex_box)
  expect_equal(convertX(ex_coords$x, "npc", valueOnly = TRUE), 0.8)

  # Debug connections count
  # We expect at least 4 connections (1 for each connect call)
  # Sometimes connect calls might produce multiple grobs depending on implementation details of 1-to-many
  conns <- attr(res, "connections")
  expect_gte(length(conns), 4)
})
