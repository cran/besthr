# Tests for publication export feature

test_that("save_besthr creates PNG file", {
  set.seed(123)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  save_besthr(hr, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})

test_that("save_besthr creates PDF file", {
  set.seed(456)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".pdf")

  save_besthr(hr, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})

test_that("save_besthr creates SVG file", {
  skip_if_not_installed("svglite")

  set.seed(789)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".svg")

  save_besthr(hr, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})

test_that("save_besthr creates TIFF file", {
  set.seed(42)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".tiff")

  save_besthr(hr, tmp)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})

test_that("save_besthr respects width and height", {
  set.seed(111)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  save_besthr(hr, tmp, width = 10, height = 8)

  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("save_besthr respects dpi parameter", {
  set.seed(222)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp_low <- tempfile(fileext = ".png")
  tmp_high <- tempfile(fileext = ".png")

  save_besthr(hr, tmp_low, dpi = 72)
  save_besthr(hr, tmp_high, dpi = 300)

  # Higher DPI should result in larger file
  expect_gt(file.size(tmp_high), file.size(tmp_low))

  unlink(tmp_low)
  unlink(tmp_high)
})

test_that("save_besthr uses sensible defaults", {
  set.seed(333)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  # Should work with just filename
  save_besthr(hr, tmp)

  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("save_besthr can save raincloud plot", {
  set.seed(555)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  save_besthr(hr, tmp, type = "raincloud")

  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("save_besthr passes plot options", {
  set.seed(666)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  save_besthr(hr, tmp, theme = "modern", colors = "okabe_ito")

  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("save_besthr errors on invalid format", {
  set.seed(777)
  hr <- estimate(make_data(), score, group, nits = 50)

  expect_error(save_besthr(hr, "test.xyz"))
})

test_that("save_besthr returns filename invisibly", {
  set.seed(888)
  hr <- estimate(make_data(), score, group, nits = 50)
  tmp <- tempfile(fileext = ".png")

  result <- save_besthr(hr, tmp)

  expect_equal(result, tmp)
  unlink(tmp)
})
