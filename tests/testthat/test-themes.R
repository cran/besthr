test_that("besthr_palette returns correct number of colors", {
  expect_length(besthr_palette("default", 3), 3)
  expect_length(besthr_palette("okabe_ito", 5), 5)
  expect_length(besthr_palette("viridis", 4), 4)
})

test_that("besthr_palette returns hex colors", {
  pal <- besthr_palette("default", 3)
  expect_true(all(grepl("^#", pal)))
})

test_that("besthr_palette errors on unknown palette", {
  expect_error(besthr_palette("nonexistent"))
})

test_that("theme_besthr returns a theme object", {
  expect_s3_class(theme_besthr("classic"), "theme")
  expect_s3_class(theme_besthr("modern"), "theme")
})

test_that("theme_besthr errors on unknown style", {
  expect_error(theme_besthr("nonexistent"))
})

test_that("scale_color_besthr returns a scale", {
  s <- scale_color_besthr("okabe_ito")
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_besthr returns a scale", {
  s <- scale_fill_besthr("okabe_ito")
  expect_s3_class(s, "Scale")
})

test_that("ci_fill_colors returns correct number of colors", {
  expect_length(ci_fill_colors("default"), 3)
  expect_length(ci_fill_colors("modern"), 3)
})

test_that("plot.hrest works with theme parameter", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p_classic <- plot(hr, theme = "classic")
  p_modern <- plot(hr, theme = "modern")

  expect_s3_class(p_classic, "ggplot")
  expect_s3_class(p_modern, "ggplot")
})

test_that("plot.hrest works with colors parameter", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p_default <- plot(hr, colors = "default")
  p_okabe <- plot(hr, colors = "okabe_ito")

  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_okabe, "ggplot")
})

test_that("plot.hrest works with both theme and colors", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot(hr, theme = "modern", colors = "okabe_ito")

  expect_s3_class(p, "ggplot")
})
