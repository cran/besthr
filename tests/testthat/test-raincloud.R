test_that("plot_raincloud returns ggplot", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot_raincloud(hr)

  expect_s3_class(p, "ggplot")
})

test_that("plot_raincloud works with different themes", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  p_classic <- plot_raincloud(hr, theme = "classic")
  p_modern <- plot_raincloud(hr, theme = "modern")

  expect_s3_class(p_classic, "ggplot")
  expect_s3_class(p_modern, "ggplot")
})

test_that("plot_raincloud works with different color palettes", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  p_default <- plot_raincloud(hr, colors = "default")
  p_okabe <- plot_raincloud(hr, colors = "okabe_ito")
  p_viridis <- plot_raincloud(hr, colors = "viridis")

  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_okabe, "ggplot")
  expect_s3_class(p_viridis, "ggplot")
})

test_that("plot_raincloud accepts config object", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  cfg <- besthr_plot_config(theme_style = "modern", color_palette = "okabe_ito")

  p <- plot_raincloud(hr, config = cfg)
  expect_s3_class(p, "ggplot")
})

test_that("plot_raincloud works without bootstrap", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot_raincloud(hr, show_bootstrap = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("plot_raincloud works with three groups", {
  d <- make_data3()
  hr <- estimate(d, score, sample, rep, nits = 10)
  p <- plot_raincloud(hr)

  expect_s3_class(p, "ggplot")
})

test_that("plot_bootstrap_raincloud returns ggplot", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot_bootstrap_raincloud(hr)

  expect_s3_class(p, "ggplot")
})

test_that("plot_bootstrap_raincloud works with different themes", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  p_classic <- plot_bootstrap_raincloud(hr, theme = "classic")
  p_modern <- plot_bootstrap_raincloud(hr, theme = "modern")

  expect_s3_class(p_classic, "ggplot")
  expect_s3_class(p_modern, "ggplot")
})

test_that("plot_raincloud errors on non-hrest input", {
  expect_error(plot_raincloud(list()), "hrest must be an hrest object")
})

test_that("plot_bootstrap_raincloud errors on non-hrest input", {
  expect_error(plot_bootstrap_raincloud(list()), "hrest must be an hrest object")
})
