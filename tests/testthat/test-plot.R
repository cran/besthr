test_that("plot.hrest returns a ggplot object", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot(hr)

  expect_s3_class(p, "ggplot")
})

test_that("plot.hrest works with rank_simulation option", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot(hr, which = "rank_simulation")

  expect_s3_class(p, "ggplot")
})

test_that("plot.hrest works with just_data option and tech reps", {
  d <- make_data2()
  hr <- estimate(d, score_column_name, sample_column_name, rep_column_name, nits = 10)
  p <- plot(hr, which = "just_data")

  expect_s3_class(p, "ggplot")
})

test_that("plot.hrest works with three groups", {
  d <- make_data3()
  hr <- estimate(d, score, sample, rep, nits = 10)
  p <- plot(hr)

  expect_s3_class(p, "ggplot")
})

test_that("plot.hrest uses patchwork for layout", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  p <- plot(hr)

  # patchwork objects are both patchwork and ggplot
  expect_s3_class(p, "patchwork")
})

test_that("plot.hrest works with config parameter", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  cfg <- besthr_plot_config(
    panel_widths = c(2, 1),
    theme_style = "modern",
    color_palette = "okabe_ito"
  )
  p <- plot(hr, config = cfg)

  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "patchwork")
})

test_that("plot.hrest config overrides theme and colors params", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  cfg <- besthr_plot_config(theme_style = "modern", color_palette = "okabe_ito")

  # When config is provided, theme and colors params are ignored
  p <- plot(hr, theme = "classic", colors = "default", config = cfg)
  expect_s3_class(p, "ggplot")
})
