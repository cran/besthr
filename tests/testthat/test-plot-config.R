test_that("besthr_plot_config creates valid config object", {
  cfg <- besthr_plot_config()

  expect_s3_class(cfg, "besthr_plot_config")
  expect_equal(cfg$panel_widths, c(1, 1))
  expect_equal(cfg$theme_style, "modern")
  expect_equal(cfg$color_palette, "okabe_ito")
})

test_that("besthr_plot_config accepts custom parameters", {
  cfg <- besthr_plot_config(
    panel_widths = c(2, 1),
    point_size_range = c(3, 10),
    theme_style = "modern",
    color_palette = "okabe_ito"
  )

  expect_equal(cfg$panel_widths, c(2, 1))
  expect_equal(cfg$point_size_range, c(3, 10))
  expect_equal(cfg$theme_style, "modern")
  expect_equal(cfg$color_palette, "okabe_ito")
})

test_that("besthr_plot_config validates inputs", {
  expect_error(besthr_plot_config(panel_widths = "invalid"))
  expect_error(besthr_plot_config(y_limits = c(1, 2, 3)))
  expect_error(besthr_plot_config(y_expand = -1))
  expect_error(besthr_plot_config(point_alpha = 2))
  expect_error(besthr_plot_config(theme_style = "nonexistent"))
  expect_error(besthr_plot_config(color_palette = "nonexistent"))
})

test_that("update_config modifies config correctly", {
  cfg <- besthr_plot_config()
  cfg2 <- update_config(cfg, theme_style = "classic", panel_widths = c(2, 1))

  expect_equal(cfg2$theme_style, "classic")
  expect_equal(cfg2$panel_widths, c(2, 1))
  # Original should be unchanged
  expect_equal(cfg$theme_style, "modern")
})

test_that("update_config validates input", {
  cfg <- besthr_plot_config()
  expect_error(update_config(list(), theme_style = "modern"))
  expect_error(update_config(cfg, nonexistent_param = "value"))
})

test_that("print.besthr_plot_config works", {
  cfg <- besthr_plot_config()
  expect_output(print(cfg), "besthr plot configuration")
  expect_output(print(cfg), "Theme: modern")
})

test_that("besthr_data_view creates valid view", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)

  expect_s3_class(dv, "besthr_data_view")
  expect_true("ranked" %in% names(dv))
  expect_true("bootstrap" %in% names(dv))
  expect_true("rank_limits" %in% names(dv))
  expect_length(dv$rank_limits, 2)
})

test_that("besthr_data_view computes unified limits", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)

  # Limits should encompass all data
  expect_true(dv$rank_limits[1] <= min(dv$ranked$rank))
  expect_true(dv$rank_limits[2] >= max(dv$ranked$rank))
  expect_true(dv$rank_limits[1] <= min(dv$bootstrap$mean))
  expect_true(dv$rank_limits[2] >= max(dv$bootstrap$mean))
})

test_that("besthr_data_view respects custom y_limits", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  cfg <- besthr_plot_config(y_limits = c(0, 100))
  dv <- besthr_data_view(hr, cfg)

  expect_equal(dv$rank_limits, c(0, 100))
})

test_that("print.besthr_data_view works", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)

  expect_output(print(dv), "besthr data view")
  expect_output(print(dv), "Groups:")
})

test_that("build_observation_panel returns ggplot", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)
  cfg <- besthr_plot_config()

  p <- build_observation_panel(dv, cfg, "rank_simulation")
  expect_s3_class(p, "ggplot")
})

test_that("build_bootstrap_panel returns ggplot", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)
  cfg <- besthr_plot_config()

  p <- build_bootstrap_panel(dv, cfg)
  expect_s3_class(p, "ggplot")
})

test_that("compose_besthr_panels creates patchwork", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)
  dv <- besthr_data_view(hr)
  cfg <- besthr_plot_config()

  p1 <- build_observation_panel(dv, cfg)
  p2 <- build_bootstrap_panel(dv, cfg)
  p <- compose_besthr_panels(list(p1, p2), cfg)

  expect_s3_class(p, "patchwork")
})

test_that("derive_ci_colors returns three colors", {
  expect_length(derive_ci_colors("default", "classic"), 3)
  expect_length(derive_ci_colors("okabe_ito", "classic"), 3)
  expect_length(derive_ci_colors("viridis", "modern"), 3)
})

test_that("derive_ci_colors returns hex colors", {
  colors <- derive_ci_colors("okabe_ito", "classic")
  expect_true(all(grepl("^#", colors)))
})

test_that("besthr_plot_config supports density_style option", {
  cfg <- besthr_plot_config(density_style = "points")
  expect_equal(cfg$density_style, "points")

  cfg2 <- besthr_plot_config(density_style = "solid")
  expect_equal(cfg2$density_style, "solid")

  cfg3 <- besthr_plot_config(density_style = "gradient")
  expect_equal(cfg3$density_style, "gradient")

  expect_error(besthr_plot_config(density_style = "invalid"))
})

test_that("besthr_style returns valid configs", {
  expect_s3_class(besthr_style("default"), "besthr_plot_config")
  expect_s3_class(besthr_style("classic"), "besthr_plot_config")
  expect_s3_class(besthr_style("publication"), "besthr_plot_config")
  expect_s3_class(besthr_style("presentation"), "besthr_plot_config")
  expect_s3_class(besthr_style("density"), "besthr_plot_config")
})

test_that("besthr_style presets have expected settings", {
  default <- besthr_style("default")
  expect_equal(default$theme_style, "modern")
  expect_equal(default$color_palette, "okabe_ito")

  classic <- besthr_style("classic")
  expect_equal(classic$theme_style, "classic")
  expect_equal(classic$color_palette, "default")

  density <- besthr_style("density")
  expect_equal(density$density_style, "gradient")
})

test_that("besthr_style validates input", {
  expect_error(besthr_style("nonexistent"))
})

test_that("list_besthr_styles outputs style list", {
  expect_output(list_besthr_styles(), "default")
  expect_output(list_besthr_styles(), "classic")
  expect_output(list_besthr_styles(), "publication")
})
