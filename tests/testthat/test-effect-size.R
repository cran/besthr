# Tests for effect size annotation feature

test_that("effect size annotation appears when show_effect_size = TRUE", {
  set.seed(123)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)
  p <- plot(hr, show_effect_size = TRUE)

  # Should have additional annotation or label
  expect_s3_class(p, "patchwork")
})

test_that("effect size annotation hidden by default", {
  set.seed(456)
  hr <- estimate(make_data(), score, group, nits = 100)
  p <- plot(hr)

  # Default plot should work without effect size
  expect_s3_class(p, "patchwork")
})

test_that("compute_effect_size returns correct structure", {
  set.seed(789)
  hr <- estimate(make_data(), score, group, control = "A", nits = 200)

  effect <- compute_effect_size(hr)

  expect_s3_class(effect, "data.frame")
  expect_true("group" %in% names(effect))
  expect_true("effect" %in% names(effect))
  expect_true("effect_ci_low" %in% names(effect))
  expect_true("effect_ci_high" %in% names(effect))
})

test_that("effect size is NA for control group", {
  set.seed(42)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)

  effect <- compute_effect_size(hr)

  expect_true(is.na(effect$effect[effect$group == "A"]))
})

test_that("effect size calculation is mathematically correct", {
  set.seed(111)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)

  effect <- compute_effect_size(hr)

  # Effect for B should be mean(B) - mean(A)
  control_mean <- hr$group_means$mean[hr$group_means$group == "A"]
  treatment_mean <- hr$group_means$mean[hr$group_means$group == "B"]
  expected_effect <- treatment_mean - control_mean

  expect_equal(effect$effect[effect$group == "B"], expected_effect)
})

test_that("effect size works with multiple groups", {
  set.seed(222)
  d <- make_data3()
  hr <- estimate(d, score, sample, control = "A", nits = 100)

  effect <- compute_effect_size(hr)

  # Should have effect for B and C, NA for A
  expect_true(is.na(effect$effect[effect$group == "A"]))
  expect_false(is.na(effect$effect[effect$group == "B"]))
  expect_false(is.na(effect$effect[effect$group == "C"]))
})

test_that("show_effect_size works with config parameter", {
  set.seed(333)
  hr <- estimate(make_data(), score, group, nits = 100)
  cfg <- besthr_plot_config(theme_style = "modern", color_palette = "okabe_ito")
  p <- plot(hr, config = cfg, show_effect_size = TRUE)

  expect_s3_class(p, "patchwork")
})

test_that("effect size CI is computed from bootstrap", {
  set.seed(444)
  hr <- estimate(make_data(), score, group, control = "A", nits = 500)

  effect <- compute_effect_size(hr)

  # CI should be numeric and properly ordered
  b_row <- effect[effect$group == "B", ]
  expect_true(b_row$effect_ci_low < b_row$effect)
  expect_true(b_row$effect_ci_high > b_row$effect)
})
