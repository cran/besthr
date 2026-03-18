# Tests for significance annotations feature

test_that("significance annotations appear when show_significance = TRUE", {
  set.seed(123)
  d <- make_data()
  hr <- estimate(d, score, group, nits = 200)
  p <- plot(hr, show_significance = TRUE)


  # Check that text/annotation layer exists in observation panel
  obs_panel <- p[[1]]
  has_text <- any(sapply(obs_panel$layers, function(l) {
    inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel")
  }))
  expect_true(has_text)
})

test_that("significance annotations hidden by default", {
  set.seed(456)
  hr <- estimate(make_data(), score, group, nits = 100)
  p <- plot(hr)

  # No text annotations by default in observation panel
  obs_panel <- p[[1]]
  has_text <- any(sapply(obs_panel$layers, function(l) {
    inherits(l$geom, "GeomText") || inherits(l$geom, "GeomLabel")
  }))
  expect_false(has_text)
})

test_that("compute_significance returns correct structure", {
  set.seed(789)
  hr <- estimate(make_data(), score, group, control = "A", nits = 200)

  sig <- compute_significance(hr)

  expect_s3_class(sig, "data.frame")
  expect_true("group" %in% names(sig))
  expect_true("significant" %in% names(sig))
  expect_true("stars" %in% names(sig))
})

test_that("significance correctly identifies non-overlapping CI", {
  # Create data with clear separation
  set.seed(42)
  d <- data.frame(
    score = c(rep(2, 10), rep(8, 10)),
    group = rep(c("A", "B"), each = 10)
  )
  hr <- estimate(d, score, group, control = "A", nits = 500)

  sig <- compute_significance(hr)

  # B should be significantly different from A
  expect_true(sig$significant[sig$group == "B"])
})

test_that("control group is not marked as significant", {
  set.seed(111)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)

  sig <- compute_significance(hr)

  # Control group should have NA or FALSE for significance
  expect_true(is.na(sig$significant[sig$group == "A"]) ||
              !sig$significant[sig$group == "A"])
})

test_that("significance stars reflect p-value thresholds", {
  # * for p < 0.05, ** for p < 0.01, *** for p < 0.001
  set.seed(222)
  d <- data.frame(
    score = c(rep(1, 10), rep(10, 10)),
    group = rep(c("A", "B"), each = 10)
  )
  hr <- estimate(d, score, group, control = "A", nits = 1000)

  sig <- compute_significance(hr)

  # With clear separation, should have at least one star
  expect_true(nchar(sig$stars[sig$group == "B"]) >= 1)
})

test_that("show_significance works with multiple groups", {
  set.seed(333)
  d <- make_data3()
  hr <- estimate(d, score, sample, control = "A", nits = 100)
  p <- plot(hr, show_significance = TRUE)

  expect_s3_class(p, "patchwork")
})

test_that("show_significance works with config parameter", {
  set.seed(444)
  hr <- estimate(make_data(), score, group, nits = 100)
  cfg <- besthr_plot_config(theme_style = "modern")
  p <- plot(hr, config = cfg, show_significance = TRUE)

  expect_s3_class(p, "patchwork")
})
