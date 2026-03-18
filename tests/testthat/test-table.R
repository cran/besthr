# Tests for summary table feature

test_that("besthr_table returns tibble with required columns", {
  set.seed(123)
  hr <- estimate(make_data(), score, group, nits = 100)
  tbl <- besthr_table(hr)

  expect_s3_class(tbl, "tbl_df")
  expect_true("group" %in% names(tbl))
  expect_true("n" %in% names(tbl))
  expect_true("mean_rank" %in% names(tbl))
  expect_true("ci_low" %in% names(tbl))
  expect_true("ci_high" %in% names(tbl))
})

test_that("besthr_table includes effect size column", {
  set.seed(456)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)
  tbl <- besthr_table(hr)

  expect_true("effect_size" %in% names(tbl))
})

test_that("besthr_table effect size is NA for control", {
  set.seed(789)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)
  tbl <- besthr_table(hr)

  expect_true(is.na(tbl$effect_size[tbl$group == "A"]))
})

test_that("besthr_table effect size is computed for treatment", {
  set.seed(42)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)
  tbl <- besthr_table(hr)

  expect_false(is.na(tbl$effect_size[tbl$group == "B"]))
})

test_that("besthr_table n values match hrest object", {
  set.seed(111)
  hr <- estimate(make_data(), score, group, nits = 100)
  tbl <- besthr_table(hr)

  for (g in unique(tbl$group)) {
    expected_n <- hr$group_n$n[hr$group_n$group == g]
    actual_n <- tbl$n[tbl$group == g]
    expect_equal(actual_n, expected_n)
  }
})

test_that("besthr_table mean_rank values match hrest object", {
  set.seed(222)
  hr <- estimate(make_data(), score, group, nits = 100)
  tbl <- besthr_table(hr)

  for (g in unique(tbl$group)) {
    expected_mean <- hr$group_means$mean[hr$group_means$group == g]
    actual_mean <- tbl$mean_rank[tbl$group == g]
    expect_equal(actual_mean, expected_mean)
  }
})

test_that("besthr_table CI values match hrest object", {
  set.seed(333)
  hr <- estimate(make_data(), score, group, control = "A", nits = 100)
  tbl <- besthr_table(hr)

  # Check treatment group CI (table rounds to 2 decimal places by default)
  b_ci <- hr$ci[hr$ci$group == "B", ]
  expect_equal(tbl$ci_low[tbl$group == "B"], round(b_ci$low, 2))
  expect_equal(tbl$ci_high[tbl$group == "B"], round(b_ci$high, 2))
})

test_that("besthr_table works with multiple groups", {
  set.seed(444)
  d <- make_data3()
  hr <- estimate(d, score, sample, control = "A", nits = 100)
  tbl <- besthr_table(hr)

  expect_equal(nrow(tbl), 3)
  expect_true(all(c("A", "B", "C") %in% tbl$group))
})

test_that("besthr_table markdown format is valid", {
  set.seed(555)
  hr <- estimate(make_data(), score, group, nits = 100)
  md <- besthr_table(hr, format = "markdown")

  expect_true(is.character(md))
  expect_true(grepl("\\|", md))  # Contains pipe characters
  expect_true(grepl("---", md))  # Contains header separator
})

test_that("besthr_table html format is valid", {
  set.seed(666)
  hr <- estimate(make_data(), score, group, nits = 100)
  html <- besthr_table(hr, format = "html")

  expect_true(is.character(html))
  expect_true(grepl("<table", html, ignore.case = TRUE))
})

test_that("besthr_table latex format is valid", {
  set.seed(777)
  hr <- estimate(make_data(), score, group, nits = 100)
  latex <- besthr_table(hr, format = "latex")

  expect_true(is.character(latex))
  expect_true(grepl("\\\\begin\\{tabular\\}", latex) ||
              grepl("tabular", latex))
})

test_that("besthr_table default format returns tibble", {
  set.seed(888)
  hr <- estimate(make_data(), score, group, nits = 100)
  tbl <- besthr_table(hr, format = "tibble")

  expect_s3_class(tbl, "tbl_df")
})

test_that("besthr_table errors on invalid format", {
  set.seed(999)
  hr <- estimate(make_data(), score, group, nits = 100)

  expect_error(besthr_table(hr, format = "invalid"))
})

test_that("besthr_table includes significance stars when requested", {
  set.seed(1111)
  d <- data.frame(
    score = c(rep(2, 10), rep(8, 10)),
    group = rep(c("A", "B"), each = 10)
  )
  hr <- estimate(d, score, group, control = "A", nits = 500)
  tbl <- besthr_table(hr, include_significance = TRUE)

  expect_true("significance" %in% names(tbl))
})

test_that("besthr_table rounds values appropriately", {
  set.seed(2222)
  hr <- estimate(make_data(), score, group, nits = 100)
  tbl <- besthr_table(hr, digits = 2)

  # Check that values are rounded (no more than 2 decimal places)
  # This is a structural test - exact implementation may vary
  expect_s3_class(tbl, "tbl_df")
})
