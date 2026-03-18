test_that("estimate returns hrest object", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_s3_class(hr, "hrest")
})

test_that("estimate returns expected structure", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_true("control" %in% names(hr))
  expect_true("group_means" %in% names(hr))
  expect_true("ranked_data" %in% names(hr))
  expect_true("original_data" %in% names(hr))
  expect_true("bootstraps" %in% names(hr))
  expect_true("ci" %in% names(hr))
  expect_true("nits" %in% names(hr))
  expect_true("low" %in% names(hr))
  expect_true("high" %in% names(hr))
  expect_true("group_n" %in% names(hr))
  expect_true("column_info" %in% names(hr))
})

test_that("estimate uses correct control group", {
  d <- make_data()
  hr <- estimate(d, score, group, control = "A", nits = 10)

  expect_equal(hr$control, "A")
})

test_that("estimate uses custom control group", {
  d <- make_data()
  hr <- estimate(d, score, group, control = "B", nits = 10)

  expect_equal(hr$control, "B")
})

test_that("estimate stores correct number of iterations", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 50)

  expect_equal(hr$nits, 50)
})

test_that("estimate stores correct confidence interval limits", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10, low = 0.1, high = 0.9)

  expect_equal(hr$low, 0.1)
  expect_equal(hr$high, 0.9)
})

test_that("estimate works with technical replicates (data2)", {
  d <- make_data2()
  hr <- estimate(d, score_column_name, sample_column_name, rep_column_name, nits = 10)

  expect_s3_class(hr, "hrest")
  expect_equal(length(hr$column_info), 3)
})

test_that("estimate works with three groups (data3)", {
  d <- make_data3()
  hr <- estimate(d, score, sample, rep, nits = 10)

  expect_s3_class(hr, "hrest")
  # Should have 2 non-control groups in CI
  expect_equal(nrow(hr$ci), 2)
})

test_that("estimate ranked_data has rank column", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_true("rank" %in% names(hr$ranked_data))
})

test_that("estimate bootstraps has correct columns", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_true("mean" %in% names(hr$bootstraps))
  expect_true("iteration" %in% names(hr$bootstraps))
  expect_true("group" %in% names(hr$bootstraps))
})

test_that("estimate ci has correct columns", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_true("low" %in% names(hr$ci))
  expect_true("high" %in% names(hr$ci))
  expect_true("mean" %in% names(hr$ci))
})
