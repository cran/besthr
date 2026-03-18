test_that("make_data returns correct structure", {
  d <- make_data()

  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 20)
  expect_equal(ncol(d), 2)
  expect_named(d, c("score", "group"))
})

test_that("make_data returns correct groups", {
  d <- make_data()

  expect_equal(unique(d$group), c("A", "B"))
  expect_equal(sum(d$group == "A"), 10)
  expect_equal(sum(d$group == "B"), 10)
})

test_that("make_data score values are in expected range", {
  d <- make_data()

  expect_true(all(d$score >= 1))
  expect_true(all(d$score <= 10))
})

test_that("make_data2 returns correct structure with tech reps", {
  d <- make_data2()

  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 24)
  expect_equal(ncol(d), 3)
  expect_named(d, c("score_column_name", "sample_column_name", "rep_column_name"))
})

test_that("make_data2 returns correct groups and reps", {
  d <- make_data2()

  expect_equal(unique(d$sample_column_name), c("A", "B"))
  expect_equal(unique(d$rep_column_name), c(1, 2, 3))
})

test_that("make_data3 returns correct structure with three groups", {
  d <- make_data3()

  expect_s3_class(d, "tbl_df")
  expect_equal(nrow(d), 36)
  expect_equal(ncol(d), 3)
  expect_named(d, c("score", "sample", "rep"))
})

test_that("make_data3 returns three groups", {
  d <- make_data3()

  expect_equal(unique(d$sample), c("A", "B", "C"))
  expect_equal(sum(d$sample == "A"), 12)
  expect_equal(sum(d$sample == "B"), 12)
  expect_equal(sum(d$sample == "C"), 12)
})
