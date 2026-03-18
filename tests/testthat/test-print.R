test_that("print.hrest produces output", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_output(print(hr), "besthr")
})

test_that("print.hrest shows control group", {
  d <- make_data()
  hr <- estimate(d, score, group, control = "A", nits = 10)

  expect_output(print(hr), "Control: A")
})

test_that("print.hrest shows confidence intervals", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_output(print(hr), "Confidence Intervals")
})

test_that("print.hrest shows bootstrap count", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 25)

  expect_output(print(hr), "25 bootstrap resamples")
})

test_that("print.hrest returns NULL invisibly", {
  d <- make_data()
  hr <- estimate(d, score, group, nits = 10)

  expect_invisible(print(hr))
})
