
testthat::test_that("prediction metrics are correct", {

  result <- evaluate_external_predictions(
    observed = c(10, 20, 30),
    predicted = c(12, 18, 33)
  )

  testthat::expect_equal(result$n, 3)
  testthat::expect_equal(result$mae, 7 / 3)
  testthat::expect_equal(result$rmse, sqrt(17 / 3))
  testthat::expect_equal(result$bias, 1)
})

testthat::test_that("perfect predictions have zero error", {

  result <- evaluate_external_predictions(
    c(1, 2, 3),
    c(1, 2, 3)
  )

  testthat::expect_equal(result$mae, 0)
  testthat::expect_equal(result$rmse, 0)
  testthat::expect_equal(result$bias, 0)
})

testthat::test_that("invalid inputs are rejected", {

  testthat::expect_error(
    evaluate_external_predictions(c(1, 2), 1),
    "lengths must match"
  )

  testthat::expect_error(
    evaluate_external_predictions(numeric(0), numeric(0)),
    "At least one prediction"
  )

  testthat::expect_error(
    evaluate_external_predictions(c(1, NA), c(1, 2)),
    "must be finite"
  )

  testthat::expect_error(
    evaluate_external_predictions("a", 1),
    "must be numeric"
  )
})

