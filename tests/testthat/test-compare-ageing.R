
testthat::test_that("ageing comparisons match frequencies", {

  example <- data.frame(
    fuel = rep("Petrol", 4),
    water = rep("w", 4),
    concentration_pct = rep(20, 4),
    age_hours = c(0, 0, 96, 96),
    frequency_hz = c(10000, 50000, 10000, 50000),
    permittivity = c(40, 20, 20, 10),
    dissipation_factor = c(2, 1, 1, 0.5),
    impedance = c(100, 200, 200, 400)
  )

  result <- compare_ageing(example, 0, 96)

  testthat::expect_equal(nrow(result), 2L)

  testthat::expect_equal(
    result$permittivity_ratio,
    c(0.5, 0.5)
  )

  testthat::expect_equal(
    result$impedance_ratio,
    c(2, 2)
  )

  testthat::expect_equal(
    result$dissipation_factor_ratio,
    c(0.5, 0.5)
  )
})

testthat::test_that("duplicate measurements are rejected", {

  example <- data.frame(
    fuel = c("Petrol", "Petrol", "Petrol"),
    water = c("w", "w", "w"),
    concentration_pct = c(20, 20, 20),
    age_hours = c(0, 0, 96),
    frequency_hz = c(10000, 10000, 10000),
    permittivity = c(40, 40, 20),
    dissipation_factor = c(2, 2, 1),
    impedance = c(100, 100, 200)
  )

  testthat::expect_error(
    compare_ageing(example, 0, 96),
    "Duplicate"
  )
})

testthat::test_that("zero denominators produce NA ratios", {

  example <- data.frame(
    fuel = c("Petrol", "Petrol"),
    water = c("w", "w"),
    concentration_pct = c(20, 20),
    age_hours = c(0, 96),
    frequency_hz = c(10000, 10000),
    permittivity = c(0, 20),
    dissipation_factor = c(2, 1),
    impedance = c(100, 200)
  )

  result <- compare_ageing(example, 0, 96)

  testthat::expect_true(
    is.na(result$permittivity_ratio)
  )

  testthat::expect_equal(
    result$impedance_ratio,
    2
  )
})

testthat::test_that("invalid ageing times are rejected", {

  example <- data.frame(
    fuel = c("Petrol", "Petrol"),
    water = c("w", "w"),
    concentration_pct = c(20, 20),
    age_hours = c(0, 96),
    frequency_hz = c(10000, 10000),
    permittivity = c(40, 20),
    dissipation_factor = c(2, 1),
    impedance = c(100, 200)
  )

  testthat::expect_error(
    compare_ageing(example, 0, 0)
  )

  testthat::expect_error(
    compare_ageing(example, NA_real_, 96)
  )
})

