
testthat::test_that(
  "descriptive variability is calculated correctly", {

    example <- data.frame(
      fuel = rep("Petrol", 3),
      water = rep("w", 3),
      concentration_pct = rep(10, 3),
      age_hours = rep(0, 3),
      permittivity = c(2, 4, 6)
    )

    result <- summarise_dielectric_variability(example)

    testthat::expect_equal(result$n_measurements, 3L)
    testthat::expect_equal(result$mean, 4)
    testthat::expect_equal(result$median, 4)
    testthat::expect_equal(result$sd, 2)
    testthat::expect_equal(result$minimum, 2)
    testthat::expect_equal(result$maximum, 6)
  }
)

testthat::test_that(
  "missing values are excluded", {

    example <- data.frame(
      fuel = rep("Diesel", 3),
      water = rep("w", 3),
      concentration_pct = rep(5, 3),
      age_hours = rep(0, 3),
      permittivity = c(2, NA, 6)
    )

    result <- summarise_dielectric_variability(example)

    testthat::expect_equal(result$n_measurements, 2L)
    testthat::expect_equal(result$mean, 4)
  }
)

testthat::test_that(
  "one measurement does not produce an SD", {

    example <- data.frame(
      fuel = "Petrol",
      water = "w",
      concentration_pct = 10,
      age_hours = 0,
      permittivity = 5
    )

    result <- summarise_dielectric_variability(example)

    testthat::expect_equal(result$n_measurements, 1L)
    testthat::expect_true(is.na(result$sd))
  }
)

testthat::test_that(
  "invalid outcome is rejected", {

    example <- data.frame(
      fuel = "Petrol",
      water = "w",
      concentration_pct = 10,
      age_hours = 0,
      permittivity = 5
    )

    testthat::expect_error(
      summarise_dielectric_variability(
        example,
        outcome = "unknown_variable"
      )
    )
  }
)

