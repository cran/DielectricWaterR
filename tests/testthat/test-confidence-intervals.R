
testthat::test_that(
  "CI uses independent units rather than measurement rows", {

    example <- data.frame(
      fuel = rep("Petrol", 6),
      water = rep("w", 6),
      concentration_pct = rep(10, 6),
      age_hours = rep(0, 6),
      frequency_hz = rep(10000, 6),
      physical_id = c("S1", "S1", "S2", "S2", "S3", "S3"),
      permittivity = c(2, 4, 4, 6, 6, 8)
    )

    result <- estimate_dielectric_ci(
      example,
      sample_id = "physical_id"
    )

    # Unit means are 3, 5 and 7
    testthat::expect_equal(result$independent_units, 3L)
    testthat::expect_equal(result$mean, 5)
    testthat::expect_equal(
      result$standard_error,
      2 / sqrt(3)
    )

    testthat::expect_lt(result$lower_ci, 5)
    testthat::expect_gt(result$upper_ci, 5)
  }
)

testthat::test_that(
  "one physical sample cannot produce a CI", {

    example <- data.frame(
      age_hours = c(0, 0, 0),
      frequency_hz = c(10000, 10000, 10000),
      physical_id = c("S1", "S1", "S1"),
      permittivity = c(2, 4, 6)
    )

    testthat::expect_error(
      estimate_dielectric_ci(
        example,
        sample_id = "physical_id"
      ),
      "At least two independent physical units"
    )
  }
)

testthat::test_that(
  "different frequencies cannot be pooled", {

    example <- data.frame(
      age_hours = c(0, 0),
      frequency_hz = c(10000, 50000),
      physical_id = c("S1", "S2"),
      permittivity = c(2, 4)
    )

    testthat::expect_error(
      estimate_dielectric_ci(
        example,
        sample_id = "physical_id"
      ),
      "one condition and one frequency"
    )
  }
)

testthat::test_that(
  "missing sample identifiers are rejected", {

    example <- data.frame(
      age_hours = c(0, 0),
      frequency_hz = c(10000, 10000),
      physical_id = c("S1", NA_character_),
      permittivity = c(2, 4)
    )

    testthat::expect_error(
      estimate_dielectric_ci(
        example,
        sample_id = "physical_id"
      ),
      "identifiers must be complete"
    )
  }
)

testthat::test_that(
  "missing sample ID argument is rejected", {

    example <- data.frame(
      age_hours = c(0, 0),
      frequency_hz = c(10000, 10000),
      permittivity = c(2, 4)
    )

    testthat::expect_error(
      estimate_dielectric_ci(example),
      "physical sample ID column is required"
    )
  }
)

testthat::test_that(
  "invalid confidence levels are rejected", {

    example <- data.frame(
      age_hours = c(0, 0),
      frequency_hz = c(10000, 10000),
      physical_id = c("S1", "S2"),
      permittivity = c(2, 4)
    )

    testthat::expect_error(
      estimate_dielectric_ci(
        example,
        sample_id = "physical_id",
        conf_level = 1.5
      ),
      "conf_level must be between zero and one"
    )
  }
)

