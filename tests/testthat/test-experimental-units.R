
testthat::test_that(
  "material labels are not assumed to be sample IDs", {

    example <- data.frame(
      material = c("Material A", "Material A", "Material A"),
      age_hours = c(0, 48, 96),
      frequency_hz = c(10000, 10000, 10000)
    )

    result <- audit_experimental_units(example)

    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_equal(result$observations, 3L)
    testthat::expect_equal(result$ageing_times, 3L)
    testthat::expect_true(is.na(result$sample_ids))

    testthat::expect_equal(
      result$identity_status,
      "Unknown physical sample identity"
    )
  }
)

testthat::test_that(
  "supplied sample IDs are counted", {

    example <- data.frame(
      material = c("Material A", "Material A", "Material A"),
      age_hours = c(0, 48, 96),
      frequency_hz = c(10000, 10000, 10000),
      physical_id = c("S1", "S1", "S2")
    )

    result <- audit_experimental_units(
      example,
      sample_id = "physical_id"
    )

    testthat::expect_equal(result$sample_ids, 2L)

    testthat::expect_equal(
      result$identity_status,
      "Sample identifiers supplied"
    )
  }
)

testthat::test_that(
  "missing sample IDs are flagged", {

    example <- data.frame(
      material = c("Material A", "Material A"),
      age_hours = c(0, 48),
      frequency_hz = c(10000, 10000),
      physical_id = c("S1", NA_character_)
    )

    result <- audit_experimental_units(
      example,
      sample_id = "physical_id"
    )

    testthat::expect_equal(
      result$identity_status,
      "Incomplete physical sample identifiers"
    )
  }
)

testthat::test_that(
  "invalid inputs are rejected", {

    testthat::expect_error(
      audit_experimental_units(data.frame(x = 1))
    )

    example <- data.frame(
      material = "Material A",
      age_hours = 0,
      frequency_hz = 10000
    )

    testthat::expect_error(
      audit_experimental_units(
        example,
        sample_id = "nonexistent_column"
      )
    )
  }
)

