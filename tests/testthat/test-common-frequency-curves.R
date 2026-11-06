
testthat::test_that(
  "one shared frequency does not permit AUC", {

    example <- data.frame(
      material = rep("A", 4),
      age_hours = c(0, 0, 48, 48),
      frequency_hz = c(10000, 20000, 10000, 30000),
      permittivity = c(2, 4, 6, 8)
    )

    result <- compare_common_frequency_curves(example)

    testthat::expect_equal(
      result$shared_frequencies, 10000
    )

    testthat::expect_null(result$curve_summaries)

    testthat::expect_equal(
      result$status,
      "INSUFFICIENT_SHARED_FREQUENCIES_FOR_AUC"
    )
  }
)

testthat::test_that(
  "multiple shared frequencies permit AUC", {

    example <- data.frame(
      material = rep("A", 4),
      age_hours = c(0, 0, 48, 48),
      frequency_hz = c(10000, 100000, 10000, 100000),
      permittivity = c(2, 4, 6, 8)
    )

    result <- compare_common_frequency_curves(example)

    testthat::expect_equal(
      result$shared_frequencies,
      c(10000, 100000)
    )

    testthat::expect_equal(
      result$curve_summaries$auc_log_frequency,
      c(3, 7)
    )

    testthat::expect_equal(
      result$status,
      "COMMON_FREQUENCY_AUC_AVAILABLE"
    )
  }
)

testthat::test_that(
  "conflicting water classifications are rejected", {

    example <- data.frame(
      material = rep("A", 4),
      water = c("w", "d", "w", "w"),
      age_hours = c(0, 0, 48, 48),
      frequency_hz = c(10000, 20000, 10000, 20000),
      permittivity = c(2, 4, 6, 8)
    )

    testthat::expect_error(
      compare_common_frequency_curves(example),
      "Conflicting or missing experimental classifications"
    )
  }
)

testthat::test_that(
  "no shared frequencies are rejected", {

    example <- data.frame(
      material = rep("A", 4),
      age_hours = c(0, 0, 48, 48),
      frequency_hz = c(10000, 20000, 30000, 40000),
      permittivity = c(2, 4, 6, 8)
    )

    testthat::expect_error(
      compare_common_frequency_curves(example),
      "No exact frequencies"
    )
  }
)

