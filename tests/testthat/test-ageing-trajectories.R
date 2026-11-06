
testthat::test_that(
  "ageing changes are calculated correctly", {

    example <- data.frame(
      material = rep("Material A", 3),
      age_hours = c(0, 48, 96),
      frequency_hz = rep(100000, 3),
      permittivity = c(2, 4, 6)
    )

    result <- analyse_ageing_trajectories(
      example,
      frequency_hz = 100000
    )

    testthat::expect_equal(nrow(result), 3L)
    testthat::expect_equal(
      result$mean_response, c(2, 4, 6)
    )
    testthat::expect_equal(
      result$absolute_change, c(0, 2, 4)
    )
    testthat::expect_equal(
      result$percent_change, c(0, 100, 200)
    )
  }
)

testthat::test_that(
  "different materials have separate baselines", {

    example <- data.frame(
      material = c("A", "A", "B", "B"),
      age_hours = c(0, 48, 0, 48),
      frequency_hz = rep(100000, 4),
      permittivity = c(2, 4, 10, 15)
    )

    result <- analyse_ageing_trajectories(
      example,
      frequency_hz = 100000
    )

    testthat::expect_equal(
      result$absolute_change,
      c(0, 2, 0, 5)
    )
  }
)

testthat::test_that(
  "different frequencies are not pooled", {

    example <- data.frame(
      material = rep("A", 3),
      age_hours = c(0, 0, 48),
      frequency_hz = c(100000, 500000, 100000),
      permittivity = c(2, 100, 4)
    )

    result <- analyse_ageing_trajectories(
      example,
      frequency_hz = 100000
    )

    testthat::expect_equal(nrow(result), 2L)
    testthat::expect_equal(
      result$mean_response, c(2, 4)
    )
  }
)

testthat::test_that(
  "unavailable frequency is rejected", {

    example <- data.frame(
      material = "A",
      age_hours = 0,
      frequency_hz = 100000,
      permittivity = 2
    )

    testthat::expect_error(
      analyse_ageing_trajectories(
        example,
        frequency_hz = 999999
      ),
      "No valid measurements"
    )
  }
)

