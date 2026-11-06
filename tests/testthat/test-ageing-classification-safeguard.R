
testthat::test_that(
  "conflicting water classifications are rejected", {

    example <- data.frame(
      material = rep("10% petrol in well water", 3),
      fuel = rep("Petrol", 3),
      water = c("w", "d", "w"),
      concentration_pct = rep(10, 3),
      age_hours = c(0, 0, 48),
      frequency_hz = rep(100000, 3),
      permittivity = c(2, 3, 4)
    )

    testthat::expect_error(
      analyse_ageing_trajectories(
        example,
        frequency_hz = 100000
      ),
      "Conflicting or missing experimental classifications"
    )
  }
)

testthat::test_that(
  "consistent classifications are accepted", {

    example <- data.frame(
      material = rep("10% petrol in well water", 3),
      fuel = rep("Petrol", 3),
      water = rep("w", 3),
      concentration_pct = rep(10, 3),
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
      result$mean_response,
      c(2, 4, 6)
    )
  }
)

