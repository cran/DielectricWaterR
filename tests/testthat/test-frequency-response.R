testthat::test_that("invalid minimum frequencies are rejected", {
    dat <- data.frame(material = "5% diesel in well water", fuel = "Diesel", water = "w", 
        concentration_pct = 5, age_hours = 0, frequency_hz = 10, permittivity = 1, 
        dissipation_factor = 1, impedance = 1)
    testthat::expect_error(analyse_frequency_response(dat, min_frequencies = 1), 
        "min_frequencies")
    testthat::expect_error(analyse_frequency_response(dat, min_frequencies = NA), 
        "min_frequencies")
})

testthat::test_that("missing required columns produce an error", {
    dat <- data.frame(fuel = "Diesel", water = "w")
    testthat::expect_error(analyse_frequency_response(dat), "Missing required columns")
})

testthat::test_that("insufficient frequency coverage is identified", {
    dat <- data.frame(material = rep("5% diesel in well water", 2), fuel = rep("Diesel", 
        2), water = rep("w", 2), concentration_pct = rep(5, 2), age_hours = rep(0, 
        2), frequency_hz = c(10, 100), permittivity = c(1, 2), dissipation_factor = c(3, 
        4), impedance = c(5, 6))
    result <- analyse_frequency_response(dat)
    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_false(result$eligible[1])
    testthat::expect_equal(result$distinct_frequencies[1], 2L)
})

