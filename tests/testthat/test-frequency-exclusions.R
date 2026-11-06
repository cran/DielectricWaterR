testthat::test_that("missing required columns produce an error", {
    dat <- data.frame(fuel = "Diesel", water = "w")
    testthat::expect_error(audit_frequency_exclusions(dat), "Missing required columns")
})

testthat::test_that("invalid frequencies are excluded", {
    dat <- data.frame(material = rep("5% diesel in well water", 3), fuel = rep("Diesel", 
        3), water = rep("w", 3), concentration_pct = rep(5, 3), age_hours = rep(0, 
        3), frequency_hz = c(10, 0, NA_real_))
    result <- audit_frequency_exclusions(dat)
    testthat::expect_equal(sum(result$analysis_decision == "Included: primary analysis"), 
        1L)
    testthat::expect_equal(sum(result$analysis_decision == "Excluded: missing/invalid condition"), 
        2L)
})

