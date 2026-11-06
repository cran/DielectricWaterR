{
    fixture <- system.file("extdata", "synthetic_import.xlsx", package = "DielectricWaterR")
    testthat::test_that("public Excel fixture is installed", {
        testthat::expect_true(nzchar(fixture))
        testthat::expect_true(file.exists(fixture))
    })
    testthat::test_that("standard headers and sheet indices work", {
        x <- read_dielectric(fixture, "Measurements", layout = "standard")
        y <- read_dielectric(fixture, 1, layout = "standard")
        testthat::expect_equal(nrow(x), 8L)
        testthat::expect_equal(x, y)
        testthat::expect_true("sample_id" %in% names(x))
        testthat::expect_equal(x$frequency_hz, rep(c(100, 1000), each = 4))
    })
    testthat::test_that("explicit column mappings work", {
        x <- read_dielectric(fixture, "CustomHeaders", column_map = c(frequency_hz = "Frequency", 
            permittivity = "Epsilon"))
        y <- read_dielectric(fixture, "Measurements")
        testthat::expect_equal(x, y)
    })
    testthat::test_that("both legacy layouts work", {
        x <- read_dielectric(fixture, "All extract_mean", layout = "legacy")
        y <- read_dielectric(fixture, "COded", layout = "legacy")
        testthat::expect_equal(nrow(x), 8L)
        testthat::expect_equal(ncol(x), 11L)
        testthat::expect_equal(ncol(y), 9L)
        testthat::expect_equal(x$permittivity, y$permittivity)
        testthat::expect_equal(x, read_dielectric(fixture))
        testthat::expect_false("fuel" %in% names(y))
    })
    testthat::test_that("invalid inputs fail informatively", {
        testthat::expect_error(read_dielectric(fixture, "Unknown"), "Worksheet not found")
        testthat::expect_error(read_dielectric(fixture, "MissingColumn", layout = "standard"), 
            "Missing standard columns")
        testthat::expect_error(read_dielectric(fixture, "Measurements", column_map = c(frequency_hz = "Absent")), 
            "Mapped source columns")
        testthat::expect_error(read_dielectric(fixture, 999), "valid worksheet")
    })
    testthat::test_that("numeric text conversion works", {
        x <- read_dielectric(fixture, "TextNumbers")
        testthat::expect_type(x$frequency_hz, "double")
        testthat::expect_warning(y <- read_dielectric(fixture, "Malformed"), "Non-numeric entries")
        testthat::expect_true(is.na(y$frequency_hz[1]))
    })
    testthat::test_that("public imported data exercise analysis", {
        x <- read_dielectric(fixture, "Measurements")
        audit <- audit_frequency_exclusions(x)
        testthat::expect_equal(nrow(audit), 8L)
        testthat::expect_true(all(audit$analysis_decision == "Included: primary analysis"))
        primary <- audit[audit$analysis_decision == "Included: primary analysis", 
            , drop = FALSE]
        a <- analyse_frequency_response(x, min_frequencies = 2)
        b <- analyse_frequency_response(primary, min_frequencies = 2)
        testthat::expect_equal(a, b)
        testthat::expect_equal(nrow(a), 4L)
        testthat::expect_true(all(a$eligible))
        testthat::expect_true(all(a$distinct_frequencies == 2L))
        testthat::expect_true(all(a$duplicate_frequencies == 0L))
    })
}
