# ============================================================
# DielectricWaterR
# Automated tests for matched fingerprint comparisons
# ============================================================

test_that("matched comparisons are calculated correctly", {
  
  example_data <- data.frame(
    material = c(
      "5% petrol in well water",
      "5% diesel in well water",
      "5% petrol in well water",
      "5% diesel in well water"
    ),
    fuel = c("Petrol", "Diesel", "Petrol", "Diesel"),
    water = c("w", "w", "w", "w"),
    concentration_pct = c(5, 5, 5, 5),
    age_hours = c(0, 0, 0, 0),
    frequency_hz = c(100, 100, 1000, 1000),
    permittivity = c(20, 10, 4, 8)
  )
  
  expect_warning(
    result <- compare_dielectric_fingerprints(
      example_data
    ),
    "Physical sample IDs unavailable"
  )
  
  expect_equal(nrow(result$matched), 2)
  
  expect_equal(
    result$matched$petrol_diesel_ratio,
    c(2, 0.5)
  )
  
  expect_equal(nrow(result$duplicate_audit), 0)
})


test_that("coding conflicts are excluded", {
  
  example_data <- data.frame(
    material = c(
      "5% petrol in well water",
      "5% diesel in well water"
    ),
    fuel = c("Petrol", "Diesel"),
    water = c("d", "w"),
    concentration_pct = c(5, 5),
    age_hours = c(0, 0),
    frequency_hz = c(100, 100),
    permittivity = c(20, 10)
  )
  
  expect_warning(
    result <- compare_dielectric_fingerprints(
      example_data
    ),
    "Physical sample IDs unavailable"
  )
  
  expect_equal(nrow(result$matched), 0)
})


test_that("unsupported response variables are rejected", {
  
  example_data <- data.frame(
    material = "5% petrol in well water",
    fuel = "Petrol",
    water = "w",
    concentration_pct = 5,
    age_hours = 0,
    frequency_hz = 100,
    permittivity = 20
  )
  
  expect_error(
    compare_dielectric_fingerprints(
      example_data,
      response = "unknown_response"
    ),
    "Unsupported response variable"
  )
})


test_that("missing required columns are detected", {
  
  example_data <- data.frame(
    fuel = "Petrol",
    permittivity = 20
  )
  
  expect_error(
    compare_dielectric_fingerprints(
      example_data
    ),
    "Missing columns"
  )
})