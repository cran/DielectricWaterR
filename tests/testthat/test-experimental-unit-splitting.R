
testthat::test_that(
  "physical units never overlap between partitions", {

    example <- data.frame(
      sample_id = rep(paste0("S", 1:10), each = 5),
      frequency_hz = rep(
        c(100, 1000, 10000, 100000, 1000000),
        times = 10
      ),
      permittivity = seq_len(50)
    )

    result <- split_by_experimental_unit(
      example,
      unit_id = "sample_id",
      train_prop = 0.8,
      seed = 123
    )

    testthat::expect_length(
      intersect(
        unique(result$train$sample_id),
        unique(result$test$sample_id)
      ),
      0
    )

    testthat::expect_equal(
      nrow(result$train) + nrow(result$test),
      nrow(example)
    )

    testthat::expect_equal(result$summary$train_units, 8)
    testthat::expect_equal(result$summary$test_units, 2)
  }
)

testthat::test_that(
  "splitting is reproducible", {

    example <- data.frame(
      sample_id = rep(paste0("S", 1:10), each = 2),
      response = seq_len(20)
    )

    a <- split_by_experimental_unit(
      example, "sample_id", seed = 2026
    )

    b <- split_by_experimental_unit(
      example, "sample_id", seed = 2026
    )

    testthat::expect_identical(a$train, b$train)
    testthat::expect_identical(a$test, b$test)
  }
)

testthat::test_that(
  "invalid experimental units are rejected", {

    example <- data.frame(
      sample_id = c("S1", NA, "S2"),
      response = c(1, 2, 3)
    )

    testthat::expect_error(
      split_by_experimental_unit(example, "sample_id"),
      "cannot be missing"
    )

    example$sample_id <- c("S1", "S1", "S1")

    testthat::expect_error(
      split_by_experimental_unit(example, "sample_id"),
      "At least two distinct"
    )

    testthat::expect_error(
      split_by_experimental_unit(example, "unknown_column"),
      "valid experimental-unit column"
    )
  }
)

testthat::test_that(
  "random-number state is preserved", {

    example <- data.frame(
      sample_id = paste0("S", 1:10),
      response = seq_len(10)
    )

    set.seed(999)
    before <- .Random.seed

    split_by_experimental_unit(
      example, "sample_id", seed = 123
    )

    testthat::expect_identical(
      .Random.seed,
      before
    )
  }
)

