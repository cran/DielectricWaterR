
testthat::test_that(
  "grouped predictive workflow is leakage-free", {

    sample_info <- data.frame(
      sample_id = paste0("S", seq_len(20)),
      concentration_pct = rep(c(1, 5, 10, 20), 5)
    )

    example <- merge(
      sample_info,
      data.frame(
        frequency_hz = c(100, 1000, 10000)
      ),
      by = NULL
    )

    example$permittivity <-
      30 +
      0.5 * example$concentration_pct -
      2 * log10(example$frequency_hz)

    partition <- split_by_experimental_unit(
      example,
      unit_id = "sample_id",
      train_prop = 0.8,
      seed = 2026
    )

    testthat::expect_length(
      intersect(
        unique(partition$train$sample_id),
        unique(partition$test$sample_id)
      ),
      0
    )

    model <- stats::lm(
      permittivity ~ concentration_pct +
        log10(frequency_hz),
      data = partition$train
    )

    predicted <- stats::predict(
      model,
      newdata = partition$test
    )

    metrics <- evaluate_external_predictions(
      observed = partition$test$permittivity,
      predicted = predicted
    )

    testthat::expect_equal(
      metrics$n,
      nrow(partition$test)
    )

    testthat::expect_lt(metrics$mae, 1e-8)
    testthat::expect_lt(metrics$rmse, 1e-8)
    testthat::expect_lt(abs(metrics$bias), 1e-8)
  }
)

testthat::test_that(
  "grouped splitting preserves all observations", {

    example <- data.frame(
      sample_id = rep(paste0("S", 1:8), each = 4),
      response = seq_len(32)
    )

    partition <- split_by_experimental_unit(
      example,
      unit_id = "sample_id",
      seed = 100
    )

    testthat::expect_equal(
      nrow(partition$train) + nrow(partition$test),
      nrow(example)
    )

    testthat::expect_equal(
      partition$summary$train_units +
        partition$summary$test_units,
      8
    )
  }
)

