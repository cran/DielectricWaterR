
#' Estimate confidence intervals using independent physical units
#'
#' Computes a Student t confidence interval for the mean response
#' at a single experimental condition and frequency.
#' Multiple measurements from one physical unit are averaged first.
#' Physical sample identifiers must be externally verified.
#'
#' @param data A data frame containing dielectric measurements.
#' @param outcome Numeric outcome column name.
#' @param sample_id Column identifying independent physical units.
#' @param conf_level Confidence level between zero and one.
#'
#' @return One-row data frame containing the mean, interval,
#'   and number of independent physical units.
#' @export
estimate_dielectric_ci <- function(
    data,
    outcome = "permittivity",
    sample_id,
    conf_level = 0.95
) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("data must be a non-empty data frame.")
  }

  if (!is.character(outcome) ||
      length(outcome) != 1L ||
      is.na(outcome) ||
      !outcome %in% names(data) ||
      !is.numeric(data[[outcome]])) {
    stop("outcome must name an existing numeric column.")
  }

  if (missing(sample_id) ||
      !is.character(sample_id) ||
      length(sample_id) != 1L ||
      is.na(sample_id) ||
      !sample_id %in% names(data)) {
    stop("A valid physical sample ID column is required.")
  }

  if (!is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      !is.finite(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1) {
    stop("conf_level must be between zero and one.")
  }

  # Never pool different experimental conditions or frequencies.
  condition_cols <- c("age_hours", "frequency_hz")

  if (!all(condition_cols %in% names(data))) {
    stop("age_hours and frequency_hz are required.")
  }

  optional_conditions <- c(
    "material", "fuel", "water", "concentration_pct"
  )

  condition_cols <- c(
    intersect(optional_conditions, names(data)),
    condition_cols
  )

  for (v in condition_cols) {
    if (anyNA(data[[v]])) {
      stop("Experimental condition values must be complete.")
    }
    if (is.character(data[[v]]) &&
        any(!nzchar(trimws(data[[v]])))) {
      stop("Experimental condition values must be complete.")
    }
  }

  for (v in condition_cols) {
    if (length(unique(data[[v]])) != 1L) {
      stop(
        "Data must contain one condition and one frequency."
      )
    }
  }

  ids <- as.character(data[[sample_id]])

  if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
    stop("Physical sample identifiers must be complete.")
  }

  x <- data[[outcome]]

  if (any(!is.finite(x))) {
    stop("Outcome values must be finite.")
  }

  # Aggregate technical repeats within each physical unit.
  unit_means <- tapply(x, ids, mean)

  n <- length(unit_means)

  if (n < 2L) {
    stop("At least two independent physical units are required.")
  }

  estimate <- mean(unit_means)
  standard_error <- stats::sd(unit_means) / sqrt(n)

  critical_value <- stats::qt(
    (1 + conf_level) / 2,
    df = n - 1
  )

  margin <- critical_value * standard_error

  data.frame(
    independent_units = n,
    mean = estimate,
    standard_error = standard_error,
    confidence_level = conf_level,
    lower_ci = estimate - margin,
    upper_ci = estimate + margin
  )
}

