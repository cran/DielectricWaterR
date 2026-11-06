
#' Summarise dielectric frequency-response curves
#'
#' Calculates descriptive curve features using the
#' trapezoidal rule over log10(frequency).
#' Curves are defined by material and ageing time.
#' These summaries do not establish independent replication.
#'
#' @param data Dielectric measurement data frame.
#' @param outcome Name of a numeric response column.
#'
#' @return A data frame containing curve-level summaries.
#' @export
summarise_dielectric_curves <- function(
    data,
    outcome = "permittivity"
) {

  required <- c(
    "material", "age_hours", "frequency_hz"
  )

  if (!is.data.frame(data) ||
      !all(required %in% names(data))) {
    stop("Required columns are missing.")
  }

  if (!is.character(outcome) ||
      length(outcome) != 1L ||
      is.na(outcome) ||
      !outcome %in% names(data) ||
      !is.numeric(data[[outcome]])) {
    stop("outcome must name an existing numeric column.")
  }

  selected <- data[
    !is.na(data$material) &
      !is.na(data$age_hours) &
      is.finite(data$frequency_hz) &
      data$frequency_hz > 0 &
      is.finite(data[[outcome]]),
    ,
    drop = FALSE
  ]

  if (nrow(selected) == 0L) {
    stop("No valid curve measurements available.")
  }

  # Prevent inconsistent condition codes from being combined.
  condition_cols <- intersect(
    c("fuel", "water", "concentration_pct"),
    names(selected)
  )

  groups <- split(
    selected,
    interaction(
      selected$material,
      selected$age_hours,
      drop = TRUE
    )
  )

  summaries <- lapply(groups, function(x) {

    for (v in condition_cols) {
      values <- x[[v]]

      if (anyNA(values) ||
          (is.character(values) &&
           any(!nzchar(trimws(values)))) ||
          length(unique(values)) > 1L) {
        stop(
          "Conflicting or missing experimental classifications detected."
        )
      }
    }

    # Aggregate duplicate frequencies descriptively.
    frequency_data <- stats::aggregate(
      x[[outcome]],
      by = list(frequency_hz = x$frequency_hz),
      FUN = mean
    )

    names(frequency_data)[2] <- "response"

    frequency_data <- frequency_data[
      order(frequency_data$frequency_hz),
      ,
      drop = FALSE
    ]

    n_freq <- nrow(frequency_data)

    if (n_freq < 2L) {
      return(NULL)
    }

    log_frequency <- log10(frequency_data$frequency_hz)
    response <- frequency_data$response

    delta <- diff(log_frequency)

    auc <- sum(
      delta *
        (utils::head(response, -1L) + utils::tail(response, -1L)) / 2
    )

    data.frame(
      material = as.character(x$material[1]),
      age_hours = x$age_hours[1],
      n_measurements = nrow(x),
      n_frequencies = n_freq,
      min_frequency_hz = min(frequency_data$frequency_hz),
      max_frequency_hz = max(frequency_data$frequency_hz),
      auc_log_frequency = auc,
      mean_curve_response = auc /
        (max(log_frequency) - min(log_frequency)),
      stringsAsFactors = FALSE
    )
  })

  summaries <- Filter(Negate(is.null), summaries)

  if (length(summaries) == 0L) {
    stop("No curves have at least two valid frequencies.")
  }

  result <- do.call(rbind, summaries)

  result <- result[
    order(result$material, result$age_hours),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  result
}

