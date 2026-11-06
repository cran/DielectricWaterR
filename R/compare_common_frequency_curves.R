
#' Compare dielectric curves at exactly shared frequencies
#'
#' Uses only frequencies observed in every material-age group.
#' Does not interpolate, extrapolate, or infer sample identity.
#' At least two shared frequencies are required for AUC.
#'
#' @param data Dielectric measurement data frame.
#' @param outcome Numeric response column name.
#' @return A list with shared frequencies, aligned responses,
#'   and curve-level AUC summaries when possible.
#' @export
compare_common_frequency_curves <- function(
    data,
    outcome = "permittivity"
) {

  required <- c(
    "material", "age_hours", "frequency_hz", outcome
  )

  if (!is.data.frame(data) ||
      !is.character(outcome) ||
      length(outcome) != 1L ||
      is.na(outcome) ||
      !all(required %in% names(data))) {
    stop("Required columns are missing or outcome is invalid.")
  }

  if (!is.numeric(data[[outcome]]) ||
      !is.numeric(data$age_hours) ||
      !is.numeric(data$frequency_hz)) {
    stop("Outcome, age and frequency must be numeric.")
  }

  if (nrow(data) == 0L ||
      anyNA(data$material) ||
      any(!nzchar(trimws(as.character(data$material)))) ||
      any(!is.finite(data$age_hours)) ||
      any(!is.finite(data$frequency_hz)) ||
      any(data$frequency_hz <= 0) ||
      any(!is.finite(data[[outcome]]))) {
    stop("Data contain missing or invalid measurements.")
  }

  # Verify condition consistency within each material-age group.
  condition_cols <- intersect(
    c("fuel", "water", "concentration_pct"),
    names(data)
  )

  groups <- split(
    data,
    interaction(
      data$material,
      data$age_hours,
      drop = TRUE
    )
  )

  for (x in groups) {
    for (v in condition_cols) {
      values <- x[[v]]

      if (anyNA(values) ||
          (is.character(values) &&
           any(!nzchar(trimws(values)))) ||
          length(unique(values)) != 1L) {
        stop(
          "Conflicting or missing experimental classifications."
        )
      }
    }
  }

  # Identify exactly shared measured frequencies.
  frequency_sets <- lapply(
    groups,
    function(x) sort(unique(x$frequency_hz))
  )

  shared <- sort(Reduce(intersect, frequency_sets))

  if (length(shared) == 0L) {
    stop("No exact frequencies are shared by all groups.")
  }

  # Align responses at shared frequencies.
  aligned <- do.call(
    rbind,
    lapply(groups, function(x) {

      x <- x[x$frequency_hz %in% shared, , drop = FALSE]

      response <- stats::aggregate(
        x[[outcome]],
        by = list(frequency_hz = x$frequency_hz),
        FUN = mean
      )

      names(response)[2] <- "response"

      response$material <- as.character(x$material[1])
      response$age_hours <- x$age_hours[1]

      response
    })
  )

  aligned <- aligned[
    order(
      aligned$material,
      aligned$age_hours,
      aligned$frequency_hz
    ),
    ,
    drop = FALSE
  ]

  rownames(aligned) <- NULL

  # A single shared frequency is not a functional curve.
  if (length(shared) < 2L) {
    return(list(
      shared_frequencies = shared,
      aligned_responses = aligned,
      curve_summaries = NULL,
      status = "INSUFFICIENT_SHARED_FREQUENCIES_FOR_AUC"
    ))
  }

  aligned_groups <- split(
    aligned,
    interaction(
      aligned$material,
      aligned$age_hours,
      drop = TRUE
    )
  )

  summaries <- lapply(aligned_groups, function(x) {

    x <- x[order(x$frequency_hz), , drop = FALSE]

    log_f <- log10(x$frequency_hz)

    auc <- sum(
      diff(log_f) *
        (
          utils::head(x$response, -1L) +
          utils::tail(x$response, -1L)
        ) / 2
    )

    data.frame(
      material = x$material[1],
      age_hours = x$age_hours[1],
      n_shared_frequencies = length(shared),
      auc_log_frequency = auc,
      mean_curve_response = auc /
        diff(range(log_f))
    )
  })

  curve_summaries <- do.call(rbind, summaries)

  rownames(curve_summaries) <- NULL

  list(
    shared_frequencies = shared,
    aligned_responses = aligned,
    curve_summaries = curve_summaries,
    status = "COMMON_FREQUENCY_AUC_AVAILABLE"
  )
}

