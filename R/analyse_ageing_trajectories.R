
#' Analyse descriptive dielectric ageing trajectories
#'
#' Summarises dielectric responses across ageing times
#' at a fixed frequency and within the same material.
#' Does not assume repeated measurements of one physical sample.
#'
#' @param data Dielectric measurement data frame.
#' @param outcome Numeric dielectric response column.
#' @param frequency_hz Frequency to analyse.
#' @param material Optional material label to select.
#'
#' @return Data frame containing ageing time, mean response,
#'   number of measurements and descriptive changes.
#' @export
analyse_ageing_trajectories <- function(
    data,
    outcome = "permittivity",
    frequency_hz,
    material = NULL
) {

  required <- c(
    "material", "age_hours", "frequency_hz", outcome
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
    stop("outcome must name a numeric column.")
  }

  if (missing(frequency_hz) ||
      !is.numeric(frequency_hz) ||
      length(frequency_hz) != 1L ||
      !is.finite(frequency_hz) ||
      frequency_hz <= 0) {
    stop("A positive frequency_hz is required.")
  }

  if (!is.null(material)) {
    if (!is.character(material) ||
        length(material) != 1L ||
        is.na(material)) {
      stop("material must be one material label.")
    }
  }

  selected <- data[
    !is.na(data$frequency_hz) &
      data$frequency_hz == frequency_hz,
    ,
    drop = FALSE
  ]

  if (!is.null(material)) {
    selected <- selected[
      !is.na(selected$material) &
        selected$material == material,
      ,
      drop = FALSE
    ]
  }

  selected <- selected[
    !is.na(selected$material) &
      is.finite(selected$age_hours) &
      is.finite(selected[[outcome]]),
    ,
    drop = FALSE
  ]

  if (nrow(selected) == 0L) {
    stop("No valid measurements at the selected frequency.")
  }

  # Check analytical classifications within material-age groups.
  condition_cols <- intersect(
    c("fuel", "water", "concentration_pct"),
    names(selected)
  )

  if (length(condition_cols) > 0L) {
    group_key <- interaction(
      selected$material, selected$age_hours,
      drop = TRUE
    )

    for (idx in split(seq_len(nrow(selected)), group_key)) {
      for (v in condition_cols) {
        values <- selected[[v]][idx]
        if (anyNA(values) ||
            (is.character(values) &&
             any(!nzchar(trimws(values)))) ||
            length(unique(values)) > 1L) {
          stop(
            "Conflicting or missing experimental classifications detected."
          )
        }
      }
    }
  }

  # Group by material and ageing time
  groups <- split(
    selected,
    interaction(
      selected$material,
      selected$age_hours,
      drop = TRUE
    )
  )

  summaries <- lapply(groups, function(x) {
    data.frame(
      material = as.character(x$material[1]),
      age_hours = x$age_hours[1],
      frequency_hz = frequency_hz,
      n_measurements = nrow(x),
      mean_response = mean(x[[outcome]]),
      median_response = stats::median(x[[outcome]])
    )
  })

  result <- do.call(rbind, summaries)

  result <- result[
    order(result$material, result$age_hours),
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL

  # Calculate changes relative to the earliest observed age
  result$baseline_response <- NA_real_
  result$absolute_change <- NA_real_
  result$percent_change <- NA_real_

  for (m in unique(result$material)) {

    idx <- which(result$material == m)

    baseline <- result$mean_response[idx[1]]

    result$baseline_response[idx] <- baseline

    result$absolute_change[idx] <-
      result$mean_response[idx] - baseline

    if (baseline != 0) {
      result$percent_change[idx] <-
        100 * result$absolute_change[idx] / abs(baseline)
    }
  }

  result
}

