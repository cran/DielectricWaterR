
#' Compare dielectric measurements at two ageing times
#'
#' Matches measurements by fuel, water medium, concentration
#' and frequency. Returns descriptive ratios between two
#' ageing times. Ratios do not establish causal effects.
#'
#' @param data A dielectric measurement data frame.
#' @param age_start Starting ageing time in hours.
#' @param age_end Ending ageing time in hours.
#'
#' @return A data frame containing matched measurements
#'   and ratios for permittivity, dissipation factor
#'   and impedance.
#'
#' @export
compare_ageing <- function(data, age_start, age_end) {

  required <- c(
    "fuel", "water", "concentration_pct",
    "age_hours", "frequency_hz",
    "permittivity", "dissipation_factor",
    "impedance"
  )

  if (!is.data.frame(data) ||
      !all(required %in% names(data))) {
    stop("Data must contain all required dielectric columns.")
  }

  if (!is.numeric(age_start) ||
      length(age_start) != 1L ||
      !is.finite(age_start) ||
      !is.numeric(age_end) ||
      length(age_end) != 1L ||
      !is.finite(age_end) ||
      age_start == age_end) {
    stop("Specify two distinct, finite numeric ageing times.")
  }

  keys <- c(
    "fuel", "water", "concentration_pct",
    "frequency_hz"
  )

  selected <- data[
    !is.na(data$age_hours) &
      data$age_hours %in% c(age_start, age_end),
    ,
    drop = FALSE
  ]

  start <- selected[
    selected$age_hours == age_start,
    ,
    drop = FALSE
  ]

  end <- selected[
    selected$age_hours == age_end,
    ,
    drop = FALSE
  ]

  if (nrow(start) == 0L || nrow(end) == 0L) {
    stop("Both ageing times must have observations.")
  }

  if (anyNA(start[keys]) || anyNA(end[keys])) {
    stop("Matching variables must not contain missing values.")
  }

  if (anyDuplicated(start[keys]) ||
      anyDuplicated(end[keys])) {
    stop("Duplicate condition-frequency measurements detected.")
  }

  outcomes <- c(
    "permittivity",
    "dissipation_factor",
    "impedance"
  )

  matched <- merge(
    start[c(keys, outcomes)],
    end[c(keys, outcomes)],
    by = keys,
    suffixes = c("_start", "_end"),
    sort = TRUE
  )

  for (outcome in outcomes) {
    beginning <- matched[[paste0(outcome, "_start")]]
    ending <- matched[[paste0(outcome, "_end")]]

    if (!is.numeric(beginning) || !is.numeric(ending)) {
      stop("Dielectric outcomes must be numeric.")
    }

    ratio <- rep(NA_real_, length(beginning))

    valid <- is.finite(beginning) &
      is.finite(ending) &
      beginning != 0

    ratio[valid] <- ending[valid] / beginning[valid]

    matched[[paste0(outcome, "_ratio")]] <- ratio
  }

  matched$age_start <- age_start
  matched$age_end <- age_end

  matched
}

