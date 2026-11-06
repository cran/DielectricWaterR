#' @importFrom rlang .data
#' @importFrom stats median
NULL
#' Compare Matched Dielectric Fingerprints
#'
#' Compare petrol and diesel at identical experimental
#' conditions using descriptive response ratios.
#'
#' @param data Data returned by read_dielectric().
#' @param response One of permittivity, dissipation_factor,
#'   or impedance.
#' @param age_hours Measurement age in hours.
#' @param max_concentration Maximum concentration percentage.
#' @param exclude_conflicts Exclude water-code mismatches.
#'
#' @return A list containing matched results, summary,
#'   and duplicate-condition diagnostics.
#' @export
compare_dielectric_fingerprints <- function(
    data,
    response = "permittivity",
    age_hours = 0,
    max_concentration = 20,
    exclude_conflicts = TRUE
) {
  
  allowed <- c(
    "permittivity",
    "dissipation_factor",
    "impedance"
  )
  
  if (!response %in% allowed) {
    stop("Unsupported response variable.")
  }
  
  required <- c(
    "material", "fuel", "water",
    "concentration_pct", "age_hours",
    "frequency_hz", response
  )
  
  missing_columns <- setdiff(required, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  df <- as.data.frame(data)
  
  expected_water <- ifelse(
    grepl("distilled water", df$material,
          ignore.case = TRUE),
    "d",
    ifelse(
      grepl("well water", df$material,
            ignore.case = TRUE),
      "w",
      NA_character_
    )
  )
  
  df$water <- trimws(tolower(as.character(df$water)))
  
  df$coding_conflict <- !is.na(expected_water) &
    (is.na(df$water) | expected_water != df$water)
  
  df$coding_conflict[is.na(df$coding_conflict)] <- FALSE
  
  keep <- !is.na(df$age_hours) &
    df$age_hours == age_hours &
    !is.na(df$concentration_pct) &
    df$concentration_pct >= 0 &
    df$concentration_pct <= max_concentration &
    !is.na(df$frequency_hz) &
    df$frequency_hz > 0 &
    !is.na(df$fuel) &
    df$fuel %in% c("Petrol", "Diesel") &
    !is.na(df$water) &
    df$water %in% c("d", "w")
  
  if (exclude_conflicts) {
    keep <- keep & !df$coding_conflict
  }
  
  df <- df[keep, , drop = FALSE]
  
  keys <- c(
    "material", "fuel", "water",
    "concentration_pct", "age_hours",
    "frequency_hz"
  )
  
  duplicate_audit <- df |>
    dplyr::count(
      dplyr::across(dplyr::all_of(keys)),
      name = "observations"
    ) |>
    dplyr::filter(.data$observations > 1)
  
  condition_data <- df |>
    dplyr::group_by(
      .data$water,
      .data$concentration_pct,
      .data$frequency_hz,
      .data$fuel
    ) |>
    dplyr::summarise(
      n_observed = sum(!is.na(.data[[response]])),
      value = if (all(is.na(.data[[response]]))) {
        NA_real_
      } else {
        median(.data[[response]], na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  matched <- condition_data |>
    dplyr::select(
      dplyr::all_of(c(
        "water",
        "concentration_pct",
        "frequency_hz",
        "fuel",
        "value"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = "fuel",
      values_from = "value"
    )
  
  for (fuel_name in c("Petrol", "Diesel")) {
    if (!fuel_name %in% names(matched)) {
      matched[[fuel_name]] <- NA_real_
    }
  }
  
  matched <- matched |>
    dplyr::filter(
      !is.na(.data$Petrol),
      !is.na(.data$Diesel),
      .data$Petrol > 0,
      .data$Diesel > 0
    ) |>
    dplyr::mutate(
      petrol_diesel_ratio = .data$Petrol / .data$Diesel,
      log10_ratio = log10(.data$petrol_diesel_ratio)
    ) |>
    dplyr::arrange(
      .data$water,
      .data$concentration_pct,
      .data$frequency_hz
    )
  
  summary <- matched |>
    dplyr::group_by(.data$water) |>
    dplyr::summarise(
      matched_conditions = dplyr::n(),
      petrol_higher = sum(.data$log10_ratio > 0),
      diesel_higher = sum(.data$log10_ratio < 0),
      median_ratio = median(.data$petrol_diesel_ratio),
      .groups = "drop"
    )
  
  if (!"sample_id" %in% names(data)) {
    warning(
      "Physical sample IDs unavailable; ",
      "results are descriptive, not independent-sample inference."
    )
  }
  
  list(
    response = response,
    matched = matched,
    summary = summary,
    duplicate_audit = duplicate_audit
  )
}