
#' Summarise dielectric frequency responses
#'
#' Provides descriptive summaries of dielectric measurements
#' across experimental conditions and frequencies.
#' Rows with unverified or conflicting water-medium labels
#' are excluded from the primary analysis.
#' No independent-sample inference is performed.
#'
#' @param data A dielectric dataset returned by read_dielectric().
#' @param min_frequencies Minimum distinct frequencies per curve.
#' @return A tibble containing descriptive curve summaries.
#' @importFrom rlang .data
#' @export
analyse_frequency_response <- function(
    data,
    min_frequencies = 5L
) {

  required <- c(
    "material", "fuel", "water",
    "concentration_pct", "age_hours",
    "frequency_hz", "permittivity",
    "dissipation_factor", "impedance"
  )

  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (
    length(min_frequencies) != 1L ||
    !is.numeric(min_frequencies) ||
    is.na(min_frequencies) ||
    !is.finite(min_frequencies) ||
    min_frequencies < 2 ||
    min_frequencies != floor(min_frequencies)
  ) {
    stop("min_frequencies must be an integer >= 2.")
  }

  audited <- data |>
    dplyr::mutate(
      expected_water = dplyr::case_when(
        grepl(
          "distilled water",
          .data$material,
          ignore.case = TRUE
        ) ~ "d",

        grepl(
          "well water",
          .data$material,
          ignore.case = TRUE
        ) ~ "w",

        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      !is.na(.data$expected_water),
      .data$water == .data$expected_water,
      .data$fuel %in% c("Petrol", "Diesel"),
      !is.na(.data$concentration_pct),
      !is.na(.data$age_hours),
      !is.na(.data$frequency_hz),
      .data$frequency_hz > 0
    )

  result <- audited |>
    dplyr::group_by(
      .data$fuel,
      .data$water,
      .data$concentration_pct,
      .data$age_hours
    ) |>
    dplyr::summarise(
      observations = dplyr::n(),

      distinct_frequencies =
        dplyr::n_distinct(.data$frequency_hz),

      duplicate_frequencies =
        dplyr::n() -
        dplyr::n_distinct(.data$frequency_hz),

      min_frequency_hz =
        min(.data$frequency_hz),

      max_frequency_hz =
        max(.data$frequency_hz),

      median_permittivity =
        stats::median(
          .data$permittivity,
          na.rm = TRUE
        ),

      median_dissipation =
        stats::median(
          .data$dissipation_factor,
          na.rm = TRUE
        ),

      median_impedance = if (
        all(is.na(.data$impedance))
      ) {
        NA_real_
      } else {
        stats::median(
          .data$impedance,
          na.rm = TRUE
        )
      },

      .groups = "drop"
    ) |>
    dplyr::mutate(
      eligible =
        .data$distinct_frequencies >= min_frequencies &
        .data$duplicate_frequencies == 0L
    ) |>
    dplyr::arrange(
      .data$fuel,
      .data$water,
      .data$concentration_pct,
      .data$age_hours
    )

  result
}

