
#' Audit observation exclusions for frequency-response analysis
#'
#' Classifies every observation according to its eligibility
#' for the primary descriptive frequency-response analysis.
#' Material-label agreement is a coding-consistency check,
#' not independent verification of the experimental medium.
#'
#' @param data A dielectric dataset returned by read_dielectric().
#'
#' @return A data frame containing the original observations,
#'   their row identifiers, expected water codes, medium-label
#'   status, and analysis inclusion decisions.
#'
#' @importFrom rlang .data
#' @export
audit_frequency_exclusions <- function(data) {

  required <- c(
    "material",
    "fuel",
    "water",
    "concentration_pct",
    "age_hours",
    "frequency_hz"
  )

  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  result <- data |>
    dplyr::mutate(
      observation_id = dplyr::row_number(),

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
      ),

      water_status = dplyr::case_when(
        is.na(.data$expected_water) ~ "Unverified",
        is.na(.data$water) ~ "Unverified",
        .data$water == .data$expected_water ~ "Verified",
        TRUE ~ "Conflict"
      ),

      analysis_decision = dplyr::case_when(
        .data$water_status == "Conflict" ~
          "Excluded: coding conflict",

        .data$water_status == "Unverified" ~
          "Excluded: unverified medium",

        is.na(.data$fuel) |
          !(.data$fuel %in% c("Petrol", "Diesel")) ~
          "Excluded: control or unknown fuel",

        is.na(.data$concentration_pct) |
          !is.finite(.data$concentration_pct) |
          is.na(.data$age_hours) |
          !is.finite(.data$age_hours) |
          is.na(.data$frequency_hz) |
          !is.finite(.data$frequency_hz) |
          .data$frequency_hz <= 0 ~
          "Excluded: missing/invalid condition",

        TRUE ~ "Included: primary analysis"
      )
    )

  result
}

