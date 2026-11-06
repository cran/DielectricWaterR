
#' Audit experimental-unit information
#'
#' Summarises measurements by material label and reports
#' whether physical sample identities can be established.
#' Material names are not assumed to identify physical samples.
#'
#' @param data A dielectric measurement data frame.
#' @param sample_id Optional column name containing genuine
#'   physical sample identifiers.
#'
#' @return A data frame summarising material labels,
#'   ageing times, frequencies and sample identity status.
#'
#' @export
#' @examples
#' # Synthetic demonstration; not experimental observations.
#' x <- data.frame(
#'   material = rep(c('Water A', 'Water B'), each = 2),
#'   age_hours = c(0, 0, 0, 0),
#'   frequency_hz = c(10, 100, 10, 100),
#'   sample_id = c('A1', 'A1', 'B1', 'B1')
#' )
#'
#' audit_experimental_units(x)
#' audit_experimental_units(x, sample_id = 'sample_id')
audit_experimental_units <- function(data, sample_id = NULL) {

  required <- c(
    "material",
    "age_hours",
    "frequency_hz"
  )

  if (!is.data.frame(data) ||
      !all(required %in% names(data))) {
    stop("Data must contain material, age_hours and frequency_hz.")
  }

  if (!is.null(sample_id)) {
    if (!is.character(sample_id) ||
        length(sample_id) != 1L ||
        is.na(sample_id) ||
        !sample_id %in% names(data)) {
      stop("sample_id must name an existing column.")
    }
  }

  materials <- unique(as.character(data$material))

  results <- lapply(materials, function(m) {

    rows <- if (is.na(m)) {
      is.na(data$material)
    } else {
      !is.na(data$material) &
        as.character(data$material) == m
    }

    subset <- data[rows, , drop = FALSE]

    ages <- unique(subset$age_hours[
      !is.na(subset$age_hours)
    ])

    frequencies <- unique(subset$frequency_hz[
      !is.na(subset$frequency_hz)
    ])

    if (is.null(sample_id)) {

      n_samples <- NA_integer_
      identity_status <- "Unknown physical sample identity"

    } else {

      ids <- as.character(subset[[sample_id]])
      ids <- unique(ids[!is.na(ids) & nzchar(trimws(ids))])

      n_samples <- length(ids)

      identity_status <- if (
        anyNA(subset[[sample_id]]) ||
        any(!nzchar(trimws(as.character(subset[[sample_id]]))))
      ) {
        "Incomplete physical sample identifiers"
      } else {
        "Sample identifiers supplied"
      }
    }

    data.frame(
      material = m,
      observations = nrow(subset),
      ageing_times = length(ages),
      frequencies = length(frequencies),
      sample_ids = n_samples,
      identity_status = identity_status,
      stringsAsFactors = FALSE
    )
  })

  if (length(results) == 0L) {
    return(data.frame(
      material = character(),
      observations = integer(),
      ageing_times = integer(),
      frequencies = integer(),
      sample_ids = integer(),
      identity_status = character()
    ))
  }

  do.call(rbind, results)
}

