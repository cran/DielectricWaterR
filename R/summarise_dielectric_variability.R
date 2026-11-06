
#' Summarise dielectric measurements without assuming independence
#'
#' Calculates descriptive statistics within experimental conditions.
#' Standard errors and confidence intervals are deliberately not
#' calculated because physical sample independence is not established.
#'
#' @param data A dielectric measurement data frame.
#' @param outcome Name of a numeric outcome column.
#' @param group_vars Names of grouping columns.
#'
#' @return A data frame of descriptive summaries.
#' @export
summarise_dielectric_variability <- function(
    data,
    outcome = "permittivity",
    group_vars = c("fuel", "water", "concentration_pct",
                   "age_hours")
) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  if (!is.character(outcome) ||
      length(outcome) != 1L ||
      is.na(outcome) ||
      !outcome %in% names(data)) {
    stop("outcome must name an existing column.")
  }

  if (!is.numeric(data[[outcome]])) {
    stop("The outcome column must be numeric.")
  }

  if (!is.character(group_vars) ||
      anyNA(group_vars) ||
      anyDuplicated(group_vars) ||
      !all(group_vars %in% names(data)) ||
      outcome %in% group_vars) {
    stop("group_vars must be distinct, valid grouping columns.")
  }

  if (length(group_vars) == 0L) {
    groups <- list(seq_len(nrow(data)))
    keys <- data.frame(.group = 1L)
  } else {
    keys <- unique(data[group_vars])
    groups <- lapply(seq_len(nrow(keys)), function(i) {
      selected <- rep(TRUE, nrow(data))

      for (v in group_vars) {
        x <- data[[v]]
        value <- keys[[v]][i]

        selected <- selected &
          if (is.na(value)) is.na(x) else !is.na(x) & x == value
      }

      which(selected)
    })
  }

  if (length(groups) == 0L) {
    stop("No observations available.")
  }

  summaries <- lapply(groups, function(idx) {
    x <- data[[outcome]][idx]
    x <- x[is.finite(x)]

    data.frame(
      n_measurements = length(x),
      mean = if (length(x)) mean(x) else NA_real_,
      median = if (length(x)) median(x) else NA_real_,
      sd = if (length(x) > 1L) stats::sd(x) else NA_real_,
      minimum = if (length(x)) min(x) else NA_real_,
      maximum = if (length(x)) max(x) else NA_real_
    )
  })

  summary_data <- do.call(rbind, summaries)
  rownames(summary_data) <- NULL

  if (length(group_vars) == 0L) {
    return(summary_data)
  }

  cbind(keys, summary_data)
}

