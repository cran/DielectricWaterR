#' Plot dielectric frequency-response curves
#'
#' Produces descriptive frequency-response curves by material
#' and recorded ageing time. Curves do not establish independent
#' physical replication or causal effects.
#'
#' @param data Data frame containing dielectric measurements.
#' @param response One of permittivity, dissipation_factor, impedance.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param show_legend Whether to display material and ageing legends.
#' @return A ggplot object.
#' @export
#' @examples
#' # Fully synthetic values, not experimental observations.
#' x <- expand.grid(
#'   material = c('Water A', 'Water B'),
#'   age_hours = c(0, 24),
#'   frequency_hz = c(10, 100, 1000, 10000)
#' )
#' x$permittivity <- 80 -
#'   4 * log10(x$frequency_hz) -
#'   0.1 * x$age_hours -
#'   ifelse(x$material == 'Water B', 3, 0)
#'
#' plot_fingerprints(
#'   x,
#'   response = 'permittivity',
#'   title = 'Synthetic dielectric responses',
#'   subtitle = 'Illustrative data only'
#' )
plot_fingerprints <- function(
    data,
    response = 'permittivity',
    title = 'Descriptive dielectric frequency-response curves',
    subtitle = 'Descriptive measurements; experimental independence must be verified',
    show_legend = TRUE
) {

  allowed <- c('permittivity', 'dissipation_factor', 'impedance')

  if (!is.data.frame(data)) {
    stop('data must be a data frame.')
  }

  if (!is.character(response) || length(response) != 1L ||
      is.na(response) || !response %in% allowed) {
    stop('Unknown response.')
  }

  required <- c('material', 'age_hours', 'frequency_hz', response)
  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0L) {
    stop('Missing columns: ', paste(missing_cols, collapse = ', '))
  }

  if (!is.numeric(data$frequency_hz) ||
      any(!is.finite(data$frequency_hz) |
          data$frequency_hz <= 0, na.rm = TRUE)) {
    stop('Frequency must contain positive finite values.')
  }

  if (!is.numeric(data$age_hours)) {
    stop('age_hours must be numeric.')
  }

  if (!is.numeric(data[[response]])) {
    stop('Selected response must be numeric.')
  }

  if (!is.logical(show_legend) ||
      length(show_legend) != 1L || is.na(show_legend)) {
    stop('show_legend must be TRUE or FALSE.')
  }

  plot_data <- data
  plot_data$age_label <- factor(
    plot_data$age_hours,
    levels = sort(unique(plot_data$age_hours[
      !is.na(plot_data$age_hours)
    ])),
    ordered = TRUE
  )

  levels(plot_data$age_label) <- paste0(
    levels(plot_data$age_label), ' hours'
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$frequency_hz,
      y = .data[[response]],
      colour = .data$material,
      linetype = .data$age_label,
      group = interaction(.data$material, .data$age_label)
    )
  ) +
    ggplot2::geom_line(na.rm = TRUE, linewidth = 0.8) +
    ggplot2::geom_point(size = 1.7, na.rm = TRUE) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = 'Frequency (Hz)',
      y = response,
      title = title,
      subtitle = subtitle,
      colour = 'Material',
      linetype = 'Ageing time'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = if (show_legend) 'bottom' else 'none',
      legend.box = 'vertical'
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(ncol = 2, order = 1),
      linetype = ggplot2::guide_legend(order = 2)
    )

  p
}
