
#' Evaluate regression predictions
#'
#' Calculates descriptive prediction errors.
#' Does not establish external-data independence.
#'
#' @param observed Numeric observed responses.
#' @param predicted Numeric predicted responses.
#' @return One-row data frame with n, MAE, RMSE and bias.
#' @export
evaluate_external_predictions <- function(observed, predicted) {
  if (!is.numeric(observed) || !is.numeric(predicted))
    stop("Observed and predicted must be numeric.")

  if (length(observed) != length(predicted))
    stop("Observed and predicted lengths must match.")

  if (length(observed) == 0L)
    stop("At least one prediction is required.")

  if (any(!is.finite(observed)) ||
      any(!is.finite(predicted)))
    stop("Observed and predicted values must be finite.")

  errors <- predicted - observed

  data.frame(
    n = length(observed),
    mae = mean(abs(errors)),
    rmse = sqrt(mean(errors^2)),
    bias = mean(errors)
  )
}

