
#' Split measurements by verified experimental unit
#'
#' Keeps every observation belonging to the same physical
#' experimental unit in either training or testing data.
#' Identifiers must be supplied and independently verified.
#' This function cannot verify physical sample provenance.
#'
#' @param data Data frame containing measurements.
#' @param unit_id Name of verified experimental-unit column.
#' @param train_prop Proportion of unique units for training.
#' @param seed Optional non-negative integer seed. NULL uses and advances the caller's random-number state. An explicit seed gives reproducible results and restores the caller's random-number state.
#' @return List containing training data, testing data,
#'   and a summary of the split.
#' @export
split_by_experimental_unit <- function(
    data,
    unit_id,
    train_prop = 0.8,
    seed = NULL
) {

  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("Data must be a non-empty data frame.")
  }

  if (!is.character(unit_id) ||
      length(unit_id) != 1L ||
      is.na(unit_id) ||
      !unit_id %in% names(data)) {
    stop("A valid experimental-unit column is required.")
  }

  ids <- data[[unit_id]]

  if (!(is.character(ids) ||
        is.factor(ids) ||
        is.numeric(ids))) {
    stop("Experimental-unit identifiers must be atomic.")
  }

  if (anyNA(ids) ||
      any(!nzchar(trimws(as.character(ids))))) {
    stop("Experimental-unit identifiers cannot be missing.")
  }

  if (!is.numeric(train_prop) ||
      length(train_prop) != 1L ||
      !is.finite(train_prop) ||
      train_prop <= 0 ||
      train_prop >= 1) {
    stop("train_prop must be between zero and one.")
  }

  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L ||
       !is.finite(seed) || seed < 0 ||
       seed > .Machine$integer.max || seed != floor(seed))) {
    stop("seed must be NULL or a non-negative integer.")
  }

  units <- unique(as.character(ids))

  if (length(units) < 2L) {
    stop("At least two distinct experimental units are required.")
  }

  n_train <- max(
    1L,
    min(
      length(units) - 1L,
      floor(length(units) * train_prop)
    )
  )

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv,
                       inherits = FALSE)
    if (had_seed) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv,
                        inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  train_units <- sample(units, n_train)

  train_rows <- as.character(ids) %in% train_units

  train_data <- data[train_rows, , drop = FALSE]
  test_data <- data[!train_rows, , drop = FALSE]

  if (length(intersect(
    unique(as.character(train_data[[unit_id]])),
    unique(as.character(test_data[[unit_id]]))
  )) != 0L) {
    stop("Experimental-unit leakage detected.")
  }

  list(
    train = train_data,
    test = test_data,
    summary = data.frame(
      n_units = length(units),
      train_units = n_train,
      test_units = length(units) - n_train,
      train_rows = nrow(train_data),
      test_rows = nrow(test_data)
    )
  )
}

