#' Import dielectric measurements from an Excel workbook
#'
#' Supports standard column names, explicit header mappings,
#' and the two original positional worksheet layouts.
#' No experimental classifications or physical sample IDs are inferred.
#'
#' @param path Path to an Excel workbook.
#' @param sheet Worksheet name or index. NULL selects All extract_mean
#'   when present, otherwise the first worksheet.
#' @param column_map Named character vector mapping standard column
#'   names to workbook headers, for example
#'   c(frequency_hz = 'Frequency', permittivity = 'Epsilon').
#' @param layout One of auto, standard, or legacy. Auto first accepts
#'   standard headers, then falls back to known legacy sheet layouts.
#' @details Standard imports require material, age_hours, frequency_hz,
#'   permittivity, dissipation_factor, and impedance.
#'   Optional columns include fuel, water, concentration_pct and sample_id.
#'   Additional columns are retained in standard imports.
#'   Frequencies must already be expressed in Hz and ages in hours;
#'   renaming columns does not convert units.
#'   Analysis functions may require additional classification columns.
#'   The COded legacy layout does not supply fuel or water labels.
#' @return A data frame with standardised column names.
#' @export
#' @examples
#' path <- system.file('extdata', 'synthetic_import.xlsx',
#'                     package = 'DielectricWaterR')
#' x <- read_dielectric(path, sheet = 'Measurements', layout = 'standard')
#' head(x)
read_dielectric <- function (path, sheet = NULL, column_map = NULL, layout = c("auto", "standard", "legacy")) 
{
    layout <- match.arg(layout)
    if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
        stop("path must identify an existing Excel workbook.")
    }
    sheets <- readxl::excel_sheets(path)
    if (is.null(sheet)) {
        sheet <- if ("All extract_mean" %in% sheets) {
            "All extract_mean"
        }
        else {
            sheets[1L]
        }
    }
    if (is.numeric(sheet)) {
        if (length(sheet) != 1L || !is.finite(sheet) || sheet != floor(sheet) || 
            sheet < 1 || sheet > length(sheets)) {
            stop("sheet must be a valid worksheet name or index.")
        }
        sheet <- sheets[as.integer(sheet)]
    }
    if (!is.character(sheet) || length(sheet) != 1L || is.na(sheet) || !sheet %in% 
        sheets) {
        stop("Worksheet not found. Available sheets: ", paste(sheets, collapse = ", "))
    }
    x <- as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"))
    if (anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))) {
        stop("Worksheet headers must be non-empty and unique.")
    }
    core <- c("material", "age_hours", "frequency_hz", "permittivity", "dissipation_factor", 
        "impedance")
    allowed <- c(core, "concentration_pct", "fuel", "water", "contaminant_code", 
        "medium_code", "sample_id")
    if (!is.null(column_map)) {
        if (layout == "legacy") {
            stop("column_map cannot be combined with layout = 'legacy'.")
        }
        if (!is.character(column_map) || !length(column_map) || is.null(names(column_map)) || 
            anyNA(column_map) || anyNA(names(column_map)) || any(!nzchar(column_map)) || 
            any(!nzchar(names(column_map))) || anyDuplicated(names(column_map)) || 
            anyDuplicated(unname(column_map)) || !all(names(column_map) %in% allowed)) {
            stop("column_map must uniquely map standard names to source headers.")
        }
        if (!all(unname(column_map) %in% names(x))) {
            stop("Mapped source columns are missing.")
        }
        names(x)[match(unname(column_map), names(x))] <- names(column_map)
        if (anyDuplicated(names(x))) {
            stop("Column mapping creates duplicate column names.")
        }
    }
    standard_ready <- all(core %in% names(x))
    use_legacy <- layout == "legacy" || (layout == "auto" && is.null(column_map) && 
        !standard_ready && sheet %in% c("All extract_mean", "COded"))
    if (use_legacy) {
        legacy_names <- switch(sheet, `All extract_mean` = c("material", "concentration_pct", 
            "fuel", "contaminant_code", "water", "medium_code", "age_hours", "frequency_hz", 
            "permittivity", "dissipation_factor", "impedance"), COded = c("material", 
            "concentration_pct", "contaminant_code", "medium_code", "age_hours", 
            "frequency_hz", "permittivity", "dissipation_factor", "impedance"), stop("Legacy layout requires All extract_mean or COded."))
        if (ncol(x) < length(legacy_names)) {
            stop("Legacy worksheet has too few columns.")
        }
        x <- x[, seq_along(legacy_names), drop = FALSE]
        names(x) <- legacy_names
    }
    absent <- setdiff(core, names(x))
    if (length(absent)) {
        stop("Missing standard columns: ", paste(absent, collapse = ", "), ". Use column_map for different workbook headers.")
    }
    numeric_columns <- intersect(c("concentration_pct", "age_hours", "frequency_hz", 
        "permittivity", "dissipation_factor", "impedance"), names(x))
    for (nm in numeric_columns) {
        if (!is.numeric(x[[nm]])) {
            original_values <- x[[nm]]
            converted <- suppressWarnings(as.numeric(as.character(original_values)))
            if (any(is.na(converted) & !is.na(original_values))) {
                warning("Non-numeric entries in ", nm, call. = FALSE)
            }
            x[[nm]] <- converted
        }
    }
    x
}
