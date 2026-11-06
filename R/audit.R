#' Audit dielectric data without inventing physical sample IDs
#' @param data Output from read_dielectric().
#' @return List containing summary, missingness, ranges, duplicated keys and warnings.
#' @export
audit_dielectric <- function(data) {
  required <- c('material','age_hours','frequency_hz','permittivity','dissipation_factor','impedance')
  absent <- setdiff(required, names(data))
  if (length(absent)) stop('Missing columns: ', paste(absent, collapse=', '))
  key <- data[c('material','age_hours','frequency_hz')]
  duplicated_key <- duplicated(key) | duplicated(key, fromLast=TRUE)
  counts <- sort(table(data$material), decreasing=TRUE)
  ranges <- lapply(intersect(c('age_hours','frequency_hz','concentration_pct',
                               'permittivity','dissipation_factor','impedance'), names(data)),
                   function(nm) {
                     z <- data[[nm]]
                     if (all(is.na(z))) return(c(min=NA_real_,max=NA_real_))
                     c(min=min(z,na.rm=TRUE),max=max(z,na.rm=TRUE))
                   })
  names(ranges) <- intersect(c('age_hours','frequency_hz','concentration_pct',
                               'permittivity','dissipation_factor','impedance'), names(data))
  flags <- c(
    if (any(data$age_hours > 168, na.rm=TRUE)) 'Some ages exceed stated 168-hour study window',
    if (anyNA(data$impedance)) 'Impedance has missing observations',
    if (any(duplicated_key)) 'Repeated material-age-frequency keys detected',
    'Material labels are NOT confirmed physical sample identifiers',
    'The workbook sheet name suggests extracted/averaged observations; raw replicates unverified',
    'Do not randomly split rows for independent-sample model validation'
  )
  list(summary=data.frame(rows=nrow(data), materials=length(unique(data$material)),
                          material_age_groups=nrow(unique(data[c('material','age_hours')])),
                          duplicated_key_rows=sum(duplicated_key)),
       missingness=data.frame(variable=names(data), missing=vapply(data,function(z) sum(is.na(z)),integer(1))),
       ranges=ranges, material_counts=counts, warnings=flags)
}
