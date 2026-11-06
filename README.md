# DielectricWaterR

An R package for auditing, exploring, and comparing
dielectric measurements of fuel-contaminated water.

## Overview

DielectricWaterR supports reproducible analysis of
frequency-dependent dielectric measurements across
fuel types, water media, concentrations, and ageing times.

## Main features

- Import dielectric measurement data.
- Audit data quality and consistency.
- Visualise dielectric fingerprints.
- Compare dielectric fingerprints.
- Analyse frequency-response measurements.
- Audit observations excluded from frequency analysis.
- Compare matched frequencies across ageing times.

## Installation

Install from a local package directory:

```r
install.packages(
  '/path/to/DielectricWaterR',
  repos = NULL,
  type = 'source'
)
```

## Example

```r
library(DielectricWaterR)

# Supply your own authorised Excel workbook.
# The original experimental dataset is not distributed.
data_path <- "path/to/your_authorised_data.xlsx"

data <- read_dielectric(data_path)
audit <- audit_dielectric(data)
```

## Scientific limitations

The package currently provides descriptive analysis.
Matched-frequency comparisons do not establish
causal effects or independent experimental replication.
The package does not provide validated contaminant
prediction or contaminant identification.

## Version

0.2.0 — Initial research prototype.

## Authors

Muhammad Zahir Khan (maintainer) and Luqman Abidoye

## Flexible Excel import

Standard headers or explicit mappings can be used on any worksheet.
Frequencies must already be in Hz and ages in hours.

```r
x <- read_dielectric('my_data.xlsx', sheet = 'Measurements',
                     layout = 'standard')
x <- read_dielectric('my_data.xlsx', sheet = 'Measurements',
                     column_map = c(frequency_hz = 'Frequency',
                                    permittivity = 'Epsilon'))
```

Public tests use an entirely synthetic Excel workbook.
The private experimental workbook is not distributed or required.
