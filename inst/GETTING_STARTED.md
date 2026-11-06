# Getting Started with DielectricWaterR

DielectricWaterR provides descriptive analysis and
data-quality auditing for dielectric measurements.

This tutorial uses entirely synthetic data.
The values are illustrative and are not experimental results.

## 1. Load the package

```r
library(DielectricWaterR)
```

## 2. Generate synthetic measurements

```r
example_data <- expand.grid(
  fuel = c('Petrol', 'Diesel'),
  water = c('d', 'w'),
  age_hours = c(0, 24),
  frequency_hz = c(10, 100, 1000, 10000, 100000, 1000000),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

example_data$concentration_pct <- 5

example_data$material <- paste(
  ifelse(example_data$water == 'd',
         'Distilled water', 'Well water'),
  example_data$fuel, '5%'
)

example_data$permittivity <-
  80 - 4 * log10(example_data$frequency_hz) -
  0.1 * example_data$age_hours -
  ifelse(example_data$fuel == 'Diesel', 3, 0) -
  ifelse(example_data$water == 'w', 2, 0)

example_data$dissipation_factor <-
  0.02 + 0.005 * log10(example_data$frequency_hz) +
  0.0005 * example_data$age_hours

example_data$impedance <-
  10000 / sqrt(example_data$frequency_hz) +
  ifelse(example_data$fuel == 'Diesel', 50, 0)

example_data$sample_id <- paste(
  example_data$fuel, example_data$water,
  example_data$age_hours, sep = '_'
)

head(example_data)
```

These formulas are invented for teaching.
They are not validated physical models.
The sample IDs are illustrative, not verified experimental units.

## 3. Audit the data

```r
audit <- audit_dielectric(example_data)
print(audit$summary)
```

## 4. Plot frequency-response curves

```r
p <- plot_fingerprints(
  example_data,
  response = 'permittivity',
  title = 'Synthetic Dielectric Frequency Responses',
  subtitle = 'Illustrative data only; not experimental observations',
  show_legend = TRUE
)

print(p)
```

Colours distinguish materials and line types distinguish
recorded ageing times. These curves are descriptive.

## 5. Important scientific safeguards

- Rows are not automatically independent physical samples.
- Repeated frequency measurements may belong to the same sample.
- Synthetic trends do not establish real-world fuel effects.
- Statistical inference requires a defensible experimental design.
- Confidential source workbooks are not distributed with the package.

## 6. Explore additional functions

```r
help(package = 'DielectricWaterR')
```
