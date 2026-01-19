# OmopHelpers

<!-- badges: start -->
[![R-CMD-check](https://github.com/iomedhealth/OmopHelpers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/iomedhealth/OmopHelpers/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->


`OmopHelpers` is an R package that provides a collection of utility functions for working with OHDSI data, particularly within the IOMED environment. It simplifies common tasks such as creating codelists, merging them, and visualizing data distributions.

## Installation

You can install OmopHelpers from GitHub using the `devtools` package:

```r
# install.packages("devtools")
devtools::install_github("iomedhealth/OmopHelpers")
```

## Usage

### Creating a Codelist from a Concept Set

The `getCodelistFromConceptSet` function allows you to create a `codelist` object directly from a `concept_set_id`.

```r
library(OmopHelpers)
library(DBI)

# Assume 'con' is a valid DBI connection to your OMOP CDM
# and 'cdmSchema' is the schema where your OMOP tables reside.

# Example: Create a codelist for concept set ID 123
my_codelist <- getCodelistFromConceptSet(
  conceptSetId = 123,
  con = con,
  cdmSchema = "your_cdm_schema"
)
```

### Merging Codelists

You can merge multiple `codelist` objects into a single one using `mergeCodelists`.

```r
# Assuming 'codelist1' and 'codelist2' are two existing codelist objects
merged_codes <- mergeCodelists(codelist1, codelist2, newName = "merged_codelist_name")
```

### Plotting Measurement Distributions

The `plotMeasurementDistribution` function generates a histogram or density plot for a given measurement.

```r
# Assume 'my_cohort_data' is a data frame or tibble
# containing patient data and measurements.

# Example: Create a histogram for a measurement starting with "hba1c"
plotMeasurementDistribution(
  data = my_cohort_data,
  variable = "hba1c",
  variableDisplay = "HbA1c",
  plotTitle = "Distribution of HbA1c at Baseline"
)

# Example: Create a density plot stratified by a 'treatment' column
plotMeasurementDistribution(
  data = my_cohort_data,
  variable = "cholesterol",
  variableDisplay = "Cholesterol Level",
  plotTitle = "Cholesterol Distribution by Treatment Group",
  plotType = "density",
  strata = "treatment"
)
```

## License

This package is licensed under the MIT License.
