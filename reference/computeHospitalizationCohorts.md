# computeHospitalizationCohorts

Generates a cohort table containing hospitalizations and readmissions
from the \`visit_occurrence\` table. Overlapping or contiguous inpatient
visits are merged into a single episode. A readmission is defined as an
episode that starts within \`readmission_window\` days of the end of a
previous episode.

## Usage

``` r
computeHospitalizationCohorts(
  cdm,
  name,
  visitConceptIds = c(9201, 262),
  readmission_window = 30
)
```

## Arguments

- cdm:

  A CDM reference object.

- name:

  The name of the cohort table to be created in the database.

- visitConceptIds:

  A vector of integer concept IDs representing inpatient visits.
  Default: \`c(9201, 262)\` (Inpatient Visit, Emergency Room and
  Inpatient Visit).

- readmission_window:

  Integer indicating the maximum number of days between discharge and a
  subsequent admission to be considered a readmission. Default: 30.

## Value

A cohort table with two \`cohort_definition_id\`s: - 1:
Hospitalization - 2: Readmission
