library(testthat)
library(dplyr)
library(omopgenerics)
library(duckdb)
library(DBI)
library(CDMConnector)

test_that("computeHospitalizationCohorts handles inconsistent dates and overlaps", {
  con <- dbConnect(duckdb(), ":memory:")

  # Create mock data
  person <- tibble(
    person_id = 1:2,
    gender_concept_id = 0,
    year_of_birth = 1990,
    month_of_birth = 1,
    day_of_birth = 1,
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )

  visit_occurrence <- tibble(
    visit_occurrence_id = 1:6,
    person_id = c(1, 1, 1, 2, 2, 2),
    visit_concept_id = 9201,
    visit_start_date = as.Date(c(
      "2020-01-01", "2020-01-05", "2020-01-20", # Person 1: overlap, then gap
      "2020-02-01", "2020-02-10", "2020-03-01" # Person 2: start > end, NA end, normal
    )),
    visit_end_date = as.Date(c(
      "2020-01-07", "2020-01-10", "2020-01-25",
      "2019-01-01", NA, "2020-03-05"
    )),
    visit_type_concept_id = 0
  )

  observation_period <- tibble(
    observation_period_id = 1:2,
    person_id = 1:2,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2030-01-01"),
    period_type_concept_id = 0
  )

  dbWriteTable(con, "person", person)
  dbWriteTable(con, "visit_occurrence", visit_occurrence)
  dbWriteTable(con, "observation_period", observation_period)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  # Run function
  res <- computeHospitalizationCohorts(cdm, name = "hosp_cohort", readmission_window = 30)

  cohort_data <- res |> collect()

  # Check Person 1 (Overlapping visits 1 & 2 merged into one)
  # Visit 1: Jan 1-7, Visit 2: Jan 5-10 -> Merge: Jan 1-10
  # Visit 3: Jan 20-25 -> Separate
  p1_hosp <- cohort_data |> filter(subject_id == 1, cohort_definition_id == 1)
  expect_equal(nrow(p1_hosp), 2)
  expect_true(any(p1_hosp$cohort_start_date == as.Date("2020-01-01") & p1_hosp$cohort_end_date == as.Date("2020-01-10")))
  expect_true(any(p1_hosp$cohort_start_date == as.Date("2020-01-20") & p1_hosp$cohort_end_date == as.Date("2020-01-25")))

  # Check Person 2 (start > end, NA end)
  # Visit 4: Feb 1, end 2019 -> Fixed to Feb 1-Feb 1
  # Visit 5: Feb 10, end NA -> Fixed to Feb 10-Feb 10
  # Visit 6: Mar 1-5
  p2_hosp <- cohort_data |> filter(subject_id == 2, cohort_definition_id == 1)
  expect_equal(nrow(p2_hosp), 3)
  expect_true(all(p2_hosp$cohort_start_date <= p2_hosp$cohort_end_date))

  # Check Readmissions (cohort_id = 2)
  # Person 1: Jan 1-10 and Jan 20-25. Gap is 10 days. Readmission!
  p1_readm <- cohort_data |> filter(subject_id == 1, cohort_definition_id == 2)
  expect_equal(nrow(p1_readm), 1)
  expect_equal(p1_readm$cohort_start_date, as.Date("2020-01-20"))

  dbDisconnect(con, shutdown = TRUE)
})
