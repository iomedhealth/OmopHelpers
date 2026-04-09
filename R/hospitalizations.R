# +-----------------------------------------------------------------+
# |                     CDM: visit_occurrence                       |
# +-----------------------------------------------------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# | 1. Extract & Impute                                             |
# |    - Filter by `visitConceptIds` (e.g., Inpatient/ER)           |
# |    - Select person_id, visit_start_date, visit_end_date         |
# |    - If visit_end_date is NA, impute with visit_start_date      |
# +-----------------------------------------------------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# |                         [raw_visits]                            |
# +-----------------------------------------------------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# | 2. Collapse Overlapping/Contiguous Episodes                     |
# |    - Group by subject_id, order by start_date                   |
# |    - Track `max_end_so_far` using cumulative max of end_dates   |
# |    - Flag `is_new_episode` if start_date > max_end_so_far + 1   |
# |    - Assign `episode_id` with cumulative sum of the flags       |
# |    - Group by subject_id & episode_id                           |
# |    - Summarise: min(start_date) and max(end_date)               |
# +-----------------------------------------------------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# |                      [collapsed_visits]                         |
# |             (Distinct, merged inpatient episodes)               |
# +-----------------------------------------------------------------+
#                                 |
#            +--------------------+--------------------+
#            |                                         |
#            v                                         v
# +-----------------------+                 +-----------------------+
# |   Hospitalizations    |                 | 3. Find Readmissions  |
# |  (cohort_id = 1)      |                 | - Group by subject_id |
# |                       |                 | - Order by start_date |
# |                       |                 | - Get `prev_end_date` |
# |                       |                 |   using lag()         |
# +-----------------------+                 +-----------------------+
#            |                                         |
#            |                                         v
#            |                              +-----------------------+
#            |                              | Filter valid lags:    |
#            |                              | - prev_end isn't NA   |
#            |                              | - (start - prev_end)  |
#            |                              |   <= window (e.g. 30) |
#            |                              | - Set cohort_id = 2   |
#            |                              +-----------------------+
#            |                                         |
#            |                                         v
#            |                              +-----------------------+
#            |                              |     Readmissions      |
#            |                              |    (cohort_id = 2)    |
#            |                              +-----------------------+
#            |                                         |
#            +--------------------+--------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# |                          UNION ALL                              |
# +-----------------------------------------------------------------+
#                                 |
#                                 v
# +-----------------------------------------------------------------+
# | 4. Format & Cleanup                                             |
# |    - Drop intermediate temporary tables created in the DB       |
# |    - Construct attrition and cohort_set metadata                |
# |    - Wrap via `omopgenerics::newCohortTable()`                  |
# |    - Return `final_cohort`                                      |
# +-----------------------------------------------------------------+
#
# 1. Contiguous Episodes: The is_new_episode logic (start > max_end_so_far + 1)
# ensures that visits separated by exactly 1 day (e.g., ending on Monday,
# starting on Tuesday) or entirely overlapping are merged into a single
# continuous hospitalization episode.
#
# 2. Readmissions: It uses window functions
# to look exactly 1 row back (dplyr::lag) in the patient's grouped and ordered
# history to check if the gap between the previous discharge and current
# admission falls strictly within the readmission_window parameter.

#' @title computeHospitalizationCohorts
#' @description
#' Generates a cohort table containing hospitalizations and readmissions from the
#' `visit_occurrence` table. Overlapping or contiguous inpatient visits are merged
#' into a single episode. A readmission is defined as an episode that starts
#' within `readmission_window` days of the end of a previous episode.
#'
#' @param cdm A CDM reference object.
#' @param name The name of the cohort table to be created in the database.
#' @param visitConceptIds A vector of integer concept IDs representing inpatient visits.
#' Default: `c(9201, 262)` (Inpatient Visit, Emergency Room and Inpatient Visit).
#' @param readmission_window Integer indicating the maximum number of days between
#' discharge and a subsequent admission to be considered a readmission. Default: 30.
#'
#' @return A cohort table with two `cohort_definition_id`s:
#' - 1: Hospitalization
#' - 2: Readmission
#' @export
computeHospitalizationCohorts <- function(cdm, name, visitConceptIds = c(9201, 262), readmission_window = 30) {
  # Validate inputs
  omopgenerics::assertCharacter(name, length = 1)
  omopgenerics::assertClass(cdm, "cdm_reference")
  visitConceptIds <- as.integer(visitConceptIds)
  readmission_window <- as.integer(readmission_window)

  tablePrefix <- omopgenerics::tmpPrefix()

  # Step 1: Extract hospitalizations
  raw_visits <- cdm$visit_occurrence |>
    dplyr::filter(.data$visit_concept_id %in% .env$visitConceptIds) |>
    dplyr::select(
      "person_id",
      "visit_start_date",
      "visit_end_date"
    ) |>
    dplyr::mutate(
      visit_end_date = dplyr::if_else(is.na(.data$visit_end_date), .data$visit_start_date, .data$visit_end_date)
    ) |>
    dplyr::compute(name = paste0(tablePrefix, "raw_visits"), temporary = FALSE)

  # Collapse overlapping/contiguous visits
  # First, sort and find boundaries
  collapsed_visits <- raw_visits |>
    dplyr::rename(
      subject_id = "person_id",
      cohort_start_date = "visit_start_date",
      cohort_end_date = "visit_end_date"
    ) |>
    dplyr::group_by(.data$subject_id) |>
    dbplyr::window_order(.data$cohort_start_date) |>
    dplyr::mutate(
      max_end_so_far = cummax(.data$cohort_end_date),
      is_new_episode = dplyr::if_else(
        dplyr::row_number() == 1L | .data$cohort_start_date > dplyr::lag(.data$max_end_so_far) + as.integer(1),
        1L,
        0L
      )
    ) |>
    dplyr::mutate(episode_id = cumsum(.data$is_new_episode)) |>
    dplyr::group_by(.data$subject_id, .data$episode_id) |>
    dplyr::summarise(
      cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
      cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select("subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::compute(name = paste0(tablePrefix, "collapsed_visits"), temporary = FALSE)

  # Step 2: Identify readmissions
  # We look at the previous episode's end date for the same subject
  episodes_with_lag <- collapsed_visits |>
    dplyr::group_by(.data$subject_id) |>
    dbplyr::window_order(.data$cohort_start_date) |>
    dplyr::mutate(
      prev_end_date = dplyr::lag(.data$cohort_end_date)
    ) |>
    dplyr::ungroup() |>
    dplyr::compute(name = paste0(tablePrefix, "episodes_with_lag"), temporary = FALSE)

  # Readmissions are those where (cohort_start_date - prev_end_date) <= readmission_window
  # and prev_end_date is not null

  readmissions <- episodes_with_lag |>
    dplyr::filter(!is.na(.data$prev_end_date)) |>
    dplyr::filter(.data$cohort_start_date - .data$prev_end_date <= .env$readmission_window) |>
    dplyr::mutate(cohort_definition_id = 2L) |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")

  hospitalizations <- collapsed_visits |>
    dplyr::mutate(cohort_definition_id = 1L) |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")

  final_cohort <- hospitalizations |>
    dplyr::union_all(readmissions) |>
    dplyr::compute(name = name, temporary = FALSE)

  # Attrition records and codelist empty (or we can build a basic one)
  attrition <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L),
    number_records = c(NA_integer_, NA_integer_),
    number_subjects = c(NA_integer_, NA_integer_),
    reason_id = c(1L, 1L),
    reason = c("Initial Qualifying Events", "Initial Qualifying Events"),
    excluded_records = c(0L, 0L),
    excluded_subjects = c(0L, 0L)
  )

  # Create an empty codelist
  empty_codelist <- dplyr::tibble(
    cohort_definition_id = integer(),
    concept_id = integer(),
    type = character()
  )

  cohort_set <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L),
    cohort_name = c("hospitalization", "readmission")
  )

  # Clean up temp tables
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  # Build OmopGenerics Cohort
  final_cohort <- final_cohort |>
    omopgenerics::newCohortTable(
      cohortSetRef = cohort_set,
      cohortAttritionRef = NULL, # Will be computed by omopgenerics if we omit it and use softValidation? Better to just construct it
      .softValidation = TRUE
    )

  return(final_cohort)
}
