#' @import magrittr
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
NULL

#' @title getCodelistFromConceptSet
#' @description
#' This function queries the `concept_set` and `concept_set_item` tables
#' to build a formal `codelist` object from the `omopgenerics` package.
#' The codelist is automatically named using the `concept_set_name` from the
#' `concept_set` table.
#'
#' @param conceptSetId The concept_set_id to query for.
#' @param con A DBI connection to the database.
#' @param cdmSchema The name of the schema where the OMOP tables reside.
#'
#' @return A named `codelist` object.
#' @export
getCodelistFromConceptSet <- function(conceptSetId, con, cdmSchema) {
  # Point to the required vocabulary tables in the database
  concept_set_tbl <- dplyr::tbl(con, dbplyr::in_schema(cdmSchema, "concept_set"))
  concept_set_item_tbl <- dplyr::tbl(con, dbplyr::in_schema(cdmSchema, "concept_set_item"))
  
  # Retrieve the name of the concept set to use as the codelist name
  codelistName <- concept_set_tbl |>
    dplyr::filter(.data$concept_set_id == .env$conceptSetId) |>
    dplyr::pull("concept_set_name") |>
    unique()
  
  # Error handling: check if the concept set ID was found
  if (length(codelistName) == 0) {
    stop(glue::glue("No concept set found for concept_set_id: {conceptSetId}"))
  }
  # Warning if multiple names exist for the same ID
  if (length(codelistName) > 1) {
    warning(glue::glue("Multiple names found for concept_set_id: {conceptSetId}. Using the first one: '{codelistName[1]}'"))
    codelistName <- codelistName[1]
  }
  
  # Retrieve all unique concept IDs associated with the concept set ID
  concept_ids <- concept_set_item_tbl |>
    dplyr::filter(.data$concept_set_id == .env$conceptSetId) |>
    dplyr::pull("concept_id") |>
    unique()
  
  # Create a named list structure required by newCodelist
  codelist <- list(concept_ids) |>
    magrittr::set_names(codelistName)
  
  # Return the formal, validated codelist object
  return(omopgenerics::newCodelist(codelist))
}

#' @title mergeCodelists
#' @description
#' Merges multiple `codelist` objects from the `omopgenerics` package into a
#' single new `codelist`. It combines all concept IDs from the input lists and
#' removes any duplicates.
#'
#' @param ... An arbitrary number of `codelist` objects to be merged.
#' @param newName A character string specifying the name for the new merged codelist.
#'
#' @return A single, merged `codelist` object named according to the `newName` parameter.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming bb_codes and ccb_codes are valid codelist objects
#'   first_line_therapies <- mergeCodelists(bb_codes, ccb_codes, newName = "first_line_hcm_drugs")
#' }
mergeCodelists <- function(..., newName) {
  # Capture all codelist objects passed via the '...' into a single list
  codelists_to_merge <- list(...)
  
  # --- Input Validation ---
  if (length(codelists_to_merge) == 0) {
    stop("No codelist objects were provided to merge.")
  }
  if (missing(newName) || !is.character(newName) || length(newName) != 1) {
    stop("A single, valid character string must be provided for the 'newName' argument.")
  }
  if (!all(sapply(codelists_to_merge, inherits, "codelist"))) {
    stop("All objects passed to '...' must be of the class 'codelist'.")
  }
  
  # --- Merging Logic ---
  # 1. Use lapply to iterate through each codelist object.
  # 2. Extract the vector of concept IDs from each (they are the first element of each object).
  # 3. `unlist()` collapses the list of vectors into a single large vector.
  all_concept_ids <- unlist(lapply(codelists_to_merge, function(cl) cl[[1]]))
  
  # 4. Find the unique concept IDs to remove any overlaps between the lists.
  unique_concept_ids <- unique(all_concept_ids)
  
  # 5. Create the required named list structure for the new codelist constructor.
  merged_list_structure <- list(unique_concept_ids) |>
    magrittr::set_names(newName)
  
  # 6. Return the formal, validated codelist object.
  return(omopgenerics::newCodelist(merged_list_structure))
}

#' @title plotMeasurementDistribution (Fully Configurable)
#' @description
#' Plots a histogram or density plot for a measurement variable, with full
#' support for stratification in both plot types. It selects the earliest
#' measurement for each patient as the baseline.
#'
#' @param data The input cohort table (a tibble or data frame).
#' @param variable A string that the measurement column name starts with.
#' @param variableDisplay A user-friendly name for the x-axis label.
#' @param plotTitle The title for the plot.
#' @param plotType The type of plot to generate: "histogram" or "density". Defaults to "histogram".
#' @param strata An optional character string with the name of the column to stratify by.
#' @param bins An integer for the number of histogram bins. Defaults to 30.
#'
#' @return A ggplot object.
#' @export
plotMeasurementDistribution <- function(data, variable, variableDisplay, plotTitle,
                                        plotType = "histogram", strata = NULL, bins = 30) {
  
  # --- Input Validation ---
  if (!plotType %in% c("histogram", "density")) {
    stop("Error: plotType must be either 'histogram' or 'density'.")
  }
  
  # --- 1. Prepare the data ---
  cols_to_select <- c("subject_id", "cohort_start_date")
  if (!is.null(strata)) {
    if (!strata %in% colnames(data)) {
      stop(glue::glue("Error: Strata column '{strata}' not found in the data."))
    }
    cols_to_select <- c(cols_to_select, strata)
  }
  
  measurement_cols <- colnames(data)[startsWith(colnames(data), variable)]
  if (length(measurement_cols) == 0) {
    stop(glue::glue("Error: No column found starting with the prefix '{variable}'."))
  }
  
  plot_data <- data %>%
    select(all_of(c(cols_to_select, measurement_cols))) %>%
    pivot_longer(
      cols = all_of(measurement_cols),
      names_to = "measurement_type",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    group_by(subject_id) %>%
    slice_min(order_by = cohort_start_date, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # --- 2. Build the plot based on user choices ---
  # Start with the base plot and aesthetics
  if (is.null(strata)) {
    plot <- ggplot(plot_data, aes(x = value))
  } else {
    plot <- ggplot(plot_data, aes(x = value, fill = .data[[strata]], color = .data[[strata]]))
  }
  
  # Add the chosen geometry layer
  if (plotType == "histogram") {
    plot <- plot + geom_histogram(bins = bins, alpha = 0.6, position = "identity")
  } else { # plotType == "density"
    plot <- plot + geom_density(alpha = 0.6)
  }
  
  # --- 3. Add labels and theme ---
  plot <- plot +
    labs(
      title = plotTitle,
      subtitle = if (!is.null(strata)) paste("Stratified by", strata) else NULL,
      x = variableDisplay,
      y = if (plotType == "histogram") "Number of Patients" else "Density",
      fill = if (!is.null(strata)) strata else NULL,
      color = if (!is.null(strata)) strata else NULL
    ) +
    theme_minimal(base_size = 14)
  
  # For a single histogram, we don't need a legend and can specify a fill
  if (is.null(strata) && plotType == "histogram") {
    plot <- plot + scale_fill_manual(values = c("#1f77b4"), guide = "none") +
      scale_color_manual(values = c("#1f77b4"), guide = "none")
  }
  
  
  return(plot)
}

#' @title clean_name
#' @description
#' A helper function to remove trailing numeric codes from a string. It looks for
#' a dash followed by a space and one or more digits at the end of the string.
#'
#' @param name_string A character string from which to remove the code.
#'
#' @return The cleaned character string without the trailing code.
#' @export
clean_name <- function(name_string) {
  # The updated regular expression `\\s*–\\s*\\d+$` is more specific:
  # \\s*: Matches zero or more whitespace characters.
  # –: Matches the specific dash character.
  # \\s*: Matches zero or more whitespace characters.
  # \\d+: Matches one or more digits.
  # $: Anchors the pattern to the end of the string, ensuring only the
  #    last instance of the pattern is removed.
  
  cleaned_string <- gsub("\\s*[–-]\\s*\\d+$", "", name_string, fixed = FALSE)
  return(cleaned_string)
}

#' @title process_codelists
#' @description
#' Processes a vector of codelist objects. It iterates through the vector, extracts the internal,
#' descriptive name from each list's first element, removes any trailing numeric codes,
#' and then assigns the first integer vector to the new, cleaned name.
#'
#' @param codelist_vector A list or vector containing the codelist objects, typically
#' created by a function like `getCodelistFromConceptSet()`.
#'
#' @return A new list with cleaned, descriptive names and containing only the first integer
#' vector of each original codelist.
#' @export
process_codelists <- function(codelist_vector) {
  # Check if the input is a list or vector
  if (!is.list(codelist_vector)) {
    stop("Input must be a list or a vector.")
  }
  
  # Initialize an empty list to store the final output
  processed_list <- list()
  
  # Iterate through the input vector to clean names and extract data
  for (name in names(codelist_vector)) {
    # Get the name of the first element in the item
    original_name <- name
    
    # Clean the name using the 'clean_name' helper function
    cleaned_name <- clean_name(original_name)
    
    # Assign the first integer vector to the new, cleaned name
    processed_list[[cleaned_name]] <- codelist_vector[[name]]
  }
  
  return(processed_list)
}

# PATCH the table person

# 1. Define the corrected version of the internal function
fixed_summariseNumeric2 <- function(x, variable, den) {
  x |>
    dplyr::rename(variable_level = !!variable) |>
    dplyr::summarise(
      # This uses if_else() to generate database-compatible SQL
      count_missing = sum(if_else(is.na(.data$variable_level), 1L, 0L), na.rm = TRUE),
      count_0 = sum(if_else(.data$variable_level == 0, 1L, 0L), na.rm = TRUE),
      distinct_values = as.integer(dplyr::n_distinct(.data$variable_level))
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      count_missing = dplyr::coalesce(as.integer(.data$count_missing), 0L),
      count_0 = dplyr::coalesce(as.integer(.data$count_0), 0L),
      distinct_values = dplyr::coalesce(.data$distinct_values, 0L),
      percentage_missing = 100 * as.numeric(.data$count_missing) / .env$den,
      percentage_0 =  100 * as.numeric(.data$count_0) / .env$den
    )
}

# 2. Programmatically overwrite the internal function in the package's namespace
assignInNamespace(
  x = "summariseNumeric2",
  value = fixed_summariseNumeric2,
  ns = "OmopSketch"
)
