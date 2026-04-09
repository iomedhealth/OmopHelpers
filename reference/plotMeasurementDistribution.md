# plotMeasurementDistribution (Fully Configurable)

Plots a histogram or density plot for a measurement variable, with full
support for stratification in both plot types. It selects the earliest
measurement for each patient as the baseline.

## Usage

``` r
plotMeasurementDistribution(
  data,
  variable,
  variableDisplay,
  plotTitle,
  plotType = "histogram",
  strata = NULL,
  bins = 30
)
```

## Arguments

- data:

  The input cohort table (a tibble or data frame).

- variable:

  A string that the measurement column name starts with.

- variableDisplay:

  A user-friendly name for the x-axis label.

- plotTitle:

  The title for the plot.

- plotType:

  The type of plot to generate: "histogram" or "density". Defaults to
  "histogram".

- strata:

  An optional character string with the name of the column to stratify
  by.

- bins:

  An integer for the number of histogram bins. Defaults to 30.

## Value

A ggplot object.
