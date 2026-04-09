# mergeCodelists

Merges multiple \`codelist\` objects from the \`omopgenerics\` package
into a single new \`codelist\`. It combines all concept IDs from the
input lists and removes any duplicates.

## Usage

``` r
mergeCodelists(..., newName)
```

## Arguments

- ...:

  An arbitrary number of \`codelist\` objects to be merged.

- newName:

  A character string specifying the name for the new merged codelist.

## Value

A single, merged \`codelist\` object named according to the \`newName\`
parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming bb_codes and ccb_codes are valid codelist objects
first_line_therapies <- mergeCodelists(bb_codes, ccb_codes, newName = "first_line_hcm_drugs")
} # }
```
