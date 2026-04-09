# getCodelistFromConceptSet

This function queries the \`concept_set\` and \`concept_set_item\`
tables to build a formal \`codelist\` object from the \`omopgenerics\`
package.

## Usage

``` r
getCodelistFromConceptSet(conceptSetId, con, cdmSchema)
```

## Arguments

- conceptSetId:

  The concept_set_id to query for.

- con:

  A DBI connection to the database.

- cdmSchema:

  The name of the schema where the OMOP tables reside.

## Value

A named \`codelist\` object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(DBI)
library(duckdb)
con <- dbConnect(duckdb::duckdb())
# ... setup concept tables ...
codes <- getCodelistFromConceptSet(123, con, "main")
} # }
```
