# getAllConceptSets

Retrieves all concept sets from the database and returns them as a named
list of codelists.

## Usage

``` r
getAllConceptSets(con, cdmSchema)
```

## Arguments

- con:

  A DBI connection to the database.

- cdmSchema:

  The name of the schema where the OMOP tables reside.

## Value

A named list of \`codelist\` objects.
