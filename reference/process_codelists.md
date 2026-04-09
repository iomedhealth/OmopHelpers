# process_codelists

Processes a vector of codelist objects. It iterates through the vector,
extracts the internal, descriptive name from each list's first element,
removes any trailing numeric codes, and then assigns the first integer
vector to the new, cleaned name.

## Usage

``` r
process_codelists(codelist_vector)
```

## Arguments

- codelist_vector:

  A list or vector containing the codelist objects, typically created by
  a function like \`getCodelistFromConceptSet()\`.

## Value

A new list with cleaned, descriptive names and containing only the first
integer vector of each original codelist.
