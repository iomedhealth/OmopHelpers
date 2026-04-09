# clean_name

A helper function to remove trailing numeric codes from a string. It
looks for a dash followed by a space and one or more digits at the end
of the string.

## Usage

``` r
clean_name(name_string)
```

## Arguments

- name_string:

  A character string from which to remove the code.

## Value

The cleaned character string without the trailing code.
