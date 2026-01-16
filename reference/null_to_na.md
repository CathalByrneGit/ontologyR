# Convert NULL to NA for DBI binding

DuckDB parameter binding requires all values to have length 1. This
helper converts NULL to NA_character\_ for text parameters.

## Usage

``` r
null_to_na(x)
```

## Arguments

- x:

  Value that might be NULL.

## Value

NA_character\_ if x is NULL, otherwise x.
