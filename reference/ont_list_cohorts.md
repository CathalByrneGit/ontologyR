# List Cohorts

Returns all defined cohorts.

## Usage

``` r
ont_list_cohorts(object_type = NULL, con = NULL)
```

## Arguments

- object_type:

  Character. Optional filter by object type.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of cohort definitions.
