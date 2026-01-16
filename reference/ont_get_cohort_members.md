# Get Cohort Members

Retrieves the members of a cohort (evaluates SQL for dynamic cohorts).

## Usage

``` r
ont_get_cohort_members(cohort_id, con = NULL)
```

## Arguments

- cohort_id:

  Character. The cohort ID.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A character vector of object keys.
