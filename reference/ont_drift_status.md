# Get Drift Status Summary

Returns a summary of drift status across all concepts or a specific
concept.

## Usage

``` r
ont_drift_status(concept_id = NULL, con = NULL)
```

## Arguments

- concept_id:

  Character. Optional filter by concept.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble summarizing drift status by concept/scope/version.
