# List Observations

Retrieves observation records for a concept.

## Usage

``` r
ont_list_observations(
  concept_id = NULL,
  scope = NULL,
  version = NULL,
  from = NULL,
  to = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. Filter by concept.

- scope:

  Character. Filter by scope.

- version:

  Integer. Filter by version.

- from:

  POSIXct or Date. Start of date range.

- to:

  POSIXct or Date. End of date range.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of observation records.
