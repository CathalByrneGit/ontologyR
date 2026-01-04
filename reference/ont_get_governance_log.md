# Get Governance Log

Retrieves governance action history.

## Usage

``` r
ont_get_governance_log(
  concept_id = NULL,
  action_type = NULL,
  actor = NULL,
  from = NULL,
  to = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. Optional filter by concept.

- action_type:

  Character. Optional filter by action type.

- actor:

  Character. Optional filter by actor.

- from:

  POSIXct or Date. Start of date range.

- to:

  POSIXct or Date. End of date range.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of governance log entries.
