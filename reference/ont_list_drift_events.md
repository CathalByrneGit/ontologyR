# List Drift Events

Returns drift events, optionally filtered by status.

## Usage

``` r
ont_list_drift_events(concept_id = NULL, status = NULL, con = NULL)
```

## Arguments

- concept_id:

  Character. Optional filter by concept.

- status:

  Character. Optional filter by status: "open", "investigating",
  "resolved", "accepted".

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of drift events.
