# Compute Rolling Disagreement Rate

Computes disagreement rate over a rolling window, useful for detecting
trends in drift over time.

## Usage

``` r
ont_rolling_disagreement(
  concept_id,
  scope,
  version,
  window_days = 7,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- window_days:

  Integer. Size of rolling window in days.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with date and rolling_disagreement_rate columns.
