# Trend Analysis

Analyzes how a concept's prevalence changes over time using observation
data.

## Usage

``` r
ont_trend_analysis(
  concept_id,
  scope,
  version = NULL,
  from = NULL,
  to = NULL,
  granularity = "day",
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to analyze.

- scope:

  Character. The scope.

- version:

  Integer. The version. If `NULL`, analyzes all versions.

- from:

  POSIXct or Date. Start of analysis period.

- to:

  POSIXct or Date. End of analysis period.

- granularity:

  Character. Time granularity: "day", "week", "month".

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with time period, observation counts, and prevalence
statistics.
