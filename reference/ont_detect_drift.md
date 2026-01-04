# Run Drift Detection with Alerting

Checks for drift and optionally creates a drift event record if
detected. This is the main entry point for automated drift monitoring.

## Usage

``` r
ont_detect_drift(
  concept_id,
  scope,
  version = NULL,
  threshold = 0.15,
  min_audits = 10,
  window_days = 30,
  create_event = TRUE,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to check.

- scope:

  Character. The scope.

- version:

  Integer. The version. If `NULL`, uses active version.

- threshold:

  Numeric. Disagreement rate threshold. Default 0.15.

- min_audits:

  Integer. Minimum audits required. Default 10.

- window_days:

  Integer. Days to look back. Default 30.

- create_event:

  Logical. If `TRUE` and drift detected, create a drift event record.
  Default `TRUE`.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with drift check results and event_id if created.
