# Create a Drift Event

Records a drift detection event for tracking and governance.

## Usage

``` r
ont_create_drift_event(
  concept_id,
  scope,
  version,
  detection_type,
  disagreement_rate,
  window_days,
  audit_count,
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

- detection_type:

  Character. How drift was detected: "threshold", "trend", or "manual".

- disagreement_rate:

  Numeric. The disagreement rate at detection.

- window_days:

  Integer. Window size used for detection.

- audit_count:

  Integer. Number of audits in the window.

- con:

  A DBI connection.

## Value

The drift_id of the created event.
