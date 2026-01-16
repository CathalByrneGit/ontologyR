# Check for Drift

Evaluates whether a concept version shows signs of drift based on audit
data. Drift is detected when the disagreement rate exceeds a threshold,
either in absolute terms or as a trend.

## Usage

``` r
ont_check_drift(
  concept_id,
  scope,
  version = NULL,
  threshold = 0.15,
  min_audits = 10,
  window_days = 30,
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

  Numeric. Disagreement rate threshold (0-1). Default 0.15.

- min_audits:

  Integer. Minimum audits required before checking. Default 10.

- window_days:

  Integer. Days to look back for recent audits. Default 30.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with drift status and details.
