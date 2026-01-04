# Compute Audit Summary Statistics

Computes agreement/disagreement statistics for audits of a concept
version.

## Usage

``` r
ont_audit_summary(
  concept_id,
  scope,
  version,
  from = NULL,
  to = NULL,
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

- from:

  Date or POSIXct. Start of date range.

- to:

  Date or POSIXct. End of date range.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with audit statistics.
