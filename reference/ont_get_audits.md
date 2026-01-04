# Get Audit History

Retrieves audit records for a concept version, optionally filtered by
date range.

## Usage

``` r
ont_get_audits(
  concept_id,
  scope = NULL,
  version = NULL,
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

  Integer. The version. If `NULL`, returns all versions.

- from:

  Date or POSIXct. Start of date range.

- to:

  Date or POSIXct. End of date range.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of audit records.
