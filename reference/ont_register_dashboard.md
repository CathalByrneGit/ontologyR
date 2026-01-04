# Register Dashboard Dependency

Records that a dashboard or report depends on a specific concept
version. This enables impact analysis when definitions change.

## Usage

``` r
ont_register_dashboard(
  dashboard_id,
  concept_id,
  scope,
  version,
  dashboard_name = NULL,
  registered_by = NULL,
  con = NULL
)
```

## Arguments

- dashboard_id:

  Character. Unique identifier for the dashboard.

- concept_id:

  Character. The concept being used.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- dashboard_name:

  Character. Optional human-readable name.

- registered_by:

  Character. Optional registrant identifier.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE`.
