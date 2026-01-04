# List Dashboard Dependencies

Returns dashboards that depend on a concept version.

## Usage

``` r
ont_list_dashboards(
  concept_id = NULL,
  scope = NULL,
  version = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. Optional filter by concept.

- scope:

  Character. Optional filter by scope.

- version:

  Integer. Optional filter by version.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of dashboard dependencies.
