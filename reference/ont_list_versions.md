# List Concept Versions

Returns versions for a concept, optionally filtered by scope and status.

## Usage

``` r
ont_list_versions(concept_id, scope = NULL, status = NULL, con = NULL)
```

## Arguments

- concept_id:

  Character. The concept to list versions for.

- scope:

  Character. Optional filter by scope.

- status:

  Character. Optional filter by status.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of concept versions.
