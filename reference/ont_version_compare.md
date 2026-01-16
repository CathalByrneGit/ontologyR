# Version Comparison

Compares how different versions of a concept evaluate the same objects.
Useful for understanding the impact of definition changes.

## Usage

``` r
ont_version_compare(
  concept_id,
  scope,
  versions = NULL,
  filter_expr = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to analyze.

- scope:

  Character. The scope.

- versions:

  Integer vector. Versions to compare (default: all).

- filter_expr:

  Character. Optional SQL WHERE clause.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with comparison results including agreement matrix.
