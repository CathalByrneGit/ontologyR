# Compare Concept Versions

Evaluates two versions of a concept and shows where they disagree.

## Usage

``` r
ont_compare_versions(
  concept_id,
  scope,
  version_a,
  version_b,
  filter_expr = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to compare.

- scope:

  Character. The scope.

- version_a:

  Integer. First version.

- version_b:

  Integer. Second version.

- filter_expr:

  Character. Optional SQL WHERE clause.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with columns from the object table plus `value_a`, `value_b`,
and `agree`.
