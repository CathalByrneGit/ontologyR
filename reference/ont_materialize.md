# Materialize a Concept

Evaluates a concept and materializes the results to a persistent table,
creating a run record and lineage edges for full traceability.

## Usage

``` r
ont_materialize(
  concept_id,
  scope,
  version = NULL,
  output_table = NULL,
  filter_expr = NULL,
  include_all_columns = TRUE,
  triggered_by = "manual",
  executed_by = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  The concept to materialize.

- scope:

  The scope.

- version:

  Optional version (uses active version if NULL).

- output_table:

  Name for the output table. Defaults to concept_id + scope.

- filter_expr:

  Optional SQL filter expression.

- include_all_columns:

  If TRUE, includes all columns from source table.

- triggered_by:

  What triggered this materialization (e.g., "manual", "scheduled").

- executed_by:

  User who executed the materialization.

- con:

  Optional DBI connection.

## Value

A list with run details including run_id, dataset_id, and row_count.
