# Execute Transform

Runs a transform, creating the output dataset and recording the run with
full lineage tracking.

## Usage

``` r
ont_execute_transform(
  transform_id,
  triggered_by = "manual",
  executed_by = NULL,
  con = NULL
)
```

## Arguments

- transform_id:

  The transform to execute.

- triggered_by:

  What triggered this execution.

- executed_by:

  User who executed.

- con:

  Optional DBI connection.

## Value

A list with run details.
