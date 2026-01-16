# List Runs

Returns a list of materialization/transform runs.

## Usage

``` r
ont_list_runs(concept_id = NULL, status = NULL, limit = 100, con = NULL)
```

## Arguments

- concept_id:

  Optional filter by concept.

- status:

  Optional filter by status ("running", "completed", "failed").

- limit:

  Maximum number of runs to return.

- con:

  Optional DBI connection.

## Value

A tibble of runs.
