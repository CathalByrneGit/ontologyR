# Get Run History for Dataset

Returns all runs that produced or used a dataset.

## Usage

``` r
ont_get_dataset_runs(
  dataset_id,
  role = c("all", "producer", "consumer"),
  limit = 100,
  con = NULL
)
```

## Arguments

- dataset_id:

  The dataset identifier.

- role:

  "producer" for runs that created this dataset, "consumer" for runs
  that used it.

- limit:

  Maximum runs to return.

- con:

  Optional DBI connection.

## Value

A tibble of runs.
