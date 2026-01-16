# Get Upstream Lineage

Returns all datasets that are upstream (inputs) of a given dataset.

## Usage

``` r
ont_get_upstream(dataset_id, depth = 10, con = NULL)
```

## Arguments

- dataset_id:

  The dataset to trace upstream from.

- depth:

  Maximum depth to traverse (default 10).

- con:

  Optional DBI connection.

## Value

A tibble of upstream datasets with their lineage edges.
