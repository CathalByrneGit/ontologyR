# Get Downstream Lineage

Returns all datasets that are downstream (outputs) of a given dataset.

## Usage

``` r
ont_get_downstream(dataset_id, depth = 10, con = NULL)
```

## Arguments

- dataset_id:

  The dataset to trace downstream from.

- depth:

  Maximum depth to traverse (default 10).

- con:

  Optional DBI connection.

## Value

A tibble of downstream datasets with their lineage edges.
