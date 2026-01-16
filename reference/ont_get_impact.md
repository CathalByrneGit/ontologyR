# Get Downstream Impact

Returns all datasets and transforms that would be affected if a dataset
changes. This is critical for change impact analysis.

## Usage

``` r
ont_get_impact(dataset_id, depth = 10, con = NULL)
```

## Arguments

- dataset_id:

  The dataset to analyze.

- depth:

  Maximum traversal depth.

- con:

  Optional DBI connection.

## Value

A list with affected datasets and transforms.
