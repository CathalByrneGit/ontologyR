# Get Full Lineage Graph

Returns the full lineage graph for a dataset (both upstream and
downstream).

## Usage

``` r
ont_get_lineage_graph(dataset_id, depth = 10, con = NULL)
```

## Arguments

- dataset_id:

  The central dataset.

- depth:

  Maximum depth in each direction.

- con:

  Optional DBI connection.

## Value

A list with nodes (datasets) and edges (lineage connections).
