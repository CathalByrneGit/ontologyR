# Update Dataset Row Count

Updates the row count for a dataset by querying its physical table.

## Usage

``` r
ont_update_dataset_count(dataset_id, con = NULL)
```

## Arguments

- dataset_id:

  The dataset identifier.

- con:

  Optional DBI connection.

## Value

The new row count (invisibly).
