# List Datasets

Returns a list of all registered datasets.

## Usage

``` r
ont_list_datasets(dataset_type = NULL, object_type = NULL, con = NULL)
```

## Arguments

- dataset_type:

  Optional filter by dataset type.

- object_type:

  Optional filter by object type.

- con:

  Optional DBI connection.

## Value

A tibble of datasets.
