# Get Transform Inputs

Returns all input datasets for a transform.

## Usage

``` r
ont_get_transform_inputs(transform_id, con = NULL)
```

## Arguments

- transform_id:

  The transform identifier.

- con:

  Optional DBI connection.

## Value

A tibble of input datasets with their roles.
