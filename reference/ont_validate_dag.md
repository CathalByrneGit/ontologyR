# Validate Transform DAG

Checks for cycles in the transform graph (which would indicate invalid
definitions).

## Usage

``` r
ont_validate_dag(con = NULL)
```

## Arguments

- con:

  Optional DBI connection.

## Value

A list with is_valid (boolean) and any cycles found.
