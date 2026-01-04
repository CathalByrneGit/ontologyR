# List Concepts

Returns all registered concepts with their metadata.

## Usage

``` r
ont_list_concepts(object_type = NULL, owner_domain = NULL, con = NULL)
```

## Arguments

- object_type:

  Character. Optional filter by object type.

- owner_domain:

  Character. Optional filter by owner domain.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of concepts.
