# List Registered Object Types

Returns a tibble of all registered object types with their metadata.

## Usage

``` r
ont_list_objects(con = NULL)
```

## Arguments

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with columns: object_type, table_name, pk_column, description,
owner_domain, created_at, created_by.
