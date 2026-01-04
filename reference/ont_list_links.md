# List Registered Link Types

Returns a tibble of all registered link types with their metadata.

## Usage

``` r
ont_list_links(con = NULL)
```

## Arguments

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with link type metadata.
