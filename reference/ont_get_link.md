# Get Link Type Metadata

Retrieves the metadata for a specific link type.

## Usage

``` r
ont_get_link(link_type, con = NULL)
```

## Arguments

- link_type:

  Character. The link type to look up.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A single-row tibble with link type metadata.
