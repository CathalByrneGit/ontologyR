# Get Object Type Metadata

Retrieves the metadata for a specific object type.

## Usage

``` r
ont_get_object(object_type, con = NULL)
```

## Arguments

- object_type:

  Character. The object type to look up.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A single-row tibble with object type metadata.
