# Query Objects by Primary Key

Retrieves objects of a given type by their primary key values.

## Usage

``` r
ont_query_objects(object_type, ids = NULL, con = NULL)
```

## Arguments

- object_type:

  Character. The object type to query.

- ids:

  Vector of primary key values to filter by. If `NULL`, returns all.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A lazy dplyr tbl. Call `collect()` to retrieve data.
