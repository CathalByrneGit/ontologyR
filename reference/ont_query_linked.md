# Query Linked Objects

Starting from objects of one type, traverse a link to get related
objects.

## Usage

``` r
ont_query_linked(from_type, from_ids, link_type, as_of = NULL, con = NULL)
```

## Arguments

- from_type:

  Character. Source object type.

- from_ids:

  Vector of source object primary keys.

- link_type:

  Character. The link type to traverse.

- as_of:

  Date or timestamp. For temporal links, return links valid at this
  point in time. If `NULL`, returns all links.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A lazy dplyr tbl with joined target objects.
