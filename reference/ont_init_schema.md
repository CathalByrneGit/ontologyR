# Initialize Ontology Schema

Creates the ontology tables if they don't exist. Called automatically by
[`ont_connect()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_connect.md)
unless `init = FALSE`.

## Usage

``` r
ont_init_schema(con = NULL)
```

## Arguments

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE`.
