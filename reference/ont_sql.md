# Execute Raw SQL

Executes arbitrary SQL against the ontology database. Use with caution.

## Usage

``` r
ont_sql(sql, params = NULL, con = NULL)
```

## Arguments

- sql:

  Character. SQL statement to execute.

- params:

  List. Optional parameters for parameterized queries.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

For SELECT queries, returns a tibble. For other queries, returns the
number of affected rows.
