# Validate SQL Expression

Checks if a SQL expression is syntactically valid by attempting to
prepare it against the object type's table.

## Usage

``` r
ont_validate_sql(sql_expr, object_type, con = NULL)
```

## Arguments

- sql_expr:

  Character. SQL expression to validate.

- object_type:

  Character. Object type to validate against.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with `valid` (logical) and `error` (character or NULL).
