# Get Role Details

Retrieves a role with its parsed permissions.

## Usage

``` r
ont_get_role(role_id, con = NULL)
```

## Arguments

- role_id:

  The role identifier.

- con:

  Optional DBI connection.

## Value

A list with role details and permissions as a character vector.
