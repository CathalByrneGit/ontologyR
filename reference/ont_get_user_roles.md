# Get User Roles

Returns all roles assigned to a user.

## Usage

``` r
ont_get_user_roles(user_id, include_expired = FALSE, con = NULL)
```

## Arguments

- user_id:

  The user identifier.

- include_expired:

  Include expired role assignments.

- con:

  Optional DBI connection.

## Value

A tibble of role assignments.
