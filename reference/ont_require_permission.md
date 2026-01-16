# Require Permission

Throws an error if user doesn't have the required permission. Use this
as a guard in functions that need permission checks.

## Usage

``` r
ont_require_permission(
  user_id,
  permission,
  scope_type = NULL,
  scope_value = NULL,
  con = NULL
)
```

## Arguments

- user_id:

  The user identifier.

- permission:

  The required permission.

- scope_type:

  Optional scope type.

- scope_value:

  Optional scope value.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE if permitted.
