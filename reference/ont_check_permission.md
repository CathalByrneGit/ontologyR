# Check User Permission

Checks if a user has a specific permission, considering all their roles.

## Usage

``` r
ont_check_permission(
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

  The permission to check (e.g., "concept:write").

- scope_type:

  Optional scope type for context-specific check.

- scope_value:

  Optional scope value for context-specific check.

- con:

  Optional DBI connection.

## Value

TRUE if user has permission, FALSE otherwise.
