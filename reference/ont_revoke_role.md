# Revoke Role from User

Removes a role assignment from a user.

## Usage

``` r
ont_revoke_role(
  user_id,
  role_id,
  scope_type = "global",
  scope_value = NULL,
  con = NULL
)
```

## Arguments

- user_id:

  The user identifier.

- role_id:

  The role to revoke.

- scope_type:

  Scope type.

- scope_value:

  The scope value if scoped.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
