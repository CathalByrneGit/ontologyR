# Grant Role to User

Assigns a role to a user, optionally scoped to a domain or concept.

## Usage

``` r
ont_grant_role(
  user_id,
  role_id,
  scope_type = "global",
  scope_value = NULL,
  granted_by = NULL,
  expires_at = NULL,
  con = NULL
)
```

## Arguments

- user_id:

  The user identifier.

- role_id:

  The role to grant.

- scope_type:

  Scope type: "global", "domain", or "concept".

- scope_value:

  The domain name or concept_id if scoped.

- granted_by:

  Who granted this role.

- expires_at:

  Optional expiration timestamp.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
