# Create Custom Role

Creates a new role with specified permissions.

## Usage

``` r
ont_create_role(
  role_id,
  role_name,
  permissions,
  description = NULL,
  con = NULL
)
```

## Arguments

- role_id:

  Unique identifier for the role.

- role_name:

  Human-readable name.

- permissions:

  Character vector of permission strings.

- description:

  Optional description.

- con:

  Optional DBI connection.

## Value

The role_id (invisibly).
