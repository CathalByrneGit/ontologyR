# Override Gate Check

Records a gate override with reason. Requires gate:override permission.

## Usage

``` r
ont_override_gate(check_id, override_reason, overridden_by, con = NULL)
```

## Arguments

- check_id:

  The check to override.

- override_reason:

  Reason for the override.

- overridden_by:

  User overriding.

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
