# Check Gate

Evaluates a gate for a concept version.

## Usage

``` r
ont_check_gate(
  gate_id,
  concept_id,
  scope,
  version,
  action_type,
  checked_by = NULL,
  con = NULL
)
```

## Arguments

- gate_id:

  The gate to check.

- concept_id:

  The concept.

- scope:

  The scope.

- version:

  The version.

- action_type:

  The action being attempted.

- checked_by:

  User performing the check.

- con:

  Optional DBI connection.

## Value

A list with passed (boolean), details, and check_id.
