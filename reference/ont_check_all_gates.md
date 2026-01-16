# Check All Gates

Evaluates all applicable gates for an action on a concept version.

## Usage

``` r
ont_check_all_gates(
  concept_id,
  scope,
  version,
  action_type,
  checked_by = NULL,
  con = NULL
)
```

## Arguments

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

A list with overall_passed, blocking_failures, warnings, and all_checks.
