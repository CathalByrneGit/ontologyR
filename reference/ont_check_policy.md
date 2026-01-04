# Check Governance Policies

Validates that a proposed action complies with governance policies.

## Usage

``` r
ont_check_policy(action, concept_id, scope, version, con = NULL)
```

## Arguments

- action:

  Character. Proposed action: "activate", "deprecate", "adopt".

- concept_id:

  Character. The concept.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with `allowed` (logical) and `reasons` (character vector).
