# Activate a Concept Version

Sets a concept version's status to "active". This is a governance action
that is logged.

## Usage

``` r
ont_activate_version(
  concept_id,
  scope,
  version,
  approved_by,
  rationale = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept ID.

- scope:

  Character. The scope.

- version:

  Integer. The version to activate.

- approved_by:

  Character. Who is approving this activation.

- rationale:

  Character. Optional reason for activation.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE` on success.
