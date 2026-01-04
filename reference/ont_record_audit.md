# Record an Audit Judgment

Records a human reviewer's judgment about whether an object truly meets
a concept definition. The comparison between `system_value` (what the
concept evaluated to) and `reviewer_value` (what the human judged) is
the basis for drift detection.

## Usage

``` r
ont_record_audit(
  concept_id,
  scope,
  version,
  object_key,
  system_value,
  reviewer_value,
  reviewer_id,
  notes = NULL,
  audit_id = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept being audited.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- object_key:

  Character. Primary key of the audited object.

- system_value:

  Logical. What the concept evaluated to.

- reviewer_value:

  Logical. What the reviewer judged.

- reviewer_id:

  Character. Identifier for the reviewer.

- notes:

  Character. Optional notes explaining the judgment.

- audit_id:

  Character. Optional custom audit ID. If `NULL`, auto-generated.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the audit_id.
