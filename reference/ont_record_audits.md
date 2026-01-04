# Record Multiple Audit Judgments

Batch version of
[`ont_record_audit()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_record_audit.md)
for recording multiple judgments at once, typically from an audit
session.

## Usage

``` r
ont_record_audits(audits, concept_id, scope, version, reviewer_id, con = NULL)
```

## Arguments

- audits:

  A data frame with columns: object_key, system_value, reviewer_value,
  and optionally notes.

- concept_id:

  Character. The concept being audited.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- reviewer_id:

  Character. Identifier for the reviewer.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns a vector of audit_ids.
