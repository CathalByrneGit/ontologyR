# Log a Governance Action

Records a governance action in the audit trail. This is called
internally by other functions (e.g., `ont_activate_version`,
`ont_resolve_drift`) but can also be called directly for custom actions.

## Usage

``` r
ont_log_governance(
  action_type,
  concept_id,
  scope = NULL,
  version = NULL,
  actor,
  rationale = NULL,
  evidence = NULL,
  blocked_by = NULL,
  con = NULL
)
```

## Arguments

- action_type:

  Character. Type of action: "adopt", "activate", "deprecate", "retire",
  "review", "block", "unblock", "drift_detected", "drift_resolved", etc.

- concept_id:

  Character. The concept involved.

- scope:

  Character. Optional scope.

- version:

  Integer. Optional version.

- actor:

  Character. Who performed the action.

- rationale:

  Character. Optional explanation.

- evidence:

  Character. Optional JSON string with supporting data.

- blocked_by:

  Character. Optional reference to blocking condition.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the log_id.
