# Create Governance Gate

Creates a new governance gate.

## Usage

``` r
ont_create_gate(
  gate_id,
  gate_name,
  gate_type,
  applies_to,
  conditions,
  severity = "blocking",
  scope_filter = NULL,
  domain_filter = NULL,
  description = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- gate_id:

  Unique identifier.

- gate_name:

  Human-readable name.

- gate_type:

  Type: "audit_coverage", "drift_threshold", "approval_required",
  "custom".

- applies_to:

  Action: "activation", "deprecation", "materialization", "all".

- conditions:

  List of conditions for the gate.

- severity:

  "blocking" or "warning".

- scope_filter:

  Optional scope filter.

- domain_filter:

  Optional domain filter.

- description:

  Optional description.

- created_by:

  Optional user.

- con:

  Optional DBI connection.

## Value

The gate_id (invisibly).
