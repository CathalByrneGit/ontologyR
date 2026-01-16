# Record a Concept Observation

Captures a point-in-time snapshot of a concept evaluation. Unlike audits
(which compare human judgment to system), observations are recorded
facts about what the system evaluated at a specific moment.

## Usage

``` r
ont_observe(
  concept_id,
  scope,
  version = NULL,
  filter_expr = NULL,
  observation_type = "snapshot",
  store_details = FALSE,
  triggered_by = "manual",
  observer_id = NULL,
  notes = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to observe.

- scope:

  Character. The scope.

- version:

  Integer. The version. If `NULL`, uses active version.

- filter_expr:

  Character. Optional SQL WHERE clause to filter objects.

- observation_type:

  Character. Type of observation: "snapshot" (default), "scheduled", or
  "triggered".

- store_details:

  Logical. If `TRUE`, store object-level details (can be large for big
  datasets). Default `FALSE`.

- triggered_by:

  Character. What triggered this observation (e.g., "manual",
  "schedule", "drift_check").

- observer_id:

  Character. Who/what initiated this observation.

- notes:

  Character. Optional notes.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with observation summary including observation_id, counts, and
prevalence rate.

## Details

This enables:

- Trend analysis: How does concept prevalence change over time?

- Comparison: How do different versions or cohorts differ?

- Monitoring: Track concept behavior in production

## Examples

``` r
if (FALSE) { # \dontrun{
ont_connect(":memory:")
# ... setup ...

# Take a snapshot observation
obs <- ont_observe(
  concept_id = "ready_for_discharge",
  scope = "flow",
  triggered_by = "daily_monitor"
)

# Observe with a filter
obs <- ont_observe(
  concept_id = "ready_for_discharge",
  scope = "flow",
  filter_expr = "ward = 'ICU'",
  notes = "ICU-specific observation"
)
} # }
```
