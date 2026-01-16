# Introduction to ontologyR

## The Problem: Definitions Drift

Most data problems aren’t about bad data—they’re about **bad
definitions**.

When a dashboard says “X patients are ready for discharge”, what it’s
really showing is not reality, but a **rule**. That rule might be:

> “A patient is ready for discharge if there is no planned intervention
> in the next 24 hours.”

This seems reasonable. Until it becomes a target.

Once a definition is used for performance management, people adapt to
it. Staff may record interventions more cautiously, delay documentation,
or reinterpret what “planned” means. The definition hasn’t changed, but
what it measures has.

This is **ontology drift**: the gradual divergence between what a
definition claims to measure and what it actually measures.

## The Solution: Evidence-Based Definitions

`ontologyR` treats definitions as **hypotheses, not decrees**. The core
idea is simple:

1.  **Version definitions** — never overwrite them
2.  **Audit definitions** — sample cases and check if the definition
    matches reality
3.  **Detect drift** — track disagreement rates over time
4.  **Govern with evidence** — use audit data to inform adoption and
    deprecation

## Quick Start

``` r
library(ontologyR)

# Connect to an ontology database
ont_connect(":memory:")

# First, we need some data to define concepts against
DBI::dbWriteTable(ont_get_connection(), "encounters", tibble::tibble(
  encounter_id = c("E1", "E2", "E3", "E4", "E5"),
  patient_age = c(80, 42, 70, 65, 55),
  planned_intervention_24h = c(TRUE, FALSE, FALSE, TRUE, FALSE),
  arrangements_confirmed = c(FALSE, TRUE, FALSE, TRUE, TRUE)
))

# Register the object type (maps logical name to physical table)
ont_register_object(
  object_type = "Encounter",
  table_name = "encounters",
  pk_column = "encounter_id",
  description = "A patient encounter or visit"
)

# Define a concept
ont_define_concept(
  concept_id = "ready_for_discharge",
  object_type = "Encounter",
  description = "Patient is clinically ready to leave hospital",
  owner_domain = "patient_flow"
)

# Add version 1: simple proxy definition
ont_add_version(
  concept_id = "ready_for_discharge",
  scope = "flow",
  version = 1,
  sql_expr = "NOT planned_intervention_24h",
  status = "active",
  rationale = "Operational proxy: no planned interventions in next 24h"
)

# Evaluate the concept
result <- ont_evaluate("ready_for_discharge", "flow", 1)
print(result)
#> # A tibble: 5 × 5
#>   encounter_id patient_age planned_intervention_24h arrangements_confirmed concept_value
#>   <chr>              <dbl> <lgl>                    <lgl>                  <lgl>        
#> 1 E1                    80 TRUE                     FALSE                  FALSE        
#> 2 E2                    42 FALSE                    TRUE                   TRUE         
#> 3 E3                    70 FALSE                    FALSE                  TRUE         
#> 4 E4                    65 TRUE                     TRUE                   FALSE        
#> 5 E5                    55 FALSE                    TRUE                   TRUE
```

## The Audit Loop

The key innovation is the **audit loop**: regularly sample cases that
the system says meet a definition, have humans check if that’s actually
true, and track the disagreement rate.

``` r
# Sample cases for audit
sample <- ont_sample_for_audit(
  concept_id = "ready_for_discharge",
  scope = "flow",
  n = 3,
  concept_value = TRUE  
)

# After human review, record judgments
# (In practice, this would come from a review form or clinical system)
ont_record_audit(
  concept_id = "ready_for_discharge",
  scope = "flow",
  version = 1,
  object_key = "E2",
  system_value = TRUE,
  reviewer_value = TRUE,  # Reviewer agrees
  reviewer_id = "dr_smith"
)

ont_record_audit(
  concept_id = "ready_for_discharge",
  scope = "flow",
  version = 1,
  object_key = "E3",
  system_value = TRUE,
  reviewer_value = FALSE,  # Reviewer disagrees!
  reviewer_id = "dr_smith",
  notes = "Patient needs social work assessment"
)

# Check audit summary
ont_audit_summary("ready_for_discharge", "flow", 1)
#> ── Audit Summary: ready_for_discharge@flow v1 ───────────────────────────
#> ℹ Total audits: 2
#> ℹ Agreements: 1 (50%)
#> ℹ Disagreements: 1 (50%)
```

## Detecting Drift

With enough audit data, drift becomes detectable:

``` r
# Check for drift (threshold-based)
ont_detect_drift(
  concept_id = "ready_for_discharge",
  scope = "flow",
  threshold = 0.15,      # Alert if >15% disagreement
  min_audits = 10,       # Need at least 10 audits
  window_days = 30       # Look at last 30 days
)

# Get drift status across all concepts
ont_drift_status()
#> ── Drift Status Summary ─────────────────────────────────────────────────
#> ✓ ready_for_discharge@flow v1: OK
```

## Governance Actions

When drift is detected, the package enforces evidence-based governance:

``` r
# Can't deprecate a version with open drift events
ont_deprecate_version(
  "ready_for_discharge", "flow", 1,
  deprecated_by = "data_team",
  rationale = "Replacing with v2"
)
#> Error: Cannot deprecate: 1 open drift event(s) exist.
#> ℹ Resolve drift events first or use `force = TRUE`.

# Check policy before taking action
ont_check_policy("deprecate", "ready_for_discharge", "flow", 1)
#> ✖ Action BLOCKED:
#> • 1 open drift event(s) must be resolved first
```

## Version Comparison

Compare how different definition versions classify the same cases:

``` r
# Add a refined version
ont_add_version(
  concept_id = "ready_for_discharge",
  scope = "flow",
  version = 2,
  sql_expr = "NOT planned_intervention_24h AND arrangements_confirmed",
  status = "draft",
  rationale = "Added arrangements requirement based on audit findings"
)

# Compare versions
ont_compare_versions("ready_for_discharge", "flow", 1, 2)
#> ── Comparison: ready_for_discharge@flow v1 vs v2 ────────────────────────
#> ℹ Total objects: 5
#> ℹ Agreement: 3 (60%)
#> ℹ Disagreement: 2 (40%)
```

## Key Principles

1.  **Definitions are hypotheses** — they should be tested, not assumed
2.  **Disagreement is evidence** — audit mismatches are data, not errors
3.  **Drift is measurable** — track disagreement rates over time
4.  **Governance follows evidence** — block actions on drifting
    definitions
5.  **Coexistence over replacement** — multiple versions can be active
    for comparison

## Next Steps

- See
  [`vignette("what-is-an-ontology")`](https://cathalbyrnegit.github.io/ontologyR/articles/what-is-an-ontology.md)
  for a conceptual overview with analogies
- See
  [`vignette("datasets-and-materialization")`](https://cathalbyrnegit.github.io/ontologyR/articles/datasets-and-materialization.md)
  for registering datasets and materializing concepts
- See
  [`vignette("transforms-and-lineage")`](https://cathalbyrnegit.github.io/ontologyR/articles/transforms-and-lineage.md)
  for building data pipelines with lineage tracking
- See
  [`vignette("governance-gates")`](https://cathalbyrnegit.github.io/ontologyR/articles/governance-gates.md)
  for access control and quality gates
- See
  [`vignette("end-to-end-workflow")`](https://cathalbyrnegit.github.io/ontologyR/articles/end-to-end-workflow.md)
  for a complete worked example
