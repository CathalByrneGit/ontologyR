# End-to-End Workflow: From Data to Governed Definitions

## The Big Picture

This vignette walks through a complete real-world scenario, showing how
all the pieces of ontologyR fit together.

**Scenario**: You’re the data lead at a healthcare organization. You
need to:

1.  Define what “ready for discharge” means
2.  Test it against clinical reality
3.  Get it approved for production use
4.  Materialize it for reporting
5.  Monitor for drift over time

Let’s do it step by step.

------------------------------------------------------------------------

## Phase 1: Setting Up the Foundation

First, connect and set up your source data.

``` r
library(ontologyR)

# Connect to database (use a real path in production)
ont_connect("healthcare_ontology.duckdb")

# In real life, these tables would exist in your data warehouse
# Here we'll create sample data for illustration
DBI::dbWriteTable(ont_get_connection(), "encounters", tibble::tibble(
  encounter_id = paste0("ENC", 1:1000),
  patient_id = paste0("PAT", sample(1:200, 1000, replace = TRUE)),
  admission_date = Sys.Date() - sample(1:30, 1000, replace = TRUE),
  los_days = sample(1:14, 1000, replace = TRUE),
  has_pending_tests = sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.3, 0.7)),
  has_pending_consults = sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.2, 0.8)),
  discharge_plan_complete = sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.6, 0.4)),
  medically_stable = sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.7, 0.3))
))

# Register the source dataset
ont_register_dataset(
  dataset_id = "ds_encounters",
  dataset_name = "Hospital Encounters",
  physical_name = "encounters",
  dataset_type = "source",
  owner = "clinical_data_team",
  description = "Real-time feed from EHR system"
)

# Register the object type
ont_register_object(
  object_type = "Encounter",
  table_name = "encounters",
  pk_column = "encounter_id",
  description = "A patient hospital encounter/admission",
  owner_domain = "clinical"
)
```

**What we did**: Created the foundational layer — source data
registration and object type mapping. This tells ontologyR “here’s our
data, and here’s what we call the things in it.”

------------------------------------------------------------------------

## Phase 2: Defining the Concept

Now define what “ready for discharge” means — but as a testable
hypothesis, not a decree.

``` r
# Define the concept (what we're trying to measure)
ont_define_concept(
  concept_id = "ready_for_discharge",
  object_type = "Encounter",
  description = "Patient is clinically ready to leave the hospital",
  owner_domain = "patient_flow"
)

# Version 1: Simple operational definition
ont_add_version(
  concept_id = "ready_for_discharge",
  scope = "operations",
  version = 1,
  sql_expr = "NOT has_pending_tests AND NOT has_pending_consults",
  status = "draft",
  rationale = "Initial proxy: no pending tests or consults means ready"
)

# Evaluate it to see what it captures
result <- ont_evaluate("ready_for_discharge", "operations", version = 1)

# Summary
table(result$concept_value)
#>  FALSE  TRUE
#>    420   580

# 580 patients flagged as "ready" - but is this actually right?
```

**What we did**: Created a concept with a draft definition. The
definition is explicit (SQL) and versioned. We can now test whether this
definition matches clinical reality.

------------------------------------------------------------------------

## Phase 3: Auditing the Definition

The key insight: **definitions are hypotheses**. We test them by having
humans check samples.

``` r
# Sample patients that the system says are "ready"
sample_ready <- ont_sample_for_audit(
  concept_id = "ready_for_discharge",
  scope = "operations",
  n = 20,
  concept_value = TRUE  # Sample from those flagged as ready
)

# In real life, clinical staff would review each case
# Here's what that might look like:

# Case 1: System says ready, clinician agrees
ont_record_audit(
  concept_id = "ready_for_discharge",
  scope = "operations",
  version = 1,
  object_key = sample_ready$encounter_id[1],
  system_value = TRUE,
  reviewer_value = TRUE,
  reviewer_id = "dr_smith",
  notes = "Patient stable, family ready, transport arranged"
)

# Case 2: System says ready, but clinician disagrees!
ont_record_audit(
  concept_id = "ready_for_discharge",
  scope = "operations",
  version = 1,
  object_key = sample_ready$encounter_id[2],
  system_value = TRUE,
  reviewer_value = FALSE,
  reviewer_id = "dr_smith",
  notes = "Patient needs social work assessment - no safe discharge destination"
)

# Continue for all 20 samples...
# (In practice, you'd batch import from a review form)

# Check the audit summary
ont_audit_summary("ready_for_discharge", "operations", 1)
#> -- Audit Summary: ready_for_discharge@operations v1 --
#> i Total audits: 20
#> i Agreements: 14 (70%)
#> i Disagreements: 6 (30%)
```

**What we did**: Tested the definition against reality. We found 30%
disagreement — the system is flagging patients as “ready” when
clinicians say they’re not. This is valuable data!

------------------------------------------------------------------------

## Phase 4: Improving the Definition

The audits revealed a gap: we’re missing the “discharge plan complete”
requirement.

``` r
# Create an improved version based on audit feedback
ont_add_version(
  concept_id = "ready_for_discharge",
  scope = "operations",
  version = 2,
  sql_expr = "NOT has_pending_tests AND NOT has_pending_consults AND discharge_plan_complete",
  status = "draft",
  rationale = "Added discharge_plan_complete based on audit findings showing social work gaps"
)

# Compare the versions
comparison <- ont_compare_versions(
  concept_id = "ready_for_discharge",
  scope = "operations",
  v1 = 1,
  v2 = 2
)

comparison$summary
#> # A tibble: 1 x 4
#>   total_objects v1_only v2_only both_true
#>           <int>   <int>   <int>     <int>
#> 1          1000     232       0       348

# v1 flags 580 as ready
# v2 flags 348 as ready
# 232 patients are "ready" by v1 but "not ready" by v2
# These are the ones missing discharge plans!
```

**What we did**: Used audit data to improve the definition. Version 2 is
more accurate because it captures a requirement we discovered through
testing.

------------------------------------------------------------------------

## Phase 5: Getting Approval

Before going to production, we need proper governance.

``` r
# First, audit the new version
sample_v2 <- ont_sample_for_audit("ready_for_discharge", "operations", n = 15, version = 2)

# Record audits (showing high agreement this time)
for (i in 1:15) {
  ont_record_audit(
    "ready_for_discharge", "operations", 2,
    sample_v2$encounter_id[i],
    system_value = sample_v2$concept_value[i],
    reviewer_value = sample_v2$concept_value[i],  # 100% agreement
    reviewer_id = "dr_jones"
  )
}

# Check governance gates
gates <- ont_check_all_gates("ready_for_discharge", "operations", 2, "activation")
gates$blocking_failures
#> $gate_approval_required
#> ... approval still needed

# Request approval
request_id <- ont_request_approval(
  "ready_for_discharge", "operations", 2,
  requested_action = "activate",
  requested_by = "data_analyst"
)

# Clinical lead reviews and approves
ont_approve_request(
  request_id,
  decided_by = "clinical_director",
  decision_notes = "Reviewed v2 definition and audit results. Better captures clinical reality. Approved."
)

# Check gates again
gates <- ont_check_all_gates("ready_for_discharge", "operations", 2, "activation")
gates$overall_passed
#> [1] TRUE

# Activate!
ont_activate_version(
  "ready_for_discharge", "operations", 2,
  activated_by = "clinical_director"
)
#> v Activated ready_for_discharge@operations v2
```

**What we did**: Followed proper governance — audited the new version,
requested approval, got sign-off, then activated. There’s now a clear
audit trail of why this definition is in production.

------------------------------------------------------------------------

## Phase 6: Materializing for Consumption

Now make the data available to downstream systems.

``` r
# Materialize the active definition for reporting
result <- ont_materialize(
  concept_id = "ready_for_discharge",
  scope = "operations",
  output_table = "rpt_ready_for_discharge"
)
#> v Materialized ready_for_discharge to rpt_ready_for_discharge
#> i 348 rows in 0.23 seconds

# The reporting team can now query this table
DBI::dbGetQuery(ont_get_connection(), "
  SELECT COUNT(*) as ready_count,
         AVG(los_days) as avg_los
  FROM rpt_ready_for_discharge
  WHERE concept_value = TRUE
")
#>   ready_count avg_los
#> 1         348    4.2

# Get full provenance
prov <- ont_get_provenance(result$dataset_id)
prov$concept$sql_expr
#> [1] "NOT has_pending_tests AND NOT has_pending_consults AND discharge_plan_complete"
```

**What we did**: Created a production table from the governed
definition. Anyone querying this table can trace back to the exact
definition, version, and approval that generated it.

------------------------------------------------------------------------

## Phase 7: Ongoing Monitoring

Definitions drift over time. Set up monitoring.

``` r
# Schedule regular audit samples (run daily via cron)
daily_audit_check <- function() {
  # Take a fresh sample
  sample <- ont_sample_for_audit("ready_for_discharge", "operations", n = 5)

  # In production, this would trigger a review workflow
  # For now, just return the sample for manual review
  sample
}

# Check for drift periodically
drift_check <- function() {
  ont_detect_drift(
    concept_id = "ready_for_discharge",
    scope = "operations",
    threshold = 0.15,  # Alert if >15% disagreement
    min_audits = 20,
    window_days = 30
  )
}

# Get overall drift status
ont_drift_status()
#> -- Drift Status Summary --
#> v ready_for_discharge@operations v2: OK (8% disagreement, 45 audits)

# Record observations for trend analysis
ont_observe("ready_for_discharge", "operations")
#> Recorded observation: 348 of 1000 (34.8% prevalence)

# View trend over time
trends <- ont_trend_analysis("ready_for_discharge", "operations")
#> Shows prevalence over time - useful for spotting data quality issues
```

**What we did**: Set up ongoing monitoring. We’re tracking audit results
over time and recording observations. If the definition starts to drift
from reality, we’ll catch it early.

------------------------------------------------------------------------

## The Complete Data Flow

Here’s what we built:

    ┌─────────────────────────────────────────────────────────────────────┐
    │                    ONTOLOGY LIFECYCLE                               │
    ├─────────────────────────────────────────────────────────────────────┤
    │                                                                     │
    │  SOURCE DATA              CONCEPT                  OUTPUT           │
    │  ┌──────────┐            ┌─────────────────────┐   ┌──────────┐    │
    │  │encounters│──register──│ready_for_discharge  │──►│dashboard │    │
    │  └──────────┘            │                     │   └──────────┘    │
    │       │                  │ v1: draft (70% acc) │                   │
    │       │                  │ v2: active (95% acc)│                   │
    │       │                  └─────────────────────┘                   │
    │       │                           │                                 │
    │       │                    ┌──────┴──────┐                         │
    │       │                    │             │                         │
    │       │               AUDIT LOOP    GOVERNANCE                     │
    │       │                    │             │                         │
    │       │              ┌─────┴─────┐ ┌─────┴─────┐                   │
    │       │              │ Sample    │ │ Gates     │                   │
    │       │              │ Review    │ │ Approval  │                   │
    │       │              │ Record    │ │ Activate  │                   │
    │       │              └───────────┘ └───────────┘                   │
    │       │                                                             │
    │       └──────────────────────────────────────────────────────────►  │
    │                         LINEAGE TRACKING                            │
    │                                                                     │
    └─────────────────────────────────────────────────────────────────────┘

------------------------------------------------------------------------

## Key Takeaways

### 1. Definitions are hypotheses

Don’t just decree definitions — test them. Version 1 seemed reasonable
until audits showed 30% disagreement.

### 2. Governance creates trust

The approved definition has a clear audit trail. Anyone can see why it’s
in production and who approved it.

### 3. Lineage enables impact analysis

We know exactly what data feeds the dashboard and what would be affected
by changes.

### 4. Monitoring catches problems early

Regular audits and drift detection mean we’ll know if the definition
stops working before it causes problems.

### 5. The process is the product

The value isn’t just the final definition — it’s the evidence-based
process that produced it.

------------------------------------------------------------------------

## Quick Reference: The Commands Used

| Phase       | Commands                                                                                                                                                                                                                                                                                                       |
|-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Setup       | [`ont_connect()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_connect.md), `ont_register_dataset()`, [`ont_register_object()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_register_object.md)                                                                                       |
| Define      | [`ont_define_concept()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_define_concept.md), [`ont_add_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_add_version.md), [`ont_evaluate()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_evaluate.md)                 |
| Audit       | [`ont_sample_for_audit()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_sample_for_audit.md), [`ont_record_audit()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_record_audit.md), [`ont_audit_summary()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_audit_summary.md) |
| Improve     | [`ont_add_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_add_version.md), [`ont_compare_versions()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_compare_versions.md)                                                                                                       |
| Govern      | `ont_check_all_gates()`, `ont_request_approval()`, `ont_approve_request()`, [`ont_activate_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_activate_version.md)                                                                                                                           |
| Materialize | `ont_materialize()`, `ont_get_provenance()`                                                                                                                                                                                                                                                                    |
| Monitor     | [`ont_detect_drift()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_detect_drift.md), `ont_observe()`, `ont_trend_analysis()`                                                                                                                                                                      |

------------------------------------------------------------------------

## Next Steps

You now have a complete governed definition. From here you can:

- **Add more scopes**: Create a “clinical” scope with stricter criteria
- **Build transforms**: Create derived datasets combining multiple
  concepts
- **Set up alerts**: Configure drift detection thresholds
- **Expand coverage**: Apply this workflow to other key definitions

Remember: **The goal isn’t perfect definitions — it’s definitions you
can test, improve, and trust.**
