# ontologyR

> Evidence-based governance for data definitions

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## The Problem

Most data problems aren't about bad data—they're about **bad definitions**.

When a dashboard says "X patients are ready for discharge", it's showing the result of a **rule**, not reality. That rule might be sensible, but once it becomes a target, people adapt to it. The definition doesn't change, but what it measures does.

This is **ontology drift**: the gradual divergence between what a definition claims to measure and what it actually measures.

## The Solution

`ontologyR` treats definitions as **hypotheses, not decrees**:

- **Version definitions** — never overwrite, always add new versions
- **Audit definitions** — sample cases and check if definitions match reality
- **Detect drift** — track disagreement rates over time
- **Govern with evidence** — use audit data to inform adoption and deprecation

## Installation

```r
# Install from GitHub
devtools::install_github("CathalByrneGit/ontologyR")
```

## Two API Styles

ontologyR offers two equivalent ways to work with ontologies:

### Style 1: Object-based (recommended for interactive use)

Inspired by `ontologyIndex`, this feels R-native with `$` accessors:

```r
library(ontologyR)

# Create ontology object
ont <- ontology(":memory:")

# Register and define
ont$register_object("Encounter", "encounters", "encounter_id")
ont$define_concept("ready_for_discharge", "Encounter")
ont$add_version("ready_for_discharge", "flow", 1, 
                "NOT planned_intervention", status = "active")

# Explore with $ syntax
ont$concepts
ont$concepts$ready_for_discharge
ont$concepts$ready_for_discharge$flow[[1]]

# Evaluate from version object
v <- ont$concepts$ready_for_discharge$flow[[1]]
result <- v$evaluate()

# Audit workflow
sample <- ont$sample("ready_for_discharge", "flow", n = 20)
ont$record_audit("ready_for_discharge", "flow", 1, "E1", TRUE, FALSE, "reviewer")
ont$drift_status()

ont$disconnect()
```

### Style 2: Function-based (recommended for scripts/packages)

Explicit functions with database connection management:

```r
library(ontologyR)

# Connect to ontology database
ont_connect(":memory:")

# Register object type (maps logical name to physical table)
ont_register_object("Encounter", "encounters", "encounter_id")

# Define a concept
ont_define_concept(
  concept_id = "ready_for_discharge",
  object_type = "Encounter",
  description = "Patient is ready to leave hospital"
)

# Add a versioned definition
ont_add_version(
  concept_id = "ready_for_discharge",
  scope = "flow",
  version = 1,
  sql_expr = "NOT planned_intervention_24h",
  status = "active",
  rationale = "Proxy: no planned interventions"
)

# Evaluate the concept
result <- ont_evaluate("ready_for_discharge", "flow", 1)

# Sample for audit
sample <- ont_sample_for_audit("ready_for_discharge", "flow", n = 20)

# Record human judgment
ont_record_audit(
  concept_id = "ready_for_discharge",
  scope = "flow", version = 1,
  object_key = "E123",
  system_value = TRUE,
  reviewer_value = FALSE,  # Disagreement!
  reviewer_id = "dr_smith"
)

# Check for drift
ont_detect_drift("ready_for_discharge", "flow", threshold = 0.15)

# View governance status
ont_drift_status()
```

## Core Concepts

| Term | Meaning |
|------|---------|
| **Object Type** | A logical entity (e.g., "Encounter") mapped to a physical table |
| **Concept** | A named definition (e.g., "ready_for_discharge") |
| **Version** | A specific SQL expression for a concept, scoped and numbered |
| **Scope** | Context for a definition (e.g., "flow", "clinical", "regulatory") |
| **Audit** | A human judgment comparing system evaluation to ground truth |
| **Drift** | When disagreement rate exceeds threshold |

## Concept Templates

Templates let you define a **base concept** that can be **inherited** by scope-specific variants. This is perfect for standards like ILO unemployment definitions where different countries have local adaptations.

```r
# Define an ILO unemployment template
ont_define_template(
  template_id = "ilo_unemployed",
  template_name = "ILO Unemployment Definition",
  object_type = "Person",
  base_sql_expr = "age >= {{min_age}} AND age <= {{max_age}} AND NOT employed AND seeking_work",
  parameters = list(
    min_age = list(default = 15, type = "integer"),
    max_age = list(default = 74, type = "integer")
  ),
  source_standard = "ILO"
)

# Create country-specific variants
ont_inherit_concept("unemployed_us", "ilo_unemployed", "united_states",
  parameter_values = list(min_age = 16, max_age = 65),
  deviation_notes = "US uses 16-65 age range per BLS")

ont_inherit_concept("unemployed_ireland", "ilo_unemployed", "ireland",
  parameter_values = list(min_age = 15, max_age = 66))

# Compare all variants
ont_compare_template_variants("ilo_unemployed")
```

## Interactive Shiny Apps

ontologyR includes three Shiny applications for interactive exploration and management:

### Ontology Explorer

Browse concepts, templates, audits, and governance information:

```r
ont_run_explorer()
# Or point to a specific database:
ont_run_explorer(db_path = "my_ontology.duckdb")
```

### Definition Builder

Visual SQL builder for non-technical users to create concept definitions:

```r
ont_run_definition_builder()
```

### Lineage Viewer

Interactive DAG visualization of datasets and transforms:

```r
ont_run_lineage_viewer()
```

List all available apps:

```r
ont_list_apps()
```

## Actions & Writeback

Actions enable governed operations based on concept evaluations—bridging analytics to operations:

```r
# Define an action type
ont_define_action(
  action_type_id = "escalate_to_manager",
  action_name = "Escalate to Manager",
  object_type = "Patient",
  trigger_concept = "high_risk_patient",
  trigger_scope = "clinical",
  trigger_condition = "concept_value = TRUE",
  parameters = list(
    reason = list(type = "text", required = TRUE),
    priority = list(type = "enum", values = c("normal", "urgent"))
  )
)

# Execute action (with full audit trail)
ont_execute_action("escalate_to_manager",
  object_key = "P123",
  params = list(reason = "Multiple risk factors", priority = "urgent"),
  actor = "nurse_jones")

# Review action history
ont_action_history("escalate_to_manager")
```

## Composite Scores

Combine multiple concepts into weighted risk/health scores:

```r
# Define a composite score
ont_define_score(
  score_id = "patient_risk_score",
  score_name = "Patient Risk Score",
  object_type = "Patient",
  components = list(
    list(concept_id = "fall_risk", scope = "clinical", weight = 0.3),
    list(concept_id = "readmission_risk", scope = "clinical", weight = 0.4),
    list(concept_id = "medication_complexity", scope = "pharmacy", weight = 0.3)
  ),
  aggregation = "weighted_sum",
  thresholds = list(low = 30, medium = 60, high = 80)
)

# Evaluate scores for all patients
scores <- ont_evaluate_score("patient_risk_score")
high_risk <- scores[scores$tier == "high", ]

# Track score distribution over time
ont_observe_score("patient_risk_score")
ont_score_trend("patient_risk_score")
```

## Scenario Analysis

Test definition changes before deployment with what-if analysis:

```r
# Compare current vs proposed definition
scenario <- ont_scenario_analysis(
  concept_id = "ready_for_discharge",
  scope = "clinical",
  proposed_sql = "los_days >= 2 AND NOT pending_results AND mobility_score > 3"
)

scenario$summary
#> Current: 142 patients match
#> Proposed: 98 patients match
#> Newly excluded: 52
#> Newly included: 8

# See specific cases affected
scenario$newly_excluded
scenario$newly_included

# Compare multiple proposals
ont_compare_scenarios("high_risk", "clinical", proposals = list(
  conservative = "risk_score >= 80",
  moderate = "risk_score >= 70",
  aggressive = "risk_score >= 60"
))

# Approve and implement as new version
ont_approve_scenario(scenario$scenario_id, "governance_board", implement = TRUE)
```

## API Reference

### Top-level Entry Points

| Function | Description |
|----------|-------------|
| `ontology()` | **Recommended**: Create an ontology object with `$` accessors |
| `ont_connect()` | Connect using function-based API |

### Object-based API (`ont$...`)

```r
ont <- ontology("path/to/db.duckdb")

# Exploration (returns S3 objects)
ont$concepts                           # All concepts
ont$concepts$my_concept                # Single concept
ont$concepts$my_concept$scope[[1]]     # Specific version
ont$objects                            # All object types

# Actions (return invisibly)
ont$register_object(...)
ont$define_concept(...)
ont$add_version(...)
ont$activate(...)
ont$deprecate(...)

# Queries (return data)
ont$evaluate(concept_id, scope, version)
ont$compare(concept_id, scope, v1, v2)
ont$sample(concept_id, scope, n)
ont$audit_summary(concept_id, scope, version)
ont$drift_status()

# Governance
ont$check_policy(action, concept_id, scope, version)
ont$governance_report()

# Utilities
ont$sql("SELECT ...")
ont$refresh()
ont$disconnect()
```

### Function-based API (`ont_*()`)

#### Connection
- `ont_connect()` / `ont_disconnect()` — Manage database connection
- `ont_status()` — View connection and content summary

### Objects & Links
- `ont_register_object()` — Map logical type to table
- `ont_register_link()` — Define relationships
- `ont_query_objects()` / `ont_query_linked()` — Query data

### Concepts
- `ont_define_concept()` — Create a new concept
- `ont_add_version()` — Add versioned definition
- `ont_activate_version()` / `ont_deprecate_version()` — Manage lifecycle
- `ont_evaluate()` — Apply concept to data
- `ont_compare_versions()` — Compare definition outputs

### Auditing
- `ont_sample_for_audit()` — Get random sample for review
- `ont_record_audit()` — Record human judgment
- `ont_audit_summary()` — View audit statistics
- `ont_rolling_disagreement()` — Track disagreement over time

### Drift Detection
- `ont_check_drift()` — Test for drift
- `ont_detect_drift()` — Check and create drift event
- `ont_list_drift_events()` — View drift history
- `ont_resolve_drift()` — Close drift event
- `ont_drift_status()` — Overview across all concepts

### Governance
- `ont_log_governance()` — Record governance action
- `ont_get_governance_log()` — View action history
- `ont_check_policy()` — Validate proposed action
- `ont_deprecation_impact()` — Analyze change impact
- `ont_governance_report()` — Generate status report

### Templates
- `ont_define_template()` — Create a reusable template with parameters
- `ont_inherit_concept()` — Create concept from template with custom parameters
- `ont_get_template()` — Retrieve template details
- `ont_list_templates()` — List all templates
- `ont_get_template_variants()` — List concepts derived from a template
- `ont_compare_template_variants()` — Compare parameter values across variants
- `ont_render_template()` — Preview SQL with parameter substitution
- `ont_get_concept_inheritance()` — Get template lineage for a concept

### Interactive Apps
- `ont_run_explorer()` — Launch Ontology Explorer (browse concepts, templates, audits)
- `ont_run_definition_builder()` — Launch Definition Builder (visual SQL builder)
- `ont_run_lineage_viewer()` — Launch Lineage Viewer (DAG visualization)
- `ont_list_apps()` — List available Shiny apps

### Actions & Writeback
- `ont_define_action()` — Define an action type with parameters and triggers
- `ont_execute_action()` — Execute an action on an object
- `ont_approve_action()` / `ont_reject_action()` — Approve or reject pending actions
- `ont_action_history()` — View action audit trail
- `ont_pending_actions()` — List actions awaiting approval
- `ont_available_actions()` — Get actions available for an object
- `ont_action_summary()` — Action statistics

### Composite Scores
- `ont_define_score()` — Create a composite score from multiple concepts
- `ont_evaluate_score()` — Calculate scores for objects
- `ont_add_score_component()` / `ont_remove_score_component()` — Manage components
- `ont_observe_score()` — Record score distribution snapshot
- `ont_score_trend()` — Get historical score observations
- `ont_score_distribution()` — Distribution statistics

### Scenario Analysis
- `ont_scenario_analysis()` — Compare current vs proposed definitions
- `ont_compare_scenarios()` — Compare multiple proposals
- `ont_impact_analysis()` — Analyze downstream effects of changes
- `ont_approve_scenario()` / `ont_reject_scenario()` — Decide on scenarios
- `ont_scenario_diff()` — Detailed comparison with sample data

## Design Principles

1. **Definitions are hypotheses** — test them, don't assume they're right
2. **Disagreement is evidence** — audit mismatches are signal, not noise
3. **Drift is measurable** — track disagreement rates systematically
4. **Governance follows evidence** — block actions on drifting definitions
5. **Coexistence over replacement** — multiple versions can be active

## Learn More

- **New to ontologies?** Start with `vignette("what-is-an-ontology")` — a plain-English guide with bakery analogies
- `vignette("introduction")` — Code examples and API walkthrough
- `vignette("templates-and-inheritance")` — Creating reusable concept templates
- `vignette("official-statistics")` — Working with statistical standards (ILO, OECD)

## Design Notes: Tables vs. Classes

ontologyR uses **tables as the source of truth** but provides **S3 classes for ergonomics**.

### Why tables (not named lists like ontologyIndex)?

| Our problem | Their problem |
|-------------|---------------|
| Definitions change over time | Ontologies are static |
| Need versioning + audit trails | Just hierarchy traversal |
| "Is this definition still valid?" | "Is term A ancestor of B?" |
| Scale unknown (could be enterprise) | Fits in memory |

Tables give us:
- Natural versioning (just add rows)
- SQL evaluation (definitions *are* SQL)
- Integration with data lakes (csolake)
- Audit queries for free

### Why S3 accessors on top?

Interactive R users expect `ont$concepts$foo`, not `ont_get_concept(con, "foo")`.

The hybrid approach:
- Database = source of truth
- S3 objects = cached views with nice printing
- Both APIs use the same tables

### When to use which

| Situation | Recommendation |
|-----------|----------------|
| Interactive exploration | `ont <- ontology(); ont$concepts` |
| Scripts and packages | `ont_connect(); ont_evaluate(...)` |
| Building pipelines | Function-based (explicit control) |
| Teaching / demos | Object-based (less boilerplate) |

## Related Work

### Comparison with ontologyIndex

[ontologyIndex](https://cran.r-project.org/package=ontologyIndex) is excellent for **bio-ontologies** (HPO, GO). Here's how we differ:

```r
# ontologyIndex: static hierarchy from OBO file
data(hpo)
hpo$name[["HP:0001250"]]          # "Seizure"
get_ancestors(hpo, "HP:0001250")  # Returns parent terms

# ontologyR: versioned definitions with governance
ont <- ontology("metrics.duckdb")
ont$concepts$ready_for_discharge$flow[[1]]$sql_expr  # "NOT planned_intervention"
ont$concepts$ready_for_discharge$flow[[1]]$audit_summary  # Drift stats
```

| | ontologyIndex | ontologyR |
|-|---------------|-----------|
| Domain | Bio-ontologies | Operational definitions |
| Data structure | Named lists | Tables + S3 views |
| Mutability | Read-only | Versioned, auditable |
| Key operation | Hierarchy traversal | SQL evaluation + drift detection |
| Persistence | In-memory | Database-backed |

**Use ontologyIndex for:** Gene Ontology, HPO, disease taxonomies

**Use ontologyR for:** KPIs, metrics, business definitions that need governance

### Other influences

- **Goodhart's Law** — "When a measure becomes a target, it ceases to be a good measure"
- **Statistical process control** — Treating drift as a measurable signal
- **Data mesh** — Decentralized ownership with standards
- Critical analysis of analytics platforms and definition lock-in

## License

MIT
