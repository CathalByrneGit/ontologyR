# ontologyR

> Evidence-based governance for data definitions

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## The Problem

Most data problems aren’t about bad data—they’re about **bad
definitions**.

When a dashboard says “X patients are ready for discharge”, it’s showing
the result of a **rule**, not reality. That rule might be sensible, but
once it becomes a target, people adapt to it. The definition doesn’t
change, but what it measures does.

This is **ontology drift**: the gradual divergence between what a
definition claims to measure and what it actually measures.

## The Solution

`ontologyR` treats definitions as **hypotheses, not decrees**:

- **Version definitions** — never overwrite, always add new versions
- **Audit definitions** — sample cases and check if definitions match
  reality
- **Detect drift** — track disagreement rates over time
- **Govern with evidence** — use audit data to inform adoption and
  deprecation

## Installation

``` r
# Install from GitHub
devtools::install_github("CathalByrneGit/ontologyR")
```

## Two API Styles

ontologyR offers two equivalent ways to work with ontologies:

### Style 1: Object-based (recommended for interactive use)

Inspired by `ontologyIndex`, this feels R-native with `$` accessors:

``` r
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

``` r
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

| Term            | Meaning                                                           |
|-----------------|-------------------------------------------------------------------|
| **Object Type** | A logical entity (e.g., “Encounter”) mapped to a physical table   |
| **Concept**     | A named definition (e.g., “ready_for_discharge”)                  |
| **Version**     | A specific SQL expression for a concept, scoped and numbered      |
| **Scope**       | Context for a definition (e.g., “flow”, “clinical”, “regulatory”) |
| **Audit**       | A human judgment comparing system evaluation to ground truth      |
| **Drift**       | When disagreement rate exceeds threshold                          |

## API Reference

### Top-level Entry Points

| Function                                                                               | Description                                                   |
|----------------------------------------------------------------------------------------|---------------------------------------------------------------|
| [`ontology()`](https://cathalbyrnegit.github.io/ontologyR/reference/ontology.md)       | **Recommended**: Create an ontology object with `$` accessors |
| [`ont_connect()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_connect.md) | Connect using function-based API                              |

### Object-based API (`ont$...`)

``` r
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

- [`ont_connect()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_connect.md)
  /
  [`ont_disconnect()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_disconnect.md)
  — Manage database connection
- [`ont_status()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_status.md)
  — View connection and content summary

### Objects & Links

- [`ont_register_object()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_register_object.md)
  — Map logical type to table
- [`ont_register_link()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_register_link.md)
  — Define relationships
- [`ont_query_objects()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_query_objects.md)
  /
  [`ont_query_linked()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_query_linked.md)
  — Query data

### Concepts

- [`ont_define_concept()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_define_concept.md)
  — Create a new concept
- [`ont_add_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_add_version.md)
  — Add versioned definition
- [`ont_activate_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_activate_version.md)
  /
  [`ont_deprecate_version()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_deprecate_version.md)
  — Manage lifecycle
- [`ont_evaluate()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_evaluate.md)
  — Apply concept to data
- [`ont_compare_versions()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_compare_versions.md)
  — Compare definition outputs

### Auditing

- [`ont_sample_for_audit()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_sample_for_audit.md)
  — Get random sample for review
- [`ont_record_audit()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_record_audit.md)
  — Record human judgment
- [`ont_audit_summary()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_audit_summary.md)
  — View audit statistics
- [`ont_rolling_disagreement()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_rolling_disagreement.md)
  — Track disagreement over time

### Drift Detection

- [`ont_check_drift()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_check_drift.md)
  — Test for drift
- [`ont_detect_drift()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_detect_drift.md)
  — Check and create drift event
- [`ont_list_drift_events()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_list_drift_events.md)
  — View drift history
- [`ont_resolve_drift()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_resolve_drift.md)
  — Close drift event
- [`ont_drift_status()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_drift_status.md)
  — Overview across all concepts

### Governance

- [`ont_log_governance()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_log_governance.md)
  — Record governance action
- [`ont_get_governance_log()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_get_governance_log.md)
  — View action history
- [`ont_check_policy()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_check_policy.md)
  — Validate proposed action
- [`ont_deprecation_impact()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_deprecation_impact.md)
  — Analyze change impact
- [`ont_governance_report()`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_governance_report.md)
  — Generate status report

## Design Principles

1.  **Definitions are hypotheses** — test them, don’t assume they’re
    right
2.  **Disagreement is evidence** — audit mismatches are signal, not
    noise
3.  **Drift is measurable** — track disagreement rates systematically
4.  **Governance follows evidence** — block actions on drifting
    definitions
5.  **Coexistence over replacement** — multiple versions can be active

## Learn More

- **New to ontologies?** Start with
  [`vignette("what-is-an-ontology")`](https://cathalbyrnegit.github.io/ontologyR/articles/what-is-an-ontology.md)
  — a plain-English guide with bakery analogies
- [`vignette("introduction")`](https://cathalbyrnegit.github.io/ontologyR/articles/introduction.md)
  — Code examples and API walkthrough
- `vignette("drift-detection")` — Details on detecting definition
  problems
- `vignette("governance")` — Managing definition changes

## Design Notes: Tables vs. Classes

ontologyR uses **tables as the source of truth** but provides **S3
classes for ergonomics**.

### Why tables (not named lists like ontologyIndex)?

| Our problem                         | Their problem              |
|-------------------------------------|----------------------------|
| Definitions change over time        | Ontologies are static      |
| Need versioning + audit trails      | Just hierarchy traversal   |
| “Is this definition still valid?”   | “Is term A ancestor of B?” |
| Scale unknown (could be enterprise) | Fits in memory             |

Tables give us: - Natural versioning (just add rows) - SQL evaluation
(definitions *are* SQL) - Integration with data lakes (csolake) - Audit
queries for free

### Why S3 accessors on top?

Interactive R users expect `ont$concepts$foo`, not
`ont_get_concept(con, "foo")`.

The hybrid approach: - Database = source of truth - S3 objects = cached
views with nice printing - Both APIs use the same tables

### When to use which

| Situation               | Recommendation                                                                                            |
|-------------------------|-----------------------------------------------------------------------------------------------------------|
| Interactive exploration | `ont <- ontology(); ont$concepts`                                                                         |
| Scripts and packages    | [`ont_connect(); ont_evaluate(...)`](https://cathalbyrnegit.github.io/ontologyR/reference/ont_connect.md) |
| Building pipelines      | Function-based (explicit control)                                                                         |
| Teaching / demos        | Object-based (less boilerplate)                                                                           |

## Related Work

### Comparison with ontologyIndex

[ontologyIndex](https://cran.r-project.org/package=ontologyIndex) is
excellent for **bio-ontologies** (HPO, GO). Here’s how we differ:

``` r
# ontologyIndex: static hierarchy from OBO file
data(hpo)
hpo$name[["HP:0001250"]]          # "Seizure"
get_ancestors(hpo, "HP:0001250")  # Returns parent terms

# ontologyR: versioned definitions with governance
ont <- ontology("metrics.duckdb")
ont$concepts$ready_for_discharge$flow[[1]]$sql_expr  # "NOT planned_intervention"
ont$concepts$ready_for_discharge$flow[[1]]$audit_summary  # Drift stats
```

|                | ontologyIndex       | ontologyR                        |
|----------------|---------------------|----------------------------------|
| Domain         | Bio-ontologies      | Operational definitions          |
| Data structure | Named lists         | Tables + S3 views                |
| Mutability     | Read-only           | Versioned, auditable             |
| Key operation  | Hierarchy traversal | SQL evaluation + drift detection |
| Persistence    | In-memory           | Database-backed                  |

**Use ontologyIndex for:** Gene Ontology, HPO, disease taxonomies

**Use ontologyR for:** KPIs, metrics, business definitions that need
governance

### Other influences

- **Goodhart’s Law** — “When a measure becomes a target, it ceases to be
  a good measure”
- **Statistical process control** — Treating drift as a measurable
  signal
- **Data mesh** — Decentralized ownership with standards
- Critical analysis of analytics platforms and definition lock-in

## License

MIT
