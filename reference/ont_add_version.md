# Add a Concept Version

Adds a new versioned definition for an existing concept. Each version is
scoped (e.g., "flow", "clinical", "regulatory") and contains a SQL
expression that evaluates to TRUE/FALSE or a numeric value.

## Usage

``` r
ont_add_version(
  concept_id,
  scope,
  version,
  sql_expr,
  status = "draft",
  rationale = NULL,
  valid_from = NULL,
  valid_to = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to add a version to.

- scope:

  Character. The scope/context for this version (e.g., "flow",
  "clinical", "regulatory"). Different scopes can have different
  definitions.

- version:

  Integer. Version number. Should be monotonically increasing within a
  concept/scope combination.

- sql_expr:

  Character. SQL expression that evaluates to BOOLEAN or numeric. Can
  reference columns from the concept's object type table.

- status:

  Character. Initial status: "draft" (default), "active", "deprecated",
  or "retired".

- rationale:

  Character. Explanation of why this version exists or how it differs
  from previous versions.

- valid_from:

  Date. When this version becomes applicable. If `NULL`, applicable
  immediately.

- valid_to:

  Date. When this version stops being applicable. If `NULL`, no end
  date.

- created_by:

  Character. Optional creator identifier.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE` on success.

## Examples

``` r
ont_connect(":memory:")
#> ! Schema file parsing failed, using inline creation: Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu': Extension "/home/runner/.local/share/R/duckdb/extensions/v1.4.3/linux_amd64/icu.duckdb_extension" not found. Extension "icu" is an existing extension.  Install it first using "INSTALL icu". ℹ Context: rapi_prepare ℹ Error type: AUTOLOAD
#> ✔ Connected to ontology database: ':memory:'
ont_register_object("Encounter", "encounters", "encounter_id")
#> Error in .local(conn, statement, ...): Bind parameter values need to have the same length
#> ℹ Context: rapi_bind
ont_define_concept("ready_for_discharge", "Encounter")
#> Error in ont_get_object(object_type, con): Unknown object type: "Encounter"

# Add version 1: simple proxy definition
ont_add_version(
    concept_id = "ready_for_discharge",
    scope = "flow",
    version = 1,
    sql_expr = "NOT planned_intervention_24h",
    status = "active",
    rationale = "Operational proxy: no planned interventions in next 24h"
)
#> Error in ont_get_concept(concept_id, con): Unknown concept: "ready_for_discharge"

# Add version 2: refined definition
ont_add_version(
    concept_id = "ready_for_discharge",
    scope = "flow",
    version = 2,
    sql_expr = "NOT planned_intervention_24h AND arrangements_confirmed",
    status = "draft",
    rationale = "Added arrangements requirement based on audit findings"
)
#> Error in ont_get_concept(concept_id, con): Unknown concept: "ready_for_discharge"

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
