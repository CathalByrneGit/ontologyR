# Evaluate a Concept

Evaluates a concept version against its object type table, returning the
original data plus a `concept_value` column with the evaluation result.

## Usage

``` r
ont_evaluate(
  concept_id,
  scope,
  version = NULL,
  filter_expr = NULL,
  collect = TRUE,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to evaluate.

- scope:

  Character. The scope.

- version:

  Integer. The version. If `NULL`, uses the active version.

- filter_expr:

  Character. Optional SQL WHERE clause to filter objects before
  evaluation.

- collect:

  Logical. If `TRUE` (default), collect results. If `FALSE`, return lazy
  tbl.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble (or lazy tbl) with original columns plus `concept_value`. Has
"ontology_provenance" attribute with evaluation metadata.

## Details

The result includes provenance metadata as attributes.

## Examples

``` r
ont_connect(":memory:")
#> ! Schema file parsing failed, using inline creation: Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu': Extension "/home/runner/.local/share/R/duckdb/extensions/v1.4.3/linux_amd64/icu.duckdb_extension" not found. Extension "icu" is an existing extension.  Install it first using "INSTALL icu". ℹ Context: rapi_prepare ℹ Error type: AUTOLOAD
#> ✔ Connected to ontology database: ':memory:'

# Setup
DBI::dbWriteTable(ont_get_connection(), "encounters", tibble::tibble(
    encounter_id = c("E1", "E2", "E3"),
    planned_intervention_24h = c(TRUE, FALSE, FALSE),
    arrangements_confirmed = c(FALSE, TRUE, FALSE)
))
ont_register_object("Encounter", "encounters", "encounter_id")
#> Error in .local(conn, statement, ...): Bind parameter values need to have the same length
#> ℹ Context: rapi_bind
ont_define_concept("ready_for_discharge", "Encounter")
#> Error in ont_get_object(object_type, con): Unknown object type: "Encounter"
ont_add_version("ready_for_discharge", "flow", 1,
    sql_expr = "NOT planned_intervention_24h",
    status = "active")
#> Error in ont_get_concept(concept_id, con): Unknown concept: "ready_for_discharge"

# Evaluate
result <- ont_evaluate("ready_for_discharge", "flow", 1)
#> Error in ont_get_version(concept_id, scope, version, con): Unknown version: "ready_for_discharge"@flow v1
print(result)
#> Error: object 'result' not found

# Check provenance
attr(result, "ontology_provenance")
#> Error: object 'result' not found

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
