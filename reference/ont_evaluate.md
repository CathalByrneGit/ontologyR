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
#> ✔ Registered object type "Encounter" -> "encounters"
ont_define_concept("ready_for_discharge", "Encounter")
#> ✔ Defined concept "ready_for_discharge" for object type "Encounter"
ont_add_version("ready_for_discharge", "flow", 1,
    sql_expr = "NOT planned_intervention_24h",
    status = "active")
#> ✔ Added version 1 for "ready_for_discharge"@flow [active]

# Evaluate
result <- ont_evaluate("ready_for_discharge", "flow", 1)
print(result)
#> # A tibble: 3 × 4
#>   encounter_id planned_intervention_24h arrangements_confirmed concept_value
#>   <chr>        <lgl>                    <lgl>                  <lgl>        
#> 1 E1           TRUE                     FALSE                  FALSE        
#> 2 E2           FALSE                    TRUE                   TRUE         
#> 3 E3           FALSE                    FALSE                  TRUE         

# Check provenance
attr(result, "ontology_provenance")
#> $concept_id
#> [1] "ready_for_discharge"
#> 
#> $scope
#> [1] "flow"
#> 
#> $version
#> [1] 1
#> 
#> $sql_expr
#> [1] "NOT planned_intervention_24h"
#> 
#> $status
#> [1] "active"
#> 
#> $object_type
#> [1] "Encounter"
#> 
#> $pk_column
#> [1] "encounter_id"
#> 
#> $table_name
#> [1] "encounters"
#> 
#> $evaluated_at
#> [1] "2026-01-15 19:50:20 UTC"
#> 

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
