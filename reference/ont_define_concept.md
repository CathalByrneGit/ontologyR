# Define a New Concept

Creates a new concept that can have multiple versioned definitions. A
concept represents a meaningful category or classification (e.g.,
"ready_for_discharge", "blocked_bed") that can be evaluated against
objects of a specific type.

## Usage

``` r
ont_define_concept(
  concept_id,
  object_type,
  description = NULL,
  owner_domain = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. Unique identifier for the concept.

- object_type:

  Character. The object type this concept applies to. Must be a
  registered object type.

- description:

  Character. Human-readable description of what this concept represents.

- owner_domain:

  Character. Optional domain/team that owns this concept.

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

# Define a concept
ont_define_concept(
    concept_id = "ready_for_discharge",
    object_type = "Encounter",
    description = "Patient is clinically ready to leave hospital",
    owner_domain = "patient_flow"
)
#> Error in ont_get_object(object_type, con): Unknown object type: "Encounter"

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
