# Register a Link Type

Defines a relationship between two object types. Links are stored in a
separate table and can optionally have temporal validity columns.

## Usage

``` r
ont_register_link(
  link_type,
  from_object,
  to_object,
  link_table,
  from_key,
  to_key,
  valid_from_col = NULL,
  valid_to_col = NULL,
  cardinality = "many-to-many",
  description = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- link_type:

  Character. Name for this link type.

- from_object:

  Character. Source object type.

- to_object:

  Character. Target object type.

- link_table:

  Character. Physical table containing the link records.

- from_key:

  Character. Column in link_table referencing from_object.

- to_key:

  Character. Column in link_table referencing to_object.

- valid_from_col:

  Character. Optional column for temporal validity start.

- valid_to_col:

  Character. Optional column for temporal validity end.

- cardinality:

  Character. Relationship cardinality: "one-to-one", "one-to-many", or
  "many-to-many" (default).

- description:

  Character. Optional description.

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

# Register object types first
ont_register_object("Encounter", "encounters", "encounter_id")
#> Error in .local(conn, statement, ...): Bind parameter values need to have the same length
#> ℹ Context: rapi_bind
ont_register_object("Patient", "patients", "patient_id")
#> Error in .local(conn, statement, ...): Bind parameter values need to have the same length
#> ℹ Context: rapi_bind

# Register link between them
ont_register_link(
    link_type = "encounter_patient",
    from_object = "Encounter",
    to_object = "Patient",
    link_table = "encounters",  # Link lives in encounters table
    from_key = "encounter_id",
    to_key = "patient_id",
    cardinality = "many-to-one"
)
#> Error in ont_get_object(from_object, con): Unknown object type: "Encounter"

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
