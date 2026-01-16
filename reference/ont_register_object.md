# Register an Object Type

Maps a logical object type name (e.g., "Encounter", "Patient") to its
physical storage: the table name and primary key column. This mapping
allows concepts to be defined against logical types without hardcoding
table names.

## Usage

``` r
ont_register_object(
  object_type,
  table_name,
  pk_column,
  description = NULL,
  owner_domain = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- object_type:

  Character. The logical name for this object type.

- table_name:

  Character. The physical table name in the database.

- pk_column:

  Character. The primary key column name.

- description:

  Character. Optional description of this object type.

- owner_domain:

  Character. Optional domain/team that owns this type.

- created_by:

  Character. Optional identifier for who created this.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE` on success.

## Examples

``` r
ont_connect(":memory:")
#> ! Schema file parsing failed, using inline creation: Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu': Extension "/home/runner/.local/share/R/duckdb/extensions/v1.4.3/linux_amd64/icu.duckdb_extension" not found. Extension "icu" is an existing extension.  Install it first using "INSTALL icu". ℹ Context: rapi_prepare ℹ Error type: AUTOLOAD
#> ✔ Connected to ontology database: ':memory:'

# Register an Encounter object type
ont_register_object(
    object_type = "Encounter",
    table_name = "encounters",
    pk_column = "encounter_id",
    description = "A patient encounter or visit",
    owner_domain = "patient_flow"
)
#> ✔ Registered object type "Encounter" -> "encounters"

# List registered types
ont_list_objects()
#> # A tibble: 1 × 7
#>   object_type table_name pk_column  description owner_domain created_at         
#>   <chr>       <chr>      <chr>      <chr>       <chr>        <dttm>             
#> 1 Encounter   encounters encounter… A patient … patient_flow 2026-01-16 01:00:44
#> # ℹ 1 more variable: created_by <chr>

ont_disconnect()
#> ✔ Disconnected from ontology database.
```
