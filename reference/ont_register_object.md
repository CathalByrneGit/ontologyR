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
if (FALSE) { # \dontrun{
ont_connect(":memory:")

# Register an Encounter object type
ont_register_object(
    object_type = "Encounter",
    table_name = "encounters",
    pk_column = "encounter_id",
    description = "A patient encounter or visit",
    owner_domain = "patient_flow"
)

# List registered types
ont_list_objects()

ont_disconnect()
} # }
```
