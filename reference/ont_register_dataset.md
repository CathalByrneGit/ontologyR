# Register a Dataset

Registers a dataset (table) in the ontology registry. Datasets can be
source tables, materialized concept outputs, or transform outputs.

## Usage

``` r
ont_register_dataset(
  dataset_id,
  dataset_name,
  physical_name,
  dataset_type = c("source", "materialized", "transform"),
  object_type = NULL,
  description = NULL,
  schema_json = NULL,
  owner = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- dataset_id:

  Unique identifier for the dataset.

- dataset_name:

  Human-readable name.

- physical_name:

  The actual table/view name in the database.

- dataset_type:

  Type of dataset: "source", "materialized", or "transform".

- object_type:

  Optional object type this dataset represents.

- description:

  Optional description.

- schema_json:

  Optional JSON string describing the schema.

- owner:

  Optional owner/domain.

- created_by:

  Optional user who registered the dataset.

- con:

  Optional DBI connection. Uses active connection if NULL.

## Value

The dataset_id (invisibly).
