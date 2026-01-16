# Define a Transform

Creates a transform definition that specifies how an output dataset is
produced. Transforms can be SQL queries or references to R functions.

## Usage

``` r
ont_define_transform(
  transform_id,
  transform_name,
  output_dataset_id,
  transform_type = c("sql", "r_function", "concept_eval"),
  code = NULL,
  input_datasets = NULL,
  description = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- transform_id:

  Unique identifier for the transform.

- transform_name:

  Human-readable name.

- output_dataset_id:

  The dataset this transform produces.

- transform_type:

  Type: "sql", "r_function", or "concept_eval".

- code:

  SQL query or R function name.

- input_datasets:

  Character vector of input dataset IDs.

- description:

  Optional description.

- created_by:

  Optional user.

- con:

  Optional DBI connection.

## Value

The transform_id (invisibly).
