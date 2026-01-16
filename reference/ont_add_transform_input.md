# Add Transform Input

Registers an input dataset for a transform.

## Usage

``` r
ont_add_transform_input(
  transform_id,
  input_dataset_id,
  input_role = "primary",
  con = NULL
)
```

## Arguments

- transform_id:

  The transform identifier.

- input_dataset_id:

  The input dataset ID.

- input_role:

  Role of this input: "primary", "join", "lookup", "filter".

- con:

  Optional DBI connection.

## Value

Invisibly returns TRUE.
