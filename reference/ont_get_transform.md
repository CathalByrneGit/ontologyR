# Get Transform Details

Retrieves a transform definition with its inputs.

## Usage

``` r
ont_get_transform(transform_id, con = NULL)
```

## Arguments

- transform_id:

  The transform identifier.

- con:

  Optional DBI connection.

## Value

A list with transform details and inputs, or NULL if not found.
