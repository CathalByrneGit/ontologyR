# Get Gate Check History

Returns the history of gate checks for a concept version.

## Usage

``` r
ont_get_gate_history(concept_id, scope, version, con = NULL)
```

## Arguments

- concept_id:

  The concept.

- scope:

  The scope.

- version:

  The version.

- con:

  Optional DBI connection.

## Value

A tibble of gate checks.
