# Get Active Version

Returns the currently active version for a concept/scope combination.

## Usage

``` r
ont_get_active_version(concept_id, scope, as_of = Sys.Date(), con = NULL)
```

## Arguments

- concept_id:

  Character. The concept ID.

- scope:

  Character. The scope.

- as_of:

  Date. Optional date for temporal validity. Defaults to today.

- con:

  A DBI connection.

## Value

A single-row tibble, or error if no active version found.
