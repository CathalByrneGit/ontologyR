# List Analysis Runs

Retrieves analysis execution history.

## Usage

``` r
ont_list_analyses(concept_id = NULL, analysis_type = NULL, con = NULL)
```

## Arguments

- concept_id:

  Character. Optional filter by concept.

- analysis_type:

  Character. Optional filter by type.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of analysis runs.
