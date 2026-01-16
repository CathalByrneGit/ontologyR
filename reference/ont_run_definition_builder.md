# Run Definition Builder Shiny App

Launches an interactive Shiny application for building concept
definitions visually without writing SQL. Designed for non-technical
users.

## Usage

``` r
ont_run_definition_builder(db_path = NULL, launch.browser = TRUE)
```

## Arguments

- db_path:

  Character. Path to the DuckDB database file. If NULL, the app will
  prompt for connection details.

- launch.browser:

  Logical. Whether to open the app in a browser. Default TRUE.

## Value

This function runs the Shiny app and does not return a value.

## Details

The Definition Builder provides:

- Visual condition builder with column selection and operators

- Real-time SQL preview

- Test definitions against actual data

- Save concepts directly to the database

- Template support for creating variants

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch with default settings
ont_run_definition_builder()

# Launch pointing to a specific database
ont_run_definition_builder(db_path = "my_ontology.duckdb")
} # }
```
