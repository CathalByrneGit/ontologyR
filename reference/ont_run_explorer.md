# Run Ontology Explorer Shiny App

Launches an interactive Shiny application for exploring concepts,
versions, templates, audits, drift events, and governance information.

## Usage

``` r
ont_run_explorer(db_path = NULL, launch.browser = TRUE)
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

The Ontology Explorer provides:

- Browse and filter concepts by object type, status, and scope

- Compare different versions of a concept

- Search SQL expressions across all concepts

- Explore templates and compare variants

- View audits and drift summaries

- Review governance logs and pending approvals

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch with default settings
ont_run_explorer()

# Launch pointing to a specific database
ont_run_explorer(db_path = "my_ontology.duckdb")
} # }
```
