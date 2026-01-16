# Run Lineage Visualizer Shiny App

Launches an interactive Shiny application for visualizing data lineage,
datasets, transforms, and their relationships in a directed acyclic
graph.

## Usage

``` r
ont_run_lineage_viewer(db_path = NULL, launch.browser = TRUE)
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

The Lineage Visualizer provides:

- Interactive DAG visualization of datasets and transforms

- Upstream/downstream dependency exploration

- Searchable tables for datasets, transforms, and runs

- Impact analysis to understand change propagation

Node types are distinguished by shape and color:

- Source datasets: Gray database icons

- Materialized datasets: Green diamonds

- Derived datasets: Blue boxes

- Transforms: Yellow squares

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch with default settings
ont_run_lineage_viewer()

# Launch pointing to a specific database
ont_run_lineage_viewer(db_path = "my_ontology.duckdb")
} # }
```
