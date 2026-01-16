# Create an Ontology Object

Returns an `ontology` S3 object that provides convenient access to
concepts, versions, and evaluations while keeping the database as source
of truth.

## Usage

``` r
ontology(path = "ontology.duckdb", read_only = FALSE)
```

## Arguments

- path:

  Path to DuckDB database, or ":memory:" for in-memory.

- read_only:

  Logical. Connect in read-only mode?

## Value

An object of class `ontology`.

## Details

This is the recommended way to work with ontologyR interactively.

## Examples

``` r
if (FALSE) { # \dontrun{
ont <- ontology(":memory:")

# Register and define concepts
ont$register_object("Encounter", "encounters", "encounter_id")
ont$define_concept("ready_for_discharge", "Encounter")
ont$add_version("ready_for_discharge", "flow", 1, "NOT planned_intervention")

# Access like ontologyIndex
ont$concepts
ont$concepts$ready_for_discharge
ont$concepts$ready_for_discharge$versions

# Evaluate
ont$evaluate("ready_for_discharge", "flow")

# Audit
ont$sample("ready_for_discharge", "flow", n = 20)
ont$record_audit("ready_for_discharge", "flow", 1, "E1", TRUE, FALSE, "reviewer1")

# Check drift
ont$drift_status()
} # }
```
