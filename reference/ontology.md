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
