# Connect to an Ontology Database

Creates or connects to an ontology database. The database stores all
ontology metadata: object types, link types, concepts, versions, audits,
and governance logs.

## Usage

``` r
ont_connect(path = "ontology.duckdb", read_only = FALSE, init = TRUE)
```

## Arguments

- path:

  Path to DuckDB database file. Use `":memory:"` for an in-memory
  database (useful for testing). Default creates a file called
  `"ontology.duckdb"` in the current working directory.

- read_only:

  Logical. If `TRUE`, connect in read-only mode.

- init:

  Logical. If `TRUE` (default), initialize schema if tables don't exist.

## Value

Invisibly returns the connection object. The connection is also stored
internally for use by other `ont_*` functions.

## Examples

``` r
# In-memory database for testing
ont_connect(":memory:")
#> ! Schema file parsing failed, using inline creation: Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu': Extension "/home/runner/.local/share/R/duckdb/extensions/v1.4.3/linux_amd64/icu.duckdb_extension" not found. Extension "icu" is an existing extension.  Install it first using "INSTALL icu". ℹ Context: rapi_prepare ℹ Error type: AUTOLOAD
#> ✔ Connected to ontology database: ':memory:'

# Persistent database
ont_connect("my_ontology.duckdb")
#> Warning: Existing connection found. Disconnecting first.
#> ✔ Disconnected from ontology database.
#> ! Schema file parsing failed, using inline creation: Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu': Extension "/home/runner/.local/share/R/duckdb/extensions/v1.4.3/linux_amd64/icu.duckdb_extension" not found. Extension "icu" is an existing extension.  Install it first using "INSTALL icu". ℹ Context: rapi_prepare ℹ Error type: AUTOLOAD
#> ✔ Connected to ontology database: my_ontology.duckdb

# Check connection status
ont_status()
#> 
#> ── Ontology Database Status ──
#> 
#> ℹ Path: my_ontology.duckdb
#> ℹ Connected: [2026-01-16 10:50:42.960114]
#> ℹ Read-only: FALSE
#> 
#> ── Contents 
#> • Object types: 0
#> • Link types: 0
#> • Concepts: 0
#> • Concept versions: 0
#> • Audits: 0
#> • Drift events: 0

# Disconnect when done
ont_disconnect()
#> ✔ Disconnected from ontology database.
```
