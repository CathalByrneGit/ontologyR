# Disconnect from Ontology Database

Closes the connection to the ontology database and cleans up resources.

## Usage

``` r
ont_disconnect(shutdown = TRUE)
```

## Arguments

- shutdown:

  Logical. If `TRUE` (default), fully shut down the DuckDB instance. Set
  to `FALSE` if you want to reconnect quickly.

## Value

Invisibly returns `TRUE` if disconnection was successful.
