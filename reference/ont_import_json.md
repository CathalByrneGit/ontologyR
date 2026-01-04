# Import Ontology from JSON

Imports ontology definitions from a JSON file.

## Usage

``` r
ont_import_json(path, overwrite = FALSE, con = NULL)
```

## Arguments

- path:

  Character. Path to JSON file.

- overwrite:

  Logical. Overwrite existing definitions? Default FALSE.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns TRUE.
