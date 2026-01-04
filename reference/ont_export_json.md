# Export Ontology to JSON

Exports the complete ontology (types, concepts, versions) to a JSON file
for backup, transfer, or documentation purposes.

## Usage

``` r
ont_export_json(
  path,
  include_audits = FALSE,
  include_governance = FALSE,
  con = NULL
)
```

## Arguments

- path:

  Character. Path to output JSON file.

- include_audits:

  Logical. Include audit records? Default FALSE.

- include_governance:

  Logical. Include governance log? Default FALSE.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the path.
