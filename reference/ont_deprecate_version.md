# Deprecate a Concept Version

Marks a concept version as deprecated. Checks for open drift events that
might block deprecation.

## Usage

``` r
ont_deprecate_version(
  concept_id,
  scope,
  version,
  deprecated_by,
  rationale = NULL,
  force = FALSE,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept ID.

- scope:

  Character. The scope.

- version:

  Integer. The version to deprecate.

- deprecated_by:

  Character. Who is deprecating this version.

- rationale:

  Character. Reason for deprecation.

- force:

  Logical. If `TRUE`, deprecate even with open drift events.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns `TRUE` on success.
