# Analyze Impact of Deprecation

Shows what dashboards/reports would be affected if a concept version is
deprecated.

## Usage

``` r
ont_deprecation_impact(concept_id, scope, version, con = NULL)
```

## Arguments

- concept_id:

  Character. The concept.

- scope:

  Character. The scope.

- version:

  Integer. The version to analyze.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A list with impact analysis.
