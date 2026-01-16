# Cohort Comparison

Compares concept prevalence across different cohorts.

## Usage

``` r
ont_cohort_compare(concept_id, scope, version = NULL, cohort_ids, con = NULL)
```

## Arguments

- concept_id:

  Character. The concept to analyze.

- scope:

  Character. The scope.

- version:

  Integer. The version.

- cohort_ids:

  Character vector. Cohort IDs to compare.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble with per-cohort prevalence statistics.
