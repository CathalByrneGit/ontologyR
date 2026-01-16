# Sample Objects for Audit

Selects a random sample of objects that meet a concept definition for
human review. This is the foundation of drift detection: by regularly
auditing samples, you can detect when definitions diverge from reality.

## Usage

``` r
ont_sample_for_audit(
  concept_id,
  scope,
  version = NULL,
  n = 20,
  where = NULL,
  concept_value = TRUE,
  seed = NULL,
  con = NULL
)
```

## Arguments

- concept_id:

  Character. The concept to sample from.

- scope:

  Character. The scope.

- version:

  Integer. The version. If `NULL`, uses active version.

- n:

  Integer. Number of objects to sample.

- where:

  Character. Optional SQL WHERE clause to filter the sample population
  (e.g., only sample from today's cases).

- concept_value:

  Logical. If `TRUE` (default), only sample objects where the concept
  evaluates to TRUE. Set to `FALSE` to sample regardless of concept
  value, or `NA` to sample where concept is FALSE.

- seed:

  Integer. Random seed for reproducibility.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

A tibble of sampled objects with their concept values.

## Examples

``` r
if (FALSE) { # \dontrun{
ont_connect(":memory:")
# ... setup ...

# Sample 20 cases that the system says are "ready for discharge"
sample <- ont_sample_for_audit(
    concept_id = "ready_for_discharge",
    scope = "flow",
    n = 20,
    concept_value = TRUE
)
} # }
```
