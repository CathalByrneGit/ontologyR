# Add Members to a Cohort

Adds explicit members to a cohort (for explicit cohorts).

## Usage

``` r
ont_add_cohort_members(cohort_id, object_keys, added_by = NULL, con = NULL)
```

## Arguments

- cohort_id:

  Character. The cohort ID.

- object_keys:

  Character vector. Primary keys of objects to add.

- added_by:

  Character. Optional identifier of who added these.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the number of members added.
