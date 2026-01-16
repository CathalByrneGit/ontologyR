# Define a Cohort

Creates a named group of objects for comparative analysis. Cohorts can
be defined dynamically via SQL expression or explicitly by listing
members.

## Usage

``` r
ont_define_cohort(
  cohort_id,
  cohort_name,
  object_type,
  sql_expr = NULL,
  description = NULL,
  created_by = NULL,
  con = NULL
)
```

## Arguments

- cohort_id:

  Character. Unique identifier for the cohort.

- cohort_name:

  Character. Human-readable name.

- object_type:

  Character. The object type for this cohort.

- sql_expr:

  Character. SQL WHERE clause defining cohort membership (for dynamic
  cohorts).

- description:

  Character. Optional description.

- created_by:

  Character. Optional creator identifier.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the cohort_id.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a cohort of ICU patients
ont_define_cohort(
  cohort_id = "icu_patients",
  cohort_name = "ICU Patients",
  object_type = "Encounter",
  sql_expr = "ward = 'ICU'"
)
} # }
```
