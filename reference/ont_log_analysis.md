# Log Analysis Run

Records an analysis execution for reproducibility and audit trail.

## Usage

``` r
ont_log_analysis(
  analysis_type,
  concept_id,
  scope = NULL,
  parameters = NULL,
  results_summary = NULL,
  executed_by = NULL,
  con = NULL
)
```

## Arguments

- analysis_type:

  Character. Type of analysis.

- concept_id:

  Character. The concept analyzed.

- scope:

  Character. The scope (if applicable).

- parameters:

  List. Analysis parameters (will be converted to JSON).

- results_summary:

  List. Key findings (will be converted to JSON).

- executed_by:

  Character. Who ran the analysis.

- con:

  A DBI connection. If `NULL`, uses the active connection.

## Value

Invisibly returns the analysis_id.
