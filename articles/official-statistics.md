# Official Statistics: Governing Definitions That Matter

## Why Official Statistics Need Governed Definitions

When a national statistics office reports that “unemployment fell to
4.2%”, that single number carries enormous weight. Policy decisions,
budget allocations, and political debates all hinge on it.

But what does “unemployed” actually mean?

- Must someone be actively seeking work?
- What counts as “actively seeking”?
- Are discouraged workers included?
- What about gig economy workers with irregular hours?

Different answers to these questions produce different unemployment
rates — potentially by several percentage points. **The definition IS
the statistic.**

This vignette shows how ontologyR helps statistical agencies:

1.  **Document definitions explicitly** — no more buried footnotes
2.  **Version definitions over time** — track methodological changes
3.  **Test definitions against reality** — catch when proxies drift
4.  **Govern changes properly** — require approval before publishing
5.  **Maintain international comparability** — manage multiple scopes

------------------------------------------------------------------------

## Scenario: Labour Force Statistics

You’re the methodology team at a national statistics office. You need to
manage definitions for the Labour Force Survey (LFS), ensuring:

- Consistency with ILO international standards
- Adaptation for national context
- Historical comparability
- Quality assurance before publication

Let’s build this system.

------------------------------------------------------------------------

## Phase 1: Setting Up the Survey Infrastructure

``` r
library(ontologyR)

# Connect to the statistics office ontology database
ont_connect("labour_force_ontology.duckdb")

# In reality, this would be your survey microdata warehouse
# Here we simulate LFS respondent data
DBI::dbWriteTable(ont_get_connection(), "lfs_respondents", tibble::tibble(
  respondent_id = paste0("R", 1:10000),
  age = sample(16:75, 10000, replace = TRUE),
  worked_last_week = sample(c(TRUE, FALSE), 10000, replace = TRUE, prob = c(0.6, 0.4)),
  hours_worked = ifelse(worked_last_week, sample(1:50, 10000, replace = TRUE), 0),
  has_job_attached = sample(c(TRUE, FALSE), 10000, replace = TRUE, prob = c(0.7, 0.3)),
  sought_work_4weeks = sample(c(TRUE, FALSE), 10000, replace = TRUE, prob = c(0.3, 0.7)),
  available_within_2weeks = sample(c(TRUE, FALSE), 10000, replace = TRUE, prob = c(0.8, 0.2)),
  reason_not_seeking = sample(c("discouraged", "family", "student", "retired", "other", NA),
                               10000, replace = TRUE),
  education_level = sample(c("primary", "secondary", "tertiary"), 10000, replace = TRUE),
  region = sample(c("Capital", "North", "South", "East", "West"), 10000, replace = TRUE),
  survey_wave = rep(c("2024Q1", "2024Q2", "2024Q3", "2024Q4"), each = 2500)
))

# Register source dataset
ont_register_dataset(
  dataset_id = "ds_lfs_respondents",
  dataset_name = "Labour Force Survey Respondents",
  physical_name = "lfs_respondents",
  dataset_type = "source",
  owner = "social_statistics_division",
  description = "Quarterly LFS microdata, respondent level"
)

# Register the object type
ont_register_object(
  object_type = "LFS_Respondent",
  table_name = "lfs_respondents",
  pk_column = "respondent_id",
  description = "Individual respondent in the Labour Force Survey",
  owner_domain = "labour_statistics"
)
```

------------------------------------------------------------------------

## Phase 2: Defining ILO-Compliant Concepts

The International Labour Organization (ILO) provides standard
definitions. We’ll implement these as our baseline.

### Employment Definition

``` r
# Define the employment concept
ont_define_concept(
  concept_id = "employed",
  object_type = "LFS_Respondent",
  description = "Person is employed according to ILO definition",
  owner_domain = "labour_statistics"
)

# ILO standard: Worked at least 1 hour in reference week, OR temporarily absent from job
ont_add_version(
  concept_id = "employed",
  scope = "ilo_standard",
  version = 1,
  sql_expr = "worked_last_week = TRUE OR (has_job_attached = TRUE AND worked_last_week = FALSE)",
  status = "active",
  rationale = "ILO 19th ICLS definition: worked 1+ hours OR temporarily absent with job attachment"
)

# National adaptation: Same core, but restricted to working age (16-64)
ont_add_version(
  concept_id = "employed",
  scope = "national_headline",
  version = 1,
  sql_expr = "(worked_last_week = TRUE OR has_job_attached = TRUE) AND age BETWEEN 16 AND 64",
  status = "active",
  rationale = "National headline rate uses 16-64 working age population"
)
```

### Unemployment Definition

This is where it gets interesting — unemployment has more nuance.

``` r
ont_define_concept(
  concept_id = "unemployed",
  object_type = "LFS_Respondent",
  description = "Person is unemployed according to standard definition",
  owner_domain = "labour_statistics"
)

# ILO standard unemployment (strict)
ont_add_version(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 1,
  sql_expr = "
    worked_last_week = FALSE
    AND has_job_attached = FALSE
    AND sought_work_4weeks = TRUE
    AND available_within_2weeks = TRUE
    AND age >= 16
  ",
  status = "active",
  rationale = "ILO 19th ICLS: without work, seeking work, available for work"
)

# Extended unemployment including discouraged workers
ont_add_version(
  concept_id = "unemployed",
  scope = "extended_measure",
  version = 1,
  sql_expr = "
    worked_last_week = FALSE
    AND has_job_attached = FALSE
    AND (
      (sought_work_4weeks = TRUE AND available_within_2weeks = TRUE)
      OR reason_not_seeking = 'discouraged'
    )
    AND age >= 16
  ",
  status = "active",
  rationale = "Extended measure includes discouraged workers per Eurostat LFS guidelines"
)
```

### Labour Force and Derived Rates

``` r
ont_define_concept(
  concept_id = "in_labour_force",
  object_type = "LFS_Respondent",
  description = "Person is in the labour force (employed or unemployed)",
  owner_domain = "labour_statistics"
)

ont_add_version(
  concept_id = "in_labour_force",
  scope = "ilo_standard",
  version = 1,
  sql_expr = "
    age >= 16 AND (
      worked_last_week = TRUE
      OR has_job_attached = TRUE
      OR (sought_work_4weeks = TRUE AND available_within_2weeks = TRUE)
    )
  ",
  status = "active",
  rationale = "Labour force = employed + unemployed"
)

# Underemployment (wants more hours)
ont_define_concept(
  concept_id = "underemployed",
  object_type = "LFS_Respondent",
  description = "Employed but working fewer hours than desired",
  owner_domain = "labour_statistics"
)

ont_add_version(
  concept_id = "underemployed",
  scope = "national_headline",
  version = 1,
  sql_expr = "
    worked_last_week = TRUE
    AND hours_worked < 35
    AND available_within_2weeks = TRUE
  ",
  status = "active",
  rationale = "Part-time workers available and wanting more hours"
)
```

------------------------------------------------------------------------

## Phase 3: Quality Assurance Through Auditing

Before publishing statistics, methodology teams should verify that
definitions capture what they intend.

### Sampling for Expert Review

``` r
# Sample respondents classified as unemployed for expert review
unemployed_sample <- ont_sample_for_audit(
  concept_id = "unemployed",
  scope = "ilo_standard",
  n = 50,
  concept_value = TRUE,
  seed = 42  # Reproducible sample
)

# Sample edge cases: not seeking but potentially discouraged
edge_cases <- ont_sample_for_audit(
  concept_id = "unemployed",
  scope = "ilo_standard",
  n = 30,
  where = "sought_work_4weeks = FALSE AND reason_not_seeking IS NOT NULL",
  concept_value = FALSE  # System says "not unemployed"
)
```

### Recording Expert Judgments

Labour statisticians review individual cases against the full
questionnaire.

``` r
# Expert reviews case R1234: System says unemployed, expert agrees
ont_record_audit(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 1,
  object_key = "R1234",
  system_value = TRUE,
  reviewer_value = TRUE,
  reviewer_id = "senior_methodologist",
  notes = "Clear case: actively seeking, sent 5 applications last week"
)

# Expert reviews case R5678: System says unemployed, but expert disagrees!
ont_record_audit(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 1,
  object_key = "R5678",
  system_value = TRUE,
  reviewer_value = FALSE,
  reviewer_id = "senior_methodologist",
  notes = "Respondent waiting to start job next month - should be employed (job attached)"
)

# Batch import from expert review spreadsheet
audit_results <- readr::read_csv("expert_review_q4_2024.csv")
for (i in seq_len(nrow(audit_results))) {
  ont_record_audit(
    concept_id = audit_results$concept[i],
    scope = audit_results$scope[i],
    version = audit_results$version[i],
    object_key = audit_results$respondent_id[i],
    system_value = audit_results$system_classification[i],
    reviewer_value = audit_results$expert_classification[i],
    reviewer_id = audit_results$reviewer[i],
    notes = audit_results$notes[i]
  )
}
```

### Checking Audit Results

``` r
# How well does our definition match expert judgment?
ont_audit_summary("unemployed", "ilo_standard", 1)
#> -- Audit Summary: unemployed@ilo_standard v1 --
#> i Total audits: 80
#> i Agreements: 72 (90%)
#> i Disagreements: 8 (10%)
#>
#> Most common disagreement reasons:
#>   - Job attachment edge cases: 5
#>   - Availability interpretation: 2
#>   - Seeking work definition: 1
```

------------------------------------------------------------------------

## Phase 4: Detecting and Managing Drift

Over time, definitions can drift from reality. New forms of work emerge,
survey questions evolve, or classifier behavior changes.

### Setting Up Drift Monitoring

``` r
# Check if unemployment definition is drifting
drift_result <- ont_detect_drift(
  concept_id = "unemployed",
  scope = "ilo_standard",
  threshold = 0.10,  # Alert if >10% disagreement
  min_audits = 50,
  window_days = 90
)

if (!drift_result$ok) {
  cli::cli_alert_warning("DRIFT DETECTED in unemployment definition!")
  cli::cli_alert_info("Disagreement rate: {drift_result$disagreement_rate}")
  cli::cli_alert_info("Consider methodology review before next publication")
}

# View overall drift status across all concepts
ont_drift_status()
#> -- Drift Status Summary --
#> v employed@ilo_standard v1: OK (5% disagreement, 120 audits)
#> v employed@national_headline v1: OK (6% disagreement, 85 audits)
#> ! unemployed@ilo_standard v1: WARNING (12% disagreement, 80 audits)
#> v in_labour_force@ilo_standard v1: OK (4% disagreement, 100 audits)
```

### Investigating Drift

When drift is detected, investigate the patterns.

``` r
# Get disagreeing cases
disagreements <- ont_get_audits(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 1
) |>
  dplyr::filter(system_value != reviewer_value)

# Analyze patterns
disagreements |>
  dplyr::left_join(lfs_respondents, by = c("object_key" = "respondent_id")) |>
  dplyr::group_by(has_job_attached, sought_work_4weeks) |>
  dplyr::summarise(
    n = n(),
    pct_system_true = mean(system_value)
  )
#> # A tibble: 4 x 4
#>   has_job_attached sought_work_4weeks     n pct_system_true
#>   <lgl>            <lgl>              <int>           <dbl>
#> 1 FALSE            TRUE                   3           1.0
#> 2 TRUE             FALSE                  5           0.0   # <-- Problem area!

# The issue: People with job attachment but not working/seeking
# Our SQL says "not unemployed", experts say "should check job status"
```

### Resolving Drift

``` r
# Create improved definition based on investigation
ont_add_version(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 2,
  sql_expr = "
    worked_last_week = FALSE
    AND (has_job_attached = FALSE OR (has_job_attached = TRUE AND hours_worked = 0))
    AND sought_work_4weeks = TRUE
    AND available_within_2weeks = TRUE
    AND age >= 16
  ",
  status = "draft",
  rationale = "v2: Clarified job attachment for zero-hours cases per audit findings"
)

# Compare versions
comparison <- ont_compare_versions("unemployed", "ilo_standard", v1 = 1, v2 = 2)
comparison$summary
#> # A tibble: 1 x 4
#>   total_objects v1_only v2_only both_true
#>           <int>   <int>   <int>     <int>
#> 1         10000      45     120       380

# v2 captures 120 additional unemployed (zero-hours job attachment cases)
```

------------------------------------------------------------------------

## Phase 5: Governance Gates for Publication

Before publishing official statistics, definitions must pass quality
gates.

### Configuring Publication Gates

``` r
# Create a custom gate for statistical publications
ont_create_gate(
  gate_id = "gate_statistical_quality",
  gate_name = "Statistical Quality Standards",
  gate_type = "audit_coverage",
  applies_to = "activation",
  conditions = list(
    min_audits = 50,
    min_agreement_rate = 0.90  # 90% agreement required
  ),
  severity = "blocking",
  description = "Definitions must achieve 90% expert agreement before publication use"
)

# Require methodology committee approval
ont_create_gate(
  gate_id = "gate_methodology_approval",
  gate_name = "Methodology Committee Sign-off",
  gate_type = "approval_required",
  applies_to = "activation",
  conditions = list(
    min_approvals = 2,
    approver_roles = c("chief_methodologist", "division_head")
  ),
  severity = "blocking",
  description = "Major definition changes require methodology committee approval"
)
```

### Going Through the Approval Process

``` r
# Check if v2 unemployment can be activated
gates <- ont_check_all_gates("unemployed", "ilo_standard", 2, "activation")

gates$overall_passed
#> [1] FALSE

gates$blocking_failures
#> $gate_statistical_quality
#>   passed: FALSE
#>   details: audit_count = 25 (need 50)
#>
#> $gate_methodology_approval
#>   passed: FALSE
#>   details: approvals = 0 (need 2)

# Conduct more audits for v2
v2_sample <- ont_sample_for_audit("unemployed", "ilo_standard", n = 30, version = 2)
# ... expert review process ...

# Request methodology committee approval
ont_request_approval(
  concept_id = "unemployed",
  scope = "ilo_standard",
  version = 2,
  requested_action = "activate",
  requested_by = "methodology_team"
)
#> v Created approval request: REQ-20250116-abc123

# Committee members review and approve
ont_approve_request("REQ-20250116-abc123", "chief_methodologist",
                     "Reviewed v2 changes. Aligns with ILO guidance on zero-hours contracts.")
ont_approve_request("REQ-20250116-def456", "division_head",
                     "Approved. Impact analysis shows minimal effect on headline rate.")

# Now gates pass
gates <- ont_check_all_gates("unemployed", "ilo_standard", 2, "activation")
gates$overall_passed
#> [1] TRUE

# Activate for publication use
ont_activate_version("unemployed", "ilo_standard", 2, activated_by = "chief_methodologist")
```

------------------------------------------------------------------------

## Phase 6: Materializing Publication Tables

Official statistics need reproducible, point-in-time outputs.

``` r
# Materialize unemployment classifications for Q4 2024 publication
result <- ont_materialize(
  concept_id = "unemployed",
  scope = "ilo_standard",
  filter_expr = "survey_wave = '2024Q4'",
  output_table = "pub_unemployed_2024q4",
  triggered_by = "quarterly_publication",
  executed_by = "production_team"
)
#> v Materialized unemployed to pub_unemployed_2024q4
#> i 2,500 rows (Q4 respondents)

# Materialize all labour force concepts for the publication
for (concept in c("employed", "unemployed", "in_labour_force", "underemployed")) {
  ont_materialize(
    concept_id = concept,
    scope = "national_headline",
    filter_expr = "survey_wave = '2024Q4'",
    output_table = paste0("pub_", concept, "_2024q4"),
    triggered_by = "quarterly_publication"
  )
}

# Calculate and store headline rates
DBI::dbExecute(ont_get_connection(), "
  CREATE TABLE pub_headline_rates_2024q4 AS
  SELECT
    'Q4 2024' as period,
    COUNT(*) FILTER (WHERE e.concept_value) as employed,
    COUNT(*) FILTER (WHERE u.concept_value) as unemployed,
    COUNT(*) FILTER (WHERE lf.concept_value) as labour_force,
    ROUND(100.0 * COUNT(*) FILTER (WHERE u.concept_value) /
          NULLIF(COUNT(*) FILTER (WHERE lf.concept_value), 0), 1) as unemployment_rate
  FROM pub_employed_2024q4 e
  JOIN pub_unemployed_2024q4 u USING (respondent_id)
  JOIN pub_in_labour_force_2024q4 lf USING (respondent_id)
")

DBI::dbGetQuery(ont_get_connection(), "SELECT * FROM pub_headline_rates_2024q4")
#>    period employed unemployed labour_force unemployment_rate
#> 1 Q4 2024     1847        198         2045               9.7
```

------------------------------------------------------------------------

## Phase 7: Full Provenance for Reproducibility

When questions arise about published statistics, trace everything back.

``` r
# "Where did the 9.7% unemployment rate come from?"
prov <- ont_get_provenance("DS-pub_unemployed_2024q4")

# What definition was used?
prov$concept
#> $concept_id
#> [1] "unemployed"
#>
#> $scope
#> [1] "ilo_standard"
#>
#> $version
#> [1] 2
#>
#> $sql_expr
#> [1] "worked_last_week = FALSE AND ..."

# When was it generated?
prov$last_run$ended_at
#> [1] "2025-01-15 09:30:00"

# Who approved this definition?
ont_get_gate_history("unemployed", "ilo_standard", 2)
#> Shows approval chain with timestamps and approvers

# What was the audit quality?
ont_audit_summary("unemployed", "ilo_standard", 2)
#> Shows 92% expert agreement

# Full lineage
ont_get_upstream("DS-pub_unemployed_2024q4")
#> DS-lfs_respondents -> DS-pub_unemployed_2024q4
```

------------------------------------------------------------------------

## Phase 8: Managing International Comparability

Statistical agencies often need multiple versions for different
purposes.

``` r
# Same concept, different scopes
ont_list_versions("unemployed")
#> # A tibble: 4 x 5
#>   concept_id scope             version status     rationale
#>   <chr>      <chr>               <int> <chr>      <chr>
#> 1 unemployed ilo_standard            2 active     ILO 19th ICLS with zero-hours fix
#> 2 unemployed national_headline       1 active     16-64 age restriction
#> 3 unemployed extended_measure        1 active     Includes discouraged workers
#> 4 unemployed eurostat_lfs            1 active     EU-LFS compatible definition

# Compare scopes for the same quarter
scopes <- c("ilo_standard", "national_headline", "extended_measure")

comparison_results <- lapply(scopes, function(s) {
  ont_evaluate("unemployed", s) |>
    dplyr::summarise(
      scope = s,
      unemployed_count = sum(concept_value),
      rate = round(100 * mean(concept_value), 1)
    )
}) |> dplyr::bind_rows()

comparison_results
#> # A tibble: 3 x 3
#>   scope             unemployed_count  rate
#>   <chr>                        <int> <dbl>
#> 1 ilo_standard                   823   8.2
#> 2 national_headline              756   9.7  # Different base (16-64 only)
#> 3 extended_measure               981  10.3  # Includes discouraged
```

------------------------------------------------------------------------

## Key Benefits for Statistical Agencies

| Challenge                               | ontologyR Solution                       |
|-----------------------------------------|------------------------------------------|
| “What does ‘unemployed’ mean exactly?”  | Explicit SQL definitions with versioning |
| “Why did the methodology change?”       | Rationale field + governance log         |
| “Is our definition still accurate?”     | Audit sampling + drift detection         |
| “Who approved this for publication?”    | Approval workflow + gate checks          |
| “Can we reproduce last year’s figures?” | Materialized outputs + provenance        |
| “How do we compare internationally?”    | Multiple scopes per concept              |

------------------------------------------------------------------------

## Recommendations for Statistical Offices

### 1. Start with International Standards

Use ILO, UN, or Eurostat definitions as your `ilo_standard` or
`international` scope. This ensures comparability and provides
authoritative rationale.

### 2. Document National Adaptations Explicitly

Don’t bury adaptations in footnotes. Create separate scopes
(`national_headline`, `national_detailed`) with clear rationale for
differences.

### 3. Build Auditing Into the Production Cycle

Schedule regular expert review samples. Catch drift before it affects
publications.

### 4. Require Approval for Headline Statistics

Configure gates that require methodology committee sign-off for
definitions used in headline indicators.

### 5. Maintain Full Provenance

When parliamentary questions arrive asking “where did this number come
from?”, you should be able to trace every step.

------------------------------------------------------------------------

## Next Steps

- See
  [`vignette("what-is-an-ontology")`](https://cathalbyrnegit.github.io/ontologyR/articles/what-is-an-ontology.md)
  for conceptual foundations
- See
  [`vignette("governance-gates")`](https://cathalbyrnegit.github.io/ontologyR/articles/governance-gates.md)
  for detailed RBAC and gate configuration
- See
  [`vignette("transforms-and-lineage")`](https://cathalbyrnegit.github.io/ontologyR/articles/transforms-and-lineage.md)
  for building statistical production pipelines

Remember: **In official statistics, the definition is the statistic.
Govern it accordingly.**
