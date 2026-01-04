# =============================================================================
# ontologyR: Comparing the Two API Styles
# =============================================================================
#
# This script demonstrates both API styles available in ontologyR:
#   1. Function-based (database-native): ont_*() functions
#   2. Object-based (R-native): ont$... accessor syntax
#
# Both use the same underlying tables - choose based on your preference.
#
# =============================================================================

library(ontologyR)

# =============================================================================
# STYLE 1: Function-based API (like our original design)
# =============================================================================

# Connect
ont_connect(":memory:")

# Create some test data
DBI::dbWriteTable(ont_get_connection(), "encounters", tibble::tibble(
    encounter_id = paste0("E", 1:100),
    patient_age = sample(20:90, 100, replace = TRUE),
    planned_intervention_24h = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.3, 0.7)),
    arrangements_confirmed = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.6, 0.4)),
    clinical_ready = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.5, 0.5))
))

# Register object type
ont_register_object("Encounter", "encounters", "encounter_id",
                    description = "Patient encounter",
                    owner_domain = "patient_flow")

# Define concept
ont_define_concept("ready_for_discharge", "Encounter",
                   description = "Patient ready to leave hospital",
                   owner_domain = "patient_flow")

# Add versions
ont_add_version("ready_for_discharge", "operational", 1,
                sql_expr = "NOT planned_intervention_24h",
                status = "active",
                rationale = "Simple proxy: no planned interventions")

ont_add_version("ready_for_discharge", "operational", 2,
                sql_expr = "NOT planned_intervention_24h AND arrangements_confirmed",
                status = "draft",
                rationale = "Added arrangements requirement")

ont_add_version("ready_for_discharge", "clinical", 1,
                sql_expr = "clinical_ready",
                status = "active",
                rationale = "Clinical team assessment")

# Evaluate
result <- ont_evaluate("ready_for_discharge", "operational", 1)
print(head(result))

# Compare versions
ont_compare_versions("ready_for_discharge", "operational", 1, 2)

# Clean up
ont_disconnect()


# =============================================================================
# STYLE 2: Object-based API (ontologyIndex-inspired)
# =============================================================================

# Create ontology object (connects automatically)
ont <- ontology(":memory:")

# Same test data setup
DBI::dbWriteTable(ont$.con, "encounters", tibble::tibble(
    encounter_id = paste0("E", 1:100),
    patient_age = sample(20:90, 100, replace = TRUE),
    planned_intervention_24h = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.3, 0.7)),
    arrangements_confirmed = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.6, 0.4)),
    clinical_ready = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.5, 0.5))
))

# Register using method syntax
ont$register_object("Encounter", "encounters", "encounter_id",
                    description = "Patient encounter")

# Define concept
ont$define_concept("ready_for_discharge", "Encounter",
                   description = "Patient ready to leave hospital")

# Add versions (fluent style)
ont$add_version("ready_for_discharge", "operational", 1,
                sql_expr = "NOT planned_intervention_24h",
                status = "active",
                rationale = "Simple proxy")

ont$add_version("ready_for_discharge", "operational", 2,
                sql_expr = "NOT planned_intervention_24h AND arrangements_confirmed",
                status = "draft",
                rationale = "Added arrangements")

ont$add_version("ready_for_discharge", "clinical", 1,
                sql_expr = "clinical_ready",
                status = "active",
                rationale = "Clinical assessment")

# -----------------------------------------------------------------------------
# Here's where the ergonomics shine
# -----------------------------------------------------------------------------

# View all concepts
ont$concepts
#> <ontology_concepts>
#> 1 concept(s):
#>   $ready_for_discharge

# Drill into a concept
ont$concepts$ready_for_discharge
#> <ontology_concept>
#> ID:          ready_for_discharge
#> Object type: Encounter
#> Description: Patient ready to leave hospital
#>
#> Scopes:
#>   $operational (2 version(s), 1 active)
#>   $clinical (1 version(s), 1 active)

# Access a specific scope
ont$concepts$ready_for_discharge$operational
#> <ontology_scope>
#> Concept: ready_for_discharge
#> Scope:   operational
#>
#> Versions:
#>   [[2]] [draft]
#>   [[1]] [active]

# Access a specific version
ont$concepts$ready_for_discharge$operational[[1]]
#> <ontology_version>
#> ready_for_discharge@operational v1
#>
#> Status:  active
#> SQL:     NOT planned_intervention_24h

# Evaluate directly from version object
v1 <- ont$concepts$ready_for_discharge$operational[[1]]
result <- v1$evaluate()
print(head(result))

# Get all versions as a data frame
ont$concepts$ready_for_discharge$versions

# Get active versions across all scopes
ont$concepts$ready_for_discharge$active

# -----------------------------------------------------------------------------
# Audit workflow
# -----------------------------------------------------------------------------

# Sample for audit
sample <- ont$sample("ready_for_discharge", "operational", n = 10)
print(sample)

# Record audit judgments (simulated)
for (i in 1:nrow(sample)) {
    # Simulate reviewer sometimes disagreeing
    reviewer_agrees <- runif(1) > 0.2
    ont$record_audit(
        concept_id = "ready_for_discharge",
        scope = "operational",
        version = 1,
        object_key = sample$encounter_id[i],
        system_value = sample$concept_value[i],
        reviewer_value = if (reviewer_agrees) sample$concept_value[i] else !sample$concept_value[i],
        reviewer_id = "dr_smith"
    )
}

# Check audit stats from version object
v1$audit_summary

# Or use the method
ont$audit_summary("ready_for_discharge", "operational", 1)

# -----------------------------------------------------------------------------
# Drift monitoring
# -----------------------------------------------------------------------------

# Check drift status across all concepts
ont$drift_status()

# Check specific concept
ont$check_drift("ready_for_discharge", "operational", 1)

# Generate governance report
ont$governance_report()

# Clean up
ont$disconnect()


# =============================================================================
# Summary: When to use which style
# =============================================================================
#
# USE FUNCTION-BASED (ont_*) WHEN:
#   - Writing package code or scripts
#   - Building pipelines
#   - Need explicit control over connection
#   - Working with multiple databases
#
# USE OBJECT-BASED (ont$...) WHEN:
#   - Interactive exploration
#   - Teaching/demos
#   - Quick ad-hoc analysis
#   - You prefer R-native feel
#
# BOTH STYLES:
#   - Use the same underlying tables
#   - Are fully interoperable
#   - Support all features
#
# =============================================================================
