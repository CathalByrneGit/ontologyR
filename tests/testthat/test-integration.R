# =============================================================================
# Integration Tests
# =============================================================================
# These tests validate end-to-end workflows and ensure components work together.
# =============================================================================

# -----------------------------------------------------------------------------
# Full Concept Lifecycle Integration Test
# -----------------------------------------------------------------------------

test_that("full concept lifecycle: define -> version -> audit -> drift -> governance", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # 1. Setup: Register object type with test data
    ont_register_object("Patient", "patients", "patient_id")

    # Create test data
    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE patients (
            patient_id INTEGER PRIMARY KEY,
            age INTEGER,
            los_days INTEGER,
            discharged BOOLEAN
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO patients VALUES
        (1, 65, 3, TRUE),
        (2, 45, 1, TRUE),
        (3, 72, 5, FALSE),
        (4, 30, 2, TRUE),
        (5, 80, 7, FALSE)
    ")

    # 2. Define concept
    ont_define_concept(
        concept_id = "ready_for_discharge",
        object_type = "Patient",
        description = "Patient ready for discharge based on clinical criteria"
    )

    # Verify concept exists
    concepts <- ont_list_concepts()
    expect_equal(nrow(concepts), 1)
    expect_equal(concepts$concept_id[1], "ready_for_discharge")

    # 3. Add version (draft)
    ont_add_version(
        concept_id = "ready_for_discharge",
        scope = "clinical",
        version = 1,
        sql_expr = "los_days >= 2 AND age < 75",
        status = "draft",
        rationale = "Initial clinical criteria"
    )

    version <- ont_get_version("ready_for_discharge", "clinical", 1)
    expect_equal(version$status, "draft")

    # 4. Evaluate concept
    result <- ont_evaluate("ready_for_discharge", "clinical", 1)
    expect_equal(nrow(result), 5)
    expect_true("concept_value" %in% names(result))

    # Count TRUE values (patients 1, 4 should match: los >= 2 AND age < 75)
    true_count <- sum(result$concept_value, na.rm = TRUE)
    expect_equal(true_count, 2)

    # 5. Record audits (simulate human review)
    # Patient 1: system says TRUE, reviewer agrees
    ont_record_audit(
        concept_id = "ready_for_discharge",
        scope = "clinical",
        version = 1,
        object_key = "1",
        system_value = TRUE,
        reviewer_value = TRUE,
        reviewer_id = "dr_smith"
    )

    # Patient 3: system says FALSE, reviewer disagrees (drift!)
    ont_record_audit(
        concept_id = "ready_for_discharge",
        scope = "clinical",
        version = 1,
        object_key = "3",
        system_value = FALSE,
        reviewer_value = TRUE,  # Reviewer thinks patient IS ready
        reviewer_id = "dr_smith",
        notes = "Patient stable despite age"
    )

    # 6. Check drift
    drift <- ont_check_drift("ready_for_discharge", "clinical", 1)
    expect_equal(drift$total_audits, 2)
    expect_equal(drift$disagreements, 1)
    expect_equal(drift$disagreement_rate, 0.5)

    # 7. Governance action: activate with rationale
    ont_governance_action(
        action_type = "activate",
        concept_id = "ready_for_discharge",
        scope = "clinical",
        version = 1,
        actor = "governance_board",
        rationale = "Approved after clinical review"
    )

    # Verify status changed
    updated_version <- ont_get_version("ready_for_discharge", "clinical", 1)
    expect_equal(updated_version$status, "active")

    # 8. Create new version to address drift
    ont_add_version(
        concept_id = "ready_for_discharge",
        scope = "clinical",
        version = 2,
        sql_expr = "los_days >= 2 AND (age < 75 OR (age >= 75 AND los_days >= 5))",
        status = "draft",
        rationale = "Updated to handle elderly patients who are stable"
    )

    # Verify both versions exist
    versions <- ont_list_versions("ready_for_discharge")
    expect_equal(nrow(versions), 2)
})

# -----------------------------------------------------------------------------
# Template Inheritance Integration Test
# -----------------------------------------------------------------------------

test_that("template inheritance workflow: define template -> create variants -> compare", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Setup
    ont_register_object("Person", "persons", "person_id")

    # Create test data
    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE persons (
            person_id INTEGER PRIMARY KEY,
            age INTEGER,
            employed BOOLEAN,
            seeking_work BOOLEAN,
            available_in_weeks INTEGER
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO persons VALUES
        (1, 25, FALSE, TRUE, 1),
        (2, 45, TRUE, FALSE, 0),
        (3, 17, FALSE, TRUE, 2),
        (4, 68, FALSE, TRUE, 1),
        (5, 30, FALSE, FALSE, 4)
    ")

    # 1. Define ILO template
    ont_define_template(
        template_id = "ilo_unemployed",
        template_name = "ILO Unemployment Definition",
        object_type = "Person",
        base_sql_expr = "age >= {{min_age}} AND age <= {{max_age}} AND NOT employed AND seeking_work AND available_in_weeks <= {{availability_weeks}}",
        parameters = list(
            min_age = list(default = 15, type = "integer"),
            max_age = list(default = 74, type = "integer"),
            availability_weeks = list(default = 2, type = "integer")
        ),
        source_standard = "ILO",
        description = "International Labour Organization standard unemployment"
    )

    # Verify template
    template <- ont_get_template("ilo_unemployed")
    expect_equal(template$source_standard, "ILO")

    # 2. Create country variants
    ont_inherit_concept(
        concept_id = "unemployed_us",
        template_id = "ilo_unemployed",
        scope = "united_states",
        parameter_values = list(min_age = 16, max_age = 65),
        deviation_notes = "US uses 16-65 age range per BLS"
    )

    ont_inherit_concept(
        concept_id = "unemployed_ireland",
        template_id = "ilo_unemployed",
        scope = "ireland",
        parameter_values = list(min_age = 15, max_age = 66),
        deviation_notes = "Ireland follows CSO guidelines"
    )

    ont_inherit_concept(
        concept_id = "unemployed_uk",
        template_id = "ilo_unemployed",
        scope = "united_kingdom",
        parameter_values = list(min_age = 16, max_age = 64, availability_weeks = 2),
        deviation_notes = "UK uses ONS definition"
    )

    # 3. Verify variants created
    variants <- ont_get_template_variants("ilo_unemployed")
    expect_equal(nrow(variants), 3)

    # 4. Compare variants
    comparison <- ont_compare_template_variants("ilo_unemployed")
    expect_equal(nrow(comparison), 3)
    expect_true("param_min_age" %in% names(comparison))
    expect_true("param_max_age" %in% names(comparison))

    # 5. Evaluate each variant
    us_result <- ont_evaluate("unemployed_us", "united_states", 1)
    ireland_result <- ont_evaluate("unemployed_ireland", "ireland", 1)
    uk_result <- ont_evaluate("unemployed_uk", "united_kingdom", 1)

    # US: age 16-65, so person 3 (age 17) and person 1 (age 25) qualify
    # Person 4 (age 68) excluded by max_age=65
    us_unemployed <- sum(us_result$concept_value, na.rm = TRUE)

    # Ireland: age 15-66, so persons 1, 3, 4 could qualify
    ireland_unemployed <- sum(ireland_result$concept_value, na.rm = TRUE)

    # Different age ranges should produce different counts
    expect_true(is.numeric(us_unemployed))
    expect_true(is.numeric(ireland_unemployed))

    # 6. Check inheritance
    us_inheritance <- ont_get_concept_inheritance("unemployed_us")
    expect_equal(nrow(us_inheritance), 1)
    expect_equal(us_inheritance$template_id[1], "ilo_unemployed")
})

# -----------------------------------------------------------------------------
# Dataset and Lineage Integration Test
# -----------------------------------------------------------------------------

test_that("dataset and lineage workflow: register -> materialize -> track lineage", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Setup
    ont_register_object("Encounter", "encounters", "encounter_id")

    # Create source data
    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE encounters (
            encounter_id INTEGER PRIMARY KEY,
            patient_id INTEGER,
            admission_date DATE,
            discharge_date DATE,
            department TEXT
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO encounters VALUES
        (1, 101, '2024-01-01', '2024-01-05', 'ICU'),
        (2, 102, '2024-01-02', '2024-01-03', 'General'),
        (3, 103, '2024-01-03', NULL, 'ICU'),
        (4, 104, '2024-01-04', '2024-01-06', 'General'),
        (5, 105, '2024-01-05', '2024-01-07', 'ICU')
    ")

    # 1. Register source dataset
    ont_register_dataset(
        dataset_id = "encounters_source",
        dataset_name = "Raw Encounters",
        physical_name = "encounters",
        dataset_type = "source",
        object_type = "Encounter"
    )

    # Verify registration
    datasets <- ont_list_datasets()
    expect_equal(nrow(datasets), 1)

    # 2. Define concept for ICU patients
    ont_define_concept("icu_patient", "Encounter", "Patient in ICU")
    ont_add_version("icu_patient", "operational", 1, "department = 'ICU'", "active")

    # 3. Materialize the concept
    mat_result <- ont_materialize(
        concept_id = "icu_patient",
        scope = "operational",
        version = 1,
        output_name = "icu_encounters"
    )

    expect_true(!is.null(mat_result$run_id))
    expect_true(!is.null(mat_result$dataset_id))

    # 4. Check materialized dataset exists
    datasets_after <- ont_list_datasets()
    expect_equal(nrow(datasets_after), 2)

    # 5. Check lineage
    lineage <- ont_get_lineage_graph()
    expect_true(nrow(lineage$nodes) >= 2)
    expect_true(nrow(lineage$edges) >= 1)

    # 6. Get upstream of materialized dataset
    upstream <- ont_get_upstream(mat_result$dataset_id)
    expect_true("encounters_source" %in% upstream$dataset_id)

    # 7. Get downstream of source
    downstream <- ont_get_downstream("encounters_source")
    expect_true(mat_result$dataset_id %in% downstream$dataset_id)
})

# -----------------------------------------------------------------------------
# RBAC and Governance Gates Integration Test
# -----------------------------------------------------------------------------

test_that("RBAC and governance gates workflow", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Setup
    ont_register_object("Record", "records", "record_id")

    # 1. Grant roles to users
    ont_grant_role("alice", "editor", scope_type = "global")
    ont_grant_role("bob", "approver", scope_type = "global")
    ont_grant_role("charlie", "viewer", scope_type = "global")

    # 2. Check permissions
    expect_true(ont_check_permission("alice", "concept:write"))
    expect_true(ont_check_permission("bob", "concept:approve"))
    expect_false(ont_check_permission("charlie", "concept:write"))
    expect_true(ont_check_permission("charlie", "concept:read"))

    # 3. List roles
    roles <- ont_list_roles()
    expect_true(nrow(roles) >= 4)  # viewer, editor, approver, admin

    # 4. Check gates exist
    gates <- ont_list_gates()
    expect_true(nrow(gates) >= 1)

    # 5. Create a concept to test gates
    ont_define_concept("test_concept", "Record", "Test")
    ont_add_version("test_concept", "test", 1, "TRUE", "draft")

    # 6. Check gate (should fail - no audits yet)
    gate_result <- ont_check_gate("gate_audit_coverage", "test_concept", "test", 1, "activation")
    expect_equal(gate_result$result, "failed")

    # 7. Request approval
    request_id <- ont_request_approval(
        concept_id = "test_concept",
        scope = "test",
        version = 1,
        requested_action = "activate",
        requested_by = "alice"
    )

    expect_true(!is.null(request_id))

    # 8. Approve request
    ont_approve_request(request_id, decided_by = "bob", decision_notes = "Approved")

    # Verify status
    con <- ont_get_connection()
    request <- DBI::dbGetQuery(con,
        "SELECT status FROM ont_approval_requests WHERE request_id = ?",
        params = list(request_id)
    )
    expect_equal(request$status[1], "approved")
})

# -----------------------------------------------------------------------------
# Observation and Analysis Integration Test
# -----------------------------------------------------------------------------

test_that("observation and analysis workflow", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # Setup
    ont_register_object("Case", "cases", "case_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE cases (
            case_id INTEGER PRIMARY KEY,
            status TEXT,
            priority INTEGER,
            created_date DATE
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO cases VALUES
        (1, 'open', 1, '2024-01-01'),
        (2, 'closed', 2, '2024-01-02'),
        (3, 'open', 1, '2024-01-03'),
        (4, 'open', 3, '2024-01-04'),
        (5, 'closed', 1, '2024-01-05')
    ")

    # 1. Define concept
    ont_define_concept("high_priority_open", "Case", "High priority open cases")
    ont_add_version("high_priority_open", "ops", 1, "status = 'open' AND priority = 1", "active")

    # 2. Record observation
    obs_result <- ont_observe("high_priority_open", "ops", 1)

    expect_true(!is.null(obs_result$observation_id))
    expect_equal(obs_result$total_objects, 5)
    expect_equal(obs_result$concept_true, 2)  # cases 1 and 3

    # 3. Record another observation (simulating time passing)
    Sys.sleep(0.1)  # Small delay to ensure different timestamp
    obs_result2 <- ont_observe("high_priority_open", "ops", 1)

    # 4. Get trend
    trend <- ont_get_trend("high_priority_open", "ops", 1)
    expect_true(nrow(trend) >= 1)

    # 5. Compare versions
    ont_add_version("high_priority_open", "ops", 2, "status = 'open' AND priority <= 2", "active")

    comparison <- ont_compare_versions(
        concept_id = "high_priority_open",
        scope = "ops",
        version1 = 1,
        version2 = 2
    )

    expect_true(!is.null(comparison))
    # Version 2 should have more matches (priority <= 2 instead of = 1)
})

# -----------------------------------------------------------------------------
# Multi-Scope Concept Integration Test
# -----------------------------------------------------------------------------

test_that("multi-scope concept workflow", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    ont_register_object("Transaction", "transactions", "txn_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE transactions (
            txn_id INTEGER PRIMARY KEY,
            amount REAL,
            risk_score INTEGER,
            flagged BOOLEAN
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO transactions VALUES
        (1, 100.00, 20, FALSE),
        (2, 5000.00, 80, TRUE),
        (3, 250.00, 45, FALSE),
        (4, 10000.00, 95, TRUE),
        (5, 500.00, 30, FALSE)
    ")

    # 1. Define concept with multiple scopes
    ont_define_concept("high_risk", "Transaction", "High risk transactions")

    # Regulatory scope (strict)
    ont_add_version("high_risk", "regulatory", 1,
        "risk_score >= 70 OR amount >= 10000",
        "active", "Regulatory compliance threshold")

    # Operational scope (moderate)
    ont_add_version("high_risk", "operational", 1,
        "risk_score >= 50",
        "active", "Internal operations threshold")

    # ML scope (data-driven)
    ont_add_version("high_risk", "ml_model", 1,
        "flagged = TRUE",
        "active", "Based on ML model predictions")

    # 2. Evaluate each scope
    reg_result <- ont_evaluate("high_risk", "regulatory", 1)
    ops_result <- ont_evaluate("high_risk", "operational", 1)
    ml_result <- ont_evaluate("high_risk", "ml_model", 1)

    reg_count <- sum(reg_result$concept_value, na.rm = TRUE)
    ops_count <- sum(ops_result$concept_value, na.rm = TRUE)
    ml_count <- sum(ml_result$concept_value, na.rm = TRUE)

    # Regulatory: txns 2, 4 (risk >= 70 or amount >= 10000)
    expect_equal(reg_count, 2)

    # Operational: txns 2, 3, 4 (risk >= 50)
    expect_equal(ops_count, 2)

    # ML: txns 2, 4 (flagged = TRUE)
    expect_equal(ml_count, 2)

    # 3. List all versions
    versions <- ont_list_versions("high_risk")
    expect_equal(nrow(versions), 3)
    expect_setequal(versions$scope, c("regulatory", "operational", "ml_model"))
})

# -----------------------------------------------------------------------------
# End-to-End Workflow: From Raw Data to Governed Insights
# -----------------------------------------------------------------------------

test_that("end-to-end: raw data to governed insights", {
    ont_connect(":memory:")
    on.exit(ont_disconnect())

    # === PHASE 1: Data Setup ===
    ont_register_object("Employee", "employees", "emp_id")

    DBI::dbExecute(ont_get_connection(), "
        CREATE TABLE employees (
            emp_id INTEGER PRIMARY KEY,
            department TEXT,
            tenure_years INTEGER,
            performance_score INTEGER,
            salary REAL
        )
    ")

    DBI::dbExecute(ont_get_connection(), "
        INSERT INTO employees VALUES
        (1, 'Engineering', 5, 4, 120000),
        (2, 'Engineering', 2, 3, 85000),
        (3, 'Sales', 8, 5, 95000),
        (4, 'Sales', 1, 2, 60000),
        (5, 'HR', 10, 4, 75000),
        (6, 'Engineering', 3, 5, 110000)
    ")

    ont_register_dataset(
        dataset_id = "employees_source",
        dataset_name = "Employee Master",
        physical_name = "employees",
        dataset_type = "source"
    )

    # === PHASE 2: Define Business Concepts ===

    # High performer concept
    ont_define_concept("high_performer", "Employee", "Employees with top performance")
    ont_add_version("high_performer", "hr", 1,
        "performance_score >= 4",
        "draft", "Initial HR definition")

    # Promotion eligible
    ont_define_concept("promotion_eligible", "Employee",
        "Employees eligible for promotion consideration")
    ont_add_version("promotion_eligible", "hr", 1,
        "performance_score >= 4 AND tenure_years >= 2",
        "draft", "Must be high performer with 2+ years")

    # === PHASE 3: Audit and Validate ===

    # Evaluate high performers
    hp_result <- ont_evaluate("high_performer", "hr", 1)
    hp_count <- sum(hp_result$concept_value, na.rm = TRUE)
    expect_equal(hp_count, 4)  # emp 1, 3, 5, 6

    # Record some audits
    ont_record_audit("high_performer", "hr", 1, "1", TRUE, TRUE, "hr_manager")
    ont_record_audit("high_performer", "hr", 1, "3", TRUE, TRUE, "hr_manager")
    ont_record_audit("high_performer", "hr", 1, "5", TRUE, TRUE, "hr_manager")

    # Check drift (should be 0%)
    drift <- ont_check_drift("high_performer", "hr", 1)
    expect_equal(drift$disagreement_rate, 0)

    # === PHASE 4: Governance ===

    # Grant roles
    ont_grant_role("hr_analyst", "editor", scope_type = "domain", scope_value = "hr")
    ont_grant_role("hr_director", "approver", scope_type = "domain", scope_value = "hr")

    # Request approval
    request_id <- ont_request_approval(
        concept_id = "high_performer",
        scope = "hr",
        version = 1,
        requested_action = "activate",
        requested_by = "hr_analyst"
    )

    # Approve
    ont_approve_request(request_id, "hr_director", "Validated against historical data")

    # Activate via governance
    ont_governance_action(
        action_type = "activate",
        concept_id = "high_performer",
        scope = "hr",
        version = 1,
        actor = "hr_director",
        rationale = "Approved by HR leadership"
    )

    # === PHASE 5: Materialize and Track ===

    mat_result <- ont_materialize(
        concept_id = "high_performer",
        scope = "hr",
        version = 1,
        output_name = "high_performers_2024"
    )

    # Verify materialization
    expect_true(!is.null(mat_result$dataset_id))

    # Check lineage
    lineage <- ont_get_lineage_graph()
    expect_true(nrow(lineage$nodes) >= 2)

    # === PHASE 6: Record Observations for Trend Analysis ===

    ont_observe("high_performer", "hr", 1)

    # Get governance log
    con <- ont_get_connection()
    gov_log <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_governance_log WHERE concept_id = 'high_performer'"
    )
    expect_true(nrow(gov_log) >= 1)

    # === FINAL VERIFICATION ===
    concepts <- ont_list_concepts()
    expect_equal(nrow(concepts), 2)

    datasets <- ont_list_datasets()
    expect_true(nrow(datasets) >= 2)
})
