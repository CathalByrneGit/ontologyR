# =============================================================================
# Extended Tests for ontologyR
# =============================================================================

# -----------------------------------------------------------------------------
# Link Type Registration
# -----------------------------------------------------------------------------

test_that("link type registration works", {
  ont_connect(":memory:")

  # Register object types first

ont_register_object("Patient", "patients", "patient_id")
  ont_register_object("Encounter", "encounters", "encounter_id")

  # Register a link type
  ont_register_link(
    link_type = "patient_encounter",
    from_object = "Patient",
    to_object = "Encounter",
    link_table = "patient_encounters",
    from_key = "patient_id",
    to_key = "encounter_id",
    cardinality = "one-to-many"
  )

  links <- ont_list_links()
  expect_equal(nrow(links), 1)
  expect_equal(links$link_type, "patient_encounter")
  expect_equal(links$cardinality, "one-to-many")

  ont_disconnect()
})

test_that("link type requires valid object types", {
  ont_connect(":memory:")

  ont_register_object("Patient", "patients", "patient_id")

  # Should fail - Encounter not registered
  expect_error(
    ont_register_link(
      link_type = "patient_encounter",
      from_object = "Patient",
      to_object = "Encounter",  # Not registered
      link_table = "patient_encounters",
      from_key = "patient_id",
      to_key = "encounter_id"
    ),
    "Unknown object type"
  )

  ont_disconnect()
})

test_that("link type validates cardinality", {
  ont_connect(":memory:")

  ont_register_object("A", "table_a", "id_a")
  ont_register_object("B", "table_b", "id_b")

  expect_error(
    ont_register_link(
      link_type = "a_to_b",
      from_object = "A",
      to_object = "B",
      link_table = "a_b_links",
      from_key = "id_a",
      to_key = "id_b",
      cardinality = "invalid"
    ),
    "Invalid cardinality"
  )

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Governance Actions
# -----------------------------------------------------------------------------

test_that("version activation works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    value = c(10, 20)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "draft")

  # Version should be draft
  version <- ont_get_version("high_value", "test", 1)
  expect_equal(version$status, "draft")

  # Activate
  ont_activate_version("high_value", "test", 1, approved_by = "admin")

  # Check status
  version <- ont_get_version("high_value", "test", 1)
  expect_equal(version$status, "active")
  expect_equal(version$approved_by, "admin")

  ont_disconnect()
})

test_that("version deprecation works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    value = c(10)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "value > 5", status = "active")

  # Deprecate
  ont_deprecate_version("test_concept", "test", 1, deprecated_by = "admin")

  version <- ont_get_version("test_concept", "test", 1)
  expect_equal(version$status, "deprecated")

  ont_disconnect()
})

test_that("governance log is recorded", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    value = c(10)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "value > 5", status = "draft")

  # Activate - this should log
  ont_activate_version("test_concept", "test", 1, approved_by = "admin")

  # Check governance log
  log <- ont_get_governance_log(concept_id = "test_concept")
  expect_true(nrow(log) >= 1)
  expect_true("activate" %in% log$action_type)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Multiple Versions and Scopes
# -----------------------------------------------------------------------------

test_that("multiple versions of same concept work", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")

  # Add multiple versions with different thresholds
  ont_add_version("high_value", "test", 1, "value > 25", status = "active")
  ont_add_version("high_value", "test", 2, "value > 15", status = "draft")

  # Evaluate each version
  result_v1 <- ont_evaluate("high_value", "test", 1)
  result_v2 <- ont_evaluate("high_value", "test", 2)

  expect_equal(sum(result_v1$concept_value), 1)  # Only C
  expect_equal(sum(result_v2$concept_value), 2)  # B and C

  ont_disconnect()
})

test_that("same concept different scopes work", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")

  # Add versions in different scopes
  ont_add_version("high_value", "operations", 1, "value > 25", status = "active")
  ont_add_version("high_value", "finance", 1, "value > 15", status = "active")

  # Evaluate each scope
  ops_result <- ont_evaluate("high_value", "operations", 1)
  fin_result <- ont_evaluate("high_value", "finance", 1)

  expect_equal(sum(ops_result$concept_value), 1)   # Only C
  expect_equal(sum(fin_result$concept_value), 2)   # B and C

  ont_disconnect()
})

test_that("get active version finds correct version", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    value = c(10)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")

  # Add multiple versions, only one active
  ont_add_version("test_concept", "test", 1, "value > 5", status = "deprecated")
  ont_add_version("test_concept", "test", 2, "value > 10", status = "active")
  ont_add_version("test_concept", "test", 3, "value > 15", status = "draft")

  active <- ont_get_active_version("test_concept", "test")
  expect_equal(active$version, 2)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Error Handling and Validation
# -----------------------------------------------------------------------------

test_that("duplicate concept ID is rejected", {
  ont_connect(":memory:")

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("my_concept", "Item")

  expect_error(
    ont_define_concept("my_concept", "Item"),
    "already exists"
  )

  ont_disconnect()
})

test_that("duplicate version is rejected", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE")

  expect_error(
    ont_add_version("test_concept", "test", 1, "FALSE"),
    "already exists"
  )

  ont_disconnect()
})

test_that("invalid status is rejected", {
  ont_connect(":memory:")

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")

  expect_error(
    ont_add_version("test_concept", "test", 1, "TRUE", status = "invalid_status"),
    "Invalid status"
  )

  ont_disconnect()
})

test_that("concept for unknown object type is rejected", {
  ont_connect(":memory:")

  expect_error(
    ont_define_concept("test_concept", "NonExistent"),
    "Unknown object type"
  )

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Audit Summary and Statistics
# -----------------------------------------------------------------------------

test_that("audit summary computes correctly", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    flag = c(TRUE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  # Record 10 audits: 7 agreements, 3 disagreements
  for (i in 1:10) {
    ont_record_audit(
      concept_id = "flagged",
      scope = "test",
      version = 1,
      object_key = paste0("obj_", i),
      system_value = TRUE,
      reviewer_value = i <= 7,  # 30% disagreement
      reviewer_id = "tester"
    )
  }

  summary <- ont_audit_summary("flagged", "test", 1)

  expect_equal(summary$audit_count, 10)
  expect_equal(summary$agreements, 7)
  expect_equal(summary$disagreements, 3)
  expect_equal(summary$disagreement_rate, 0.3)

  ont_disconnect()
})

test_that("audit summary handles no audits", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  summary <- ont_audit_summary("test_concept", "test", 1)

  expect_equal(summary$audit_count, 0)
  expect_true(is.na(summary$disagreement_rate))

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Drift Event Management
# -----------------------------------------------------------------------------

test_that("drift event creation works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    flag = c(TRUE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  # Create drift event
  drift_id <- ont_create_drift_event(
    concept_id = "flagged",
    scope = "test",
    version = 1,
    detection_type = "threshold",
    disagreement_rate = 0.30,
    window_days = 7,
    audit_count = 20
  )

  expect_true(grepl("^DRIFT-", drift_id))

  # List drift events
  events <- ont_list_drift_events(concept_id = "flagged")
  expect_equal(nrow(events), 1)
  expect_equal(events$status, "open")

  ont_disconnect()
})

test_that("drift event resolution works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    flag = c(TRUE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  # Create and resolve drift event
  drift_id <- ont_create_drift_event(
    concept_id = "flagged",
    scope = "test",
    version = 1,
    detection_type = "threshold",
    disagreement_rate = 0.30,
    window_days = 7,
    audit_count = 20
  )

  ont_resolve_drift(
    drift_id = drift_id,
    resolution = "Updated definition in v2",
    resolved_by = "admin"
  )

  events <- ont_list_drift_events(concept_id = "flagged")
  expect_equal(events$status, "resolved")
  expect_equal(events$resolution, "Updated definition in v2")

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Dashboard Registration
# -----------------------------------------------------------------------------

test_that("dashboard registration works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  # Register dashboard
  ont_register_dashboard(
    dashboard_id = "dashboard_001",
    concept_id = "test_concept",
    scope = "test",
    version = 1,
    dashboard_name = "My Dashboard"
  )

  dashboards <- ont_list_dashboards(concept_id = "test_concept")
  expect_equal(nrow(dashboards), 1)
  expect_equal(dashboards$dashboard_name, "My Dashboard")

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Edge Cases
# -----------------------------------------------------------------------------

test_that("empty table evaluation works", {
  ont_connect(":memory:")

  # Setup empty table
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = character(0),
    value = numeric(0)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 10", status = "active")

  result <- ont_evaluate("high_value", "test", 1)
  expect_equal(nrow(result), 0)

  ont_disconnect()
})

test_that("sampling with insufficient population warns", {
  ont_connect(":memory:")

  # Setup with only 2 items
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    flag = c(TRUE, TRUE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  # Request 20 samples but only 2 exist
  sample <- ont_sample_for_audit("flagged", "test", n = 20)

  # Should get only 2
  expect_equal(nrow(sample), 2)

  ont_disconnect()
})

test_that("batch audit recording works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    flag = c(TRUE, TRUE, FALSE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  # Batch record audits
  audits_df <- tibble::tibble(
    object_key = c("A", "B", "C"),
    system_value = c(TRUE, TRUE, FALSE),
    reviewer_value = c(TRUE, FALSE, FALSE),
    notes = c("Correct", "Incorrect", "Correct")
  )

  audit_ids <- ont_record_audits(
    audits = audits_df,
    concept_id = "flagged",
    scope = "test",
    version = 1,
    reviewer_id = "batch_tester"
  )

  expect_equal(length(audit_ids), 3)

  all_audits <- ont_get_audits("flagged", "test", 1)
  expect_equal(nrow(all_audits), 3)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# SQL Expression Validation
# -----------------------------------------------------------------------------

test_that("SQL validation detects valid expressions", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    value = c(10)
  ))
  ont_register_object("Item", "items", "item_id")

  result <- ont_validate_sql("value > 5", "Item")
  expect_true(result$valid)
  expect_null(result$error)

  ont_disconnect()
})

test_that("SQL validation detects invalid expressions", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A"),
    value = c(10)
  ))
  ont_register_object("Item", "items", "item_id")

  result <- ont_validate_sql("nonexistent_column > 5", "Item")
  expect_false(result$valid)
  expect_true(!is.null(result$error))

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Object Type with Optional Fields
# -----------------------------------------------------------------------------

test_that("object registration with all optional fields works", {
  ont_connect(":memory:")

  ont_register_object(
    object_type = "FullObject",
    table_name = "full_table",
    pk_column = "id",
    description = "A fully documented object type",
    owner_domain = "test_domain",
    created_by = "test_user"
  )

  objects <- ont_list_objects()
  expect_equal(nrow(objects), 1)
  expect_equal(objects$description, "A fully documented object type")
  expect_equal(objects$owner_domain, "test_domain")
  expect_equal(objects$created_by, "test_user")

  ont_disconnect()
})

test_that("concept version with optional fields works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item", description = "Test description")

  ont_add_version(
    concept_id = "test_concept",
    scope = "test",
    version = 1,
    sql_expr = "TRUE",
    status = "draft",
    rationale = "Initial version for testing",
    created_by = "test_user"
  )

  version <- ont_get_version("test_concept", "test", 1)
  expect_equal(version$rationale, "Initial version for testing")
  expect_equal(version$created_by, "test_user")

  ont_disconnect()
})
