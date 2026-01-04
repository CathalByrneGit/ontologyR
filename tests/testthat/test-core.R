test_that("connection works", {
  # Connect to in-memory database
  ont_connect(":memory:")
  
expect_true(!is.null(ont_get_connection()))
  
  status <- ont_status()
  expect_true(status$connected)
  
  ont_disconnect()
})

test_that("object registration works", {
  ont_connect(":memory:")
  
  # Register an object type
  ont_register_object(
    object_type = "TestObject",
    table_name = "test_table",
    pk_column = "id"
  )
  
  objects <- ont_list_objects()
  expect_equal(nrow(objects), 1)
  expect_equal(objects$object_type, "TestObject")
  
  ont_disconnect()
})

test_that("concept lifecycle works", {
  ont_connect(":memory:")
  
  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))
  ont_register_object("Item", "items", "item_id")
  
  # Define concept
  ont_define_concept("high_value", "Item", description = "Value > 15")
  
  # Add version
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")
  
  # Evaluate
  result <- ont_evaluate("high_value", "test", 1)
  
  expect_equal(nrow(result), 3)
  expect_true("concept_value" %in% names(result))
  expect_equal(sum(result$concept_value), 2)  # B and C
  
  # Check provenance
  prov <- attr(result, "ontology_provenance")
  expect_equal(prov$concept_id, "high_value")
  expect_equal(prov$version, 1)
  
  ont_disconnect()
})

test_that("audit recording works", {
  ont_connect(":memory:")
  
  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    flag = c(TRUE, FALSE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")
  
  # Record audit
  audit_id <- ont_record_audit(
    concept_id = "flagged",
    scope = "test",
    version = 1,
    object_key = "A",
    system_value = TRUE,
    reviewer_value = TRUE,
    reviewer_id = "tester"
  )
  
  expect_true(grepl("^AUD-", audit_id))
  
  # Get audits
  audits <- ont_get_audits("flagged", "test", 1)
  expect_equal(nrow(audits), 1)
  expect_equal(audits$reviewer_id, "tester")
  
  ont_disconnect()
})

test_that("drift detection works", {
  ont_connect(":memory:")
  
  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    flag = c(TRUE, TRUE, TRUE)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")
  
  # Record audits with disagreement
  for (i in 1:20) {
    ont_record_audit(
      concept_id = "flagged",
      scope = "test",
      version = 1,
      object_key = paste0("obj_", i),
      system_value = TRUE,
      reviewer_value = i <= 15,  # 25% disagreement (5 of 20)
      reviewer_id = "tester"
    )
  }
  
  # Check drift
  check <- ont_check_drift("flagged", "test", 1, threshold = 0.15, min_audits = 10)
  
  expect_true(check$has_sufficient_data)
  expect_equal(check$disagreement_rate, 0.25)
  expect_true(check$drift_detected)
  
  ont_disconnect()
})
