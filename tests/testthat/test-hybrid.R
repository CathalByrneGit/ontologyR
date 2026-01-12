# =============================================================================
# Tests for Hybrid API (ontology S3 object)
# =============================================================================

test_that("ontology object creation works", {
  ont <- ontology(":memory:")

  expect_s3_class(ont, "ontology")
  expect_true(!is.null(ont$.con))
  expect_equal(ont$.path, ":memory:")

  ont_disconnect()
})

test_that("ontology object methods work", {
  ont <- ontology(":memory:")

  # Create test table
  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))

  # Use $ methods
  ont$register_object("Item", "items", "item_id")
  ont$define_concept("high_value", "Item", description = "Items with high value")
  ont$add_version("high_value", "test", 1, "value > 15", status = "active")

  # Check objects accessor
  objects <- ont$objects
  expect_equal(length(objects), 1)
  expect_true("Item" %in% names(objects))

  # Check concepts accessor
  concepts <- ont$concepts
  expect_equal(length(concepts), 1)
  expect_true("high_value" %in% names(concepts))

  ont_disconnect()
})

test_that("ontology evaluate method works", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("high_value", "Item")
  ont$add_version("high_value", "test", 1, "value > 15", status = "active")

  result <- ont$evaluate("high_value", "test")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$concept_value), 2)  # B and C

  ont_disconnect()
})

test_that("ontology sample method works", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(
    item_id = c("A", "B", "C", "D", "E"),
    flag = rep(TRUE, 5)
  ))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("flagged", "Item")
  ont$add_version("flagged", "test", 1, "flag", status = "active")

  sample <- ont$sample("flagged", "test", n = 3)
  expect_equal(nrow(sample), 3)

  ont_disconnect()
})

test_that("ontology audit methods work", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(
    item_id = c("A"),
    flag = TRUE
  ))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("flagged", "Item")
  ont$add_version("flagged", "test", 1, "flag", status = "active")

  audit_id <- ont$record_audit("flagged", "test", 1, "A", TRUE, TRUE, "reviewer1")
  expect_true(grepl("^AUD-", audit_id))

  ont_disconnect()
})

test_that("ontology activation and deprecation work", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(item_id = "A"))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("test_concept", "Item")
  ont$add_version("test_concept", "test", 1, "TRUE", status = "draft")

  # Activate
  ont$activate("test_concept", "test", 1, "admin")

  version <- ont_get_version("test_concept", "test", 1, con = ont$.con)
  expect_equal(version$status, "active")

  # Deprecate
  ont$deprecate("test_concept", "test", 1, "admin")

  version <- ont_get_version("test_concept", "test", 1, con = ont$.con)
  expect_equal(version$status, "deprecated")

  ont_disconnect()
})

test_that("ontology drift_status method works", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(
    item_id = c("A"),
    flag = TRUE
  ))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("flagged", "Item")
  ont$add_version("flagged", "test", 1, "flag", status = "active")

  # Record audits
  for (i in 1:15) {
    ont$record_audit("flagged", "test", 1, paste0("obj_", i), TRUE, TRUE, "reviewer1")
  }

  status <- ont$drift_status()
  expect_true(is.data.frame(status) || is.list(status))

  ont_disconnect()
})

test_that("ontology print method works", {
  ont <- ontology(":memory:")

  # Should not error
  expect_output(print(ont), "ontology")

  ont_disconnect()
})

test_that("ontology concepts accessor provides nested access", {
  ont <- ontology(":memory:")

  DBI::dbWriteTable(ont$.con, "items", tibble::tibble(item_id = "A"))

  ont$register_object("Item", "items", "item_id")
  ont$define_concept("my_concept", "Item", description = "Test concept")
  ont$add_version("my_concept", "scope1", 1, "TRUE", status = "active")
  ont$add_version("my_concept", "scope2", 1, "TRUE", status = "draft")

  # Access concept metadata
  concepts <- ont$concepts
  expect_true("my_concept" %in% names(concepts))

  # Access specific concept
  my_concept <- concepts$my_concept
  expect_equal(my_concept$description, "Test concept")

  ont_disconnect()
})

test_that("ontology cache invalidation works", {
  ont <- ontology(":memory:")

  # Register object
  ont$register_object("Item1", "table1", "id1")

  objects1 <- ont$objects
  expect_equal(length(objects1), 1)

  # Register another object - cache should be invalidated
  ont$register_object("Item2", "table2", "id2")

  objects2 <- ont$objects
  expect_equal(length(objects2), 2)

  ont_disconnect()
})

test_that("ontology link registration works", {
  ont <- ontology(":memory:")

  ont$register_object("Patient", "patients", "patient_id")
  ont$register_object("Encounter", "encounters", "encounter_id")

  ont$register_link(
    link_type = "patient_encounter",
    from_object = "Patient",
    to_object = "Encounter",
    link_table = "patient_encounters",
    from_key = "patient_id",
    to_key = "encounter_id"
  )

  links <- ont$links
  expect_equal(length(links), 1)
  expect_true("patient_encounter" %in% names(links))

  ont_disconnect()
})
