# =============================================================================
# Tests for Dataset Registry and Materialization
# =============================================================================

# -----------------------------------------------------------------------------
# Dataset Registry
# -----------------------------------------------------------------------------

test_that("dataset registration works", {
  ont_connect(":memory:")

  # Create a source table
  DBI::dbWriteTable(ont_get_connection(), "products", tibble::tibble(
    product_id = c("P1", "P2", "P3"),
    name = c("Widget", "Gadget", "Gizmo"),
    price = c(10, 20, 30)
  ))

  # Register it
  ont_register_dataset(
    dataset_id = "ds_products",
    dataset_name = "Products Table",
    physical_name = "products",
    dataset_type = "source",
    description = "Product catalog"
  )

  ds <- ont_get_dataset("ds_products")
  expect_equal(ds$dataset_id, "ds_products")
  expect_equal(ds$dataset_name, "Products Table")
  expect_equal(ds$dataset_type, "source")
  expect_equal(ds$row_count, 3)

  ont_disconnect()
})

test_that("list datasets works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "table1", tibble::tibble(id = "A"))
  DBI::dbWriteTable(ont_get_connection(), "table2", tibble::tibble(id = "B"))

  ont_register_dataset("ds1", "Dataset 1", "table1", "source")
  ont_register_dataset("ds2", "Dataset 2", "table2", "source")

  datasets <- ont_list_datasets()
  expect_equal(nrow(datasets), 2)

  ont_disconnect()
})

test_that("list datasets with filter works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "source_table", tibble::tibble(id = "A"))

  ont_register_dataset("ds_source", "Source", "source_table", "source")

  # Filter by type
  source_ds <- ont_list_datasets(dataset_type = "source")
  expect_equal(nrow(source_ds), 1)

  mat_ds <- ont_list_datasets(dataset_type = "materialized")
  expect_equal(nrow(mat_ds), 0)

  ont_disconnect()
})

test_that("duplicate dataset registration fails", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "table1", tibble::tibble(id = "A"))

  ont_register_dataset("ds1", "Dataset 1", "table1", "source")

  expect_error(
    ont_register_dataset("ds1", "Duplicate", "table1", "source"),
    "already exists"
  )

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Materialization
# -----------------------------------------------------------------------------

test_that("basic materialization works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "orders", tibble::tibble(
    order_id = c("O1", "O2", "O3", "O4"),
    amount = c(100, 200, 300, 50),
    status = c("complete", "complete", "pending", "complete")
  ))

  ont_register_object("Order", "orders", "order_id")
  ont_define_concept("high_value_order", "Order")
  ont_add_version("high_value_order", "prod", 1, "amount > 150", status = "active")

  # Materialize
  result <- ont_materialize("high_value_order", "prod")

  expect_equal(result$status, "completed")
  expect_equal(result$row_count, 4)  # All orders, with concept_value column
  expect_true(grepl("^RUN-", result$run_id))
  expect_true(grepl("^DS-", result$dataset_id))

  # Verify the materialized table exists
  mat_data <- DBI::dbGetQuery(ont_get_connection(), "SELECT * FROM mat_high_value_order_prod")
  expect_equal(nrow(mat_data), 4)
  expect_true("concept_value" %in% names(mat_data))

  # O2 and O3 should be TRUE (amount > 150)
  high_value <- mat_data[mat_data$concept_value == TRUE, ]
  expect_equal(nrow(high_value), 2)

  ont_disconnect()
})

test_that("materialization with filter works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D"),
    value = c(10, 20, 30, 40),
    category = c("X", "X", "Y", "Y")
  ))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  # Materialize only category X
  result <- ont_materialize(
    "high_value", "test",
    filter_expr = "category = 'X'",
    output_table = "mat_high_value_catx"
  )

  expect_equal(result$row_count, 2)  # Only A and B

  mat_data <- DBI::dbGetQuery(ont_get_connection(), "SELECT * FROM mat_high_value_catx")
  expect_equal(nrow(mat_data), 2)
  expect_true(all(mat_data$category == "X"))

  ont_disconnect()
})

test_that("materialization creates dataset record", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    flag = c(TRUE, FALSE)
  ))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("flagged", "Item")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  result <- ont_materialize("flagged", "test")

  # Check dataset was registered
  ds <- ont_get_dataset(result$dataset_id)
  expect_equal(ds$dataset_type, "materialized")
  expect_equal(ds$source_concept_id, "flagged")
  expect_equal(ds$source_scope, "test")
  expect_equal(ds$source_version, 1)

  ont_disconnect()
})

test_that("materialization creates run record", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = "A",
    value = 10
  ))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  result <- ont_materialize("test_concept", "test")

  # Check run record
  run <- ont_get_run(result$run_id)
  expect_equal(run$status, "completed")
  expect_equal(run$run_type, "materialization")
  expect_equal(run$concept_id, "test_concept")
  expect_true(!is.na(run$sql_executed))

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Run Management
# -----------------------------------------------------------------------------

test_that("list runs works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    flag = c(TRUE, FALSE)
  ))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("concept1", "Item")
  ont_add_version("concept1", "test", 1, "flag", status = "active")
  ont_define_concept("concept2", "Item")
  ont_add_version("concept2", "test", 1, "NOT flag", status = "active")

  # Multiple materializations
  ont_materialize("concept1", "test")
  ont_materialize("concept2", "test")

  runs <- ont_list_runs()
  expect_equal(nrow(runs), 2)

  # Filter by concept
  runs_c1 <- ont_list_runs(concept_id = "concept1")
  expect_equal(nrow(runs_c1), 1)

  ont_disconnect()
})

test_that("list runs by status works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  ont_materialize("test_concept", "test")

  completed <- ont_list_runs(status = "completed")
  expect_equal(nrow(completed), 1)

  failed <- ont_list_runs(status = "failed")
  expect_equal(nrow(failed), 0)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Lineage
# -----------------------------------------------------------------------------

test_that("lineage edges are created during materialization", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    value = c(10, 20)
  ))

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  result <- ont_materialize("high_value", "test")

  # Check lineage edge was created
  edges <- DBI::dbGetQuery(
    ont_get_connection(),
    "SELECT * FROM ont_lineage_edges WHERE to_dataset_id = ?",
    params = list(result$dataset_id)
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$edge_type, "materialization")
  expect_equal(edges$from_dataset_id, "DS-items")

  ont_disconnect()
})

test_that("upstream lineage query works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "source_data", tibble::tibble(
    id = c("A", "B"),
    flag = c(TRUE, FALSE)
  ))

  ont_register_object("Entity", "source_data", "id")
  ont_define_concept("flagged", "Entity")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  result <- ont_materialize("flagged", "test")

  upstream <- ont_get_upstream(result$dataset_id)
  expect_equal(nrow(upstream), 1)
  expect_equal(upstream$from_dataset_id, "DS-source_data")
  expect_equal(upstream$edge_type, "materialization")

  ont_disconnect()
})

test_that("downstream lineage query works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "base_table", tibble::tibble(
    id = c("A", "B"),
    value = c(10, 20)
  ))

  ont_register_object("Entity", "base_table", "id")
  ont_define_concept("high_value", "Entity")
  ont_add_version("high_value", "test", 1, "value > 5", status = "active")

  ont_materialize("high_value", "test")

  # Source dataset should have downstream
  downstream <- ont_get_downstream("DS-base_table")
  expect_equal(nrow(downstream), 1)
  expect_true(grepl("mat_high_value_test", downstream$to_dataset_id))

  ont_disconnect()
})

test_that("lineage graph works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "raw_data", tibble::tibble(
    id = "A",
    flag = TRUE
  ))

  ont_register_object("Thing", "raw_data", "id")
  ont_define_concept("flagged", "Thing")
  ont_add_version("flagged", "test", 1, "flag", status = "active")

  result <- ont_materialize("flagged", "test")

  graph <- ont_get_lineage_graph(result$dataset_id)

  expect_equal(graph$central_dataset, result$dataset_id)
  expect_true(nrow(graph$nodes) >= 2)  # Source and materialized
  expect_equal(nrow(graph$edges), 1)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Provenance
# -----------------------------------------------------------------------------

test_that("dataset provenance works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(
    pk = c("X", "Y"),
    active = c(TRUE, FALSE)
  ))

  ont_register_object("Record", "data", "pk")
  ont_define_concept("active_record", "Record")
  ont_add_version("active_record", "prod", 1, "active", status = "active")

  result <- ont_materialize("active_record", "prod")

  prov <- ont_get_provenance(result$dataset_id)

  expect_equal(prov$dataset$dataset_id, result$dataset_id)
  expect_equal(prov$concept$concept_id, "active_record")
  expect_equal(prov$concept$scope, "prod")
  expect_equal(prov$concept$version, 1)
  expect_equal(prov$concept$sql_expr, "active")
  expect_equal(nrow(prov$upstream), 1)

  ont_disconnect()
})

test_that("provenance for non-existent dataset fails", {
  ont_connect(":memory:")

  expect_error(
    ont_get_provenance("nonexistent_ds"),
    "not found"
  )

  ont_disconnect()
})
