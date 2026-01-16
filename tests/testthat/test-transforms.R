# =============================================================================
# Tests for Transform Management and Execution
# =============================================================================

# -----------------------------------------------------------------------------
# Transform Definition
# -----------------------------------------------------------------------------

test_that("define transform works", {
    ont_connect(":memory:")

    # Create source datasets
    DBI::dbWriteTable(ont_get_connection(), "orders", tibble::tibble(
        order_id = c("O1", "O2", "O3"),
        amount = c(100, 200, 300)
    ))

    ont_register_dataset("ds_orders", "Orders", "orders", "source")

    # Define a transform
    ont_define_transform(
        transform_id = "txf_high_orders",
        transform_name = "High Value Orders",
        output_dataset_id = "ds_high_orders",
        transform_type = "sql",
        code = "SELECT * FROM orders WHERE amount > 150",
        input_datasets = c("ds_orders"),
        description = "Filter high value orders"
    )

    transform <- ont_get_transform("txf_high_orders")
    expect_equal(transform$transform_id, "txf_high_orders")
    expect_equal(transform$transform_type, "sql")
    expect_equal(nrow(transform$inputs), 1)
    expect_equal(transform$inputs$input_dataset_id, "ds_orders")

    ont_disconnect()
})

test_that("list transforms works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(id = "A"))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform 1", "ds_out1", "sql", "SELECT * FROM data")
    ont_define_transform("txf2", "Transform 2", "ds_out2", "sql", "SELECT * FROM data")

    transforms <- ont_list_transforms()
    expect_equal(nrow(transforms), 2)

    ont_disconnect()
})

test_that("duplicate transform fails", {
    ont_connect(":memory:")

    ont_define_transform("txf1", "Transform 1", "ds_out1", "sql", "SELECT 1")

    expect_error(
        ont_define_transform("txf1", "Duplicate", "ds_out2", "sql", "SELECT 2"),
        "already exists"
    )

    ont_disconnect()
})

test_that("update transform code works", {
    ont_connect(":memory:")

    ont_define_transform("txf1", "Transform", "ds_out", "sql", "SELECT 1 as val")

    ont_update_transform("txf1", "SELECT 2 as val")

    transform <- ont_get_transform("txf1")
    expect_equal(transform$code, "SELECT 2 as val")

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Transform Inputs
# -----------------------------------------------------------------------------

test_that("add transform input works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "table1", tibble::tibble(id = "A"))
    DBI::dbWriteTable(ont_get_connection(), "table2", tibble::tibble(id = "B"))

    ont_register_dataset("ds1", "Dataset 1", "table1", "source")
    ont_register_dataset("ds2", "Dataset 2", "table2", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT a.id, b.id FROM table1 a JOIN table2 b ON 1=1",
                           input_datasets = "ds1")

    ont_add_transform_input("txf1", "ds2", input_role = "join")

    inputs <- ont_get_transform_inputs("txf1")
    expect_equal(nrow(inputs), 2)
    expect_true("ds1" %in% inputs$input_dataset_id)
    expect_true("ds2" %in% inputs$input_dataset_id)

    ont_disconnect()
})

test_that("remove transform input works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "table1", tibble::tibble(id = "A"))
    ont_register_dataset("ds1", "Dataset 1", "table1", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql", "SELECT * FROM table1",
                           input_datasets = "ds1")

    expect_equal(nrow(ont_get_transform_inputs("txf1")), 1)

    ont_remove_transform_input("txf1", "ds1")

    expect_equal(nrow(ont_get_transform_inputs("txf1")), 0)

    ont_disconnect()
})

test_that("get transform inputs returns metadata", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "source_data", tibble::tibble(
        id = c("A", "B", "C"),
        value = c(1, 2, 3)
    ))

    ont_register_dataset("ds_source", "Source Data", "source_data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql", "SELECT * FROM source_data",
                           input_datasets = "ds_source")

    inputs <- ont_get_transform_inputs("txf1")
    expect_equal(inputs$dataset_name, "Source Data")
    expect_equal(inputs$physical_name, "source_data")
    expect_equal(inputs$row_count, 3)

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Transform Execution
# -----------------------------------------------------------------------------

test_that("execute transform works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "raw_orders", tibble::tibble(
        order_id = c("O1", "O2", "O3", "O4"),
        amount = c(50, 150, 250, 350)
    ))

    ont_register_dataset("ds_raw_orders", "Raw Orders", "raw_orders", "source")

    ont_define_transform(
        "txf_big_orders",
        "Big Orders",
        "ds_big_orders",
        "sql",
        "SELECT * FROM raw_orders WHERE amount > 100",
        input_datasets = "ds_raw_orders"
    )

    result <- ont_execute_transform("txf_big_orders")

    expect_equal(result$status, "completed")
    expect_equal(result$row_count, 3)  # O2, O3, O4
    expect_true(grepl("^RUN-", result$run_id))

    # Verify table was created
    output_data <- DBI::dbGetQuery(ont_get_connection(), "SELECT * FROM ds_big_orders")
    expect_equal(nrow(output_data), 3)

    ont_disconnect()
})

test_that("transform execution creates run record", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_dataset("ds_items", "Items", "items", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM items",
                           input_datasets = "ds_items")

    result <- ont_execute_transform("txf1")

    run <- ont_get_run(result$run_id)
    expect_equal(run$status, "completed")
    expect_equal(run$run_type, "transform")
    expect_equal(run$transform_id, "txf1")
    expect_true(!is.na(run$input_snapshot))

    ont_disconnect()
})

test_that("transform execution creates lineage edges", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "source1", tibble::tibble(id = "A", val = 1))
    DBI::dbWriteTable(ont_get_connection(), "source2", tibble::tibble(id = "A", extra = "X"))

    ont_register_dataset("ds_s1", "Source 1", "source1", "source")
    ont_register_dataset("ds_s2", "Source 2", "source2", "source")

    ont_define_transform(
        "txf_join",
        "Join Sources",
        "ds_joined",
        "sql",
        "SELECT a.*, b.extra FROM source1 a JOIN source2 b ON a.id = b.id",
        input_datasets = c("ds_s1", "ds_s2")
    )

    result <- ont_execute_transform("txf_join")

    # Check lineage edges
    edges <- DBI::dbGetQuery(
        ont_get_connection(),
        "SELECT * FROM ont_lineage_edges WHERE run_id = ?",
        params = list(result$run_id)
    )

    expect_equal(nrow(edges), 2)  # One edge from each input
    expect_true("ds_s1" %in% edges$from_dataset_id)
    expect_true("ds_s2" %in% edges$from_dataset_id)
    expect_true(all(edges$to_dataset_id == "ds_joined"))

    ont_disconnect()
})

test_that("transform execution stores input snapshot", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(
        id = c("A", "B", "C"),
        value = c(1, 2, 3)
    ))

    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    result <- ont_execute_transform("txf1")

    run <- ont_get_run(result$run_id)
    snapshot <- jsonlite::fromJSON(run$input_snapshot)

    expect_true("ds_data" %in% names(snapshot))
    expect_equal(snapshot$ds_data$row_count, 3)

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Impact Analysis
# -----------------------------------------------------------------------------

test_that("get impact analysis works", {
    ont_connect(":memory:")

    # Create chain: source -> intermediate -> final
    DBI::dbWriteTable(ont_get_connection(), "source_data", tibble::tibble(id = "A"))
    ont_register_dataset("ds_source", "Source", "source_data", "source")

    ont_define_transform("txf1", "Step 1", "ds_intermediate", "sql",
                           "SELECT * FROM source_data",
                           input_datasets = "ds_source")
    ont_execute_transform("txf1")

    ont_define_transform("txf2", "Step 2", "ds_final", "sql",
                           "SELECT * FROM ds_intermediate",
                           input_datasets = "ds_intermediate")
    ont_execute_transform("txf2")

    impact <- ont_get_impact("ds_source")

    expect_equal(impact$source_dataset, "ds_source")
    expect_equal(nrow(impact$direct_transforms), 1)
    expect_equal(impact$direct_transforms$transform_id, "txf1")
    expect_true(impact$total_affected_datasets >= 2)

    ont_disconnect()
})

test_that("get transform DAG works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(id = "A"))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform 1", "ds_out1", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    dag <- ont_get_transform_dag()

    expect_true(nrow(dag$nodes) >= 2)
    expect_true(nrow(dag$edges) >= 1)
    expect_true("transform" %in% dag$nodes$node_type)
    expect_true("source" %in% dag$nodes$node_type)

    ont_disconnect()
})

test_that("validate DAG detects no cycles in valid graph", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(id = "A"))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    validation <- ont_validate_dag()

    expect_true(validation$is_valid)
    expect_equal(length(validation$cycles), 0)

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Run History and Comparison
# -----------------------------------------------------------------------------

test_that("get dataset runs works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(id = "A"))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    ont_execute_transform("txf1")
    ont_execute_transform("txf1")  # Run again

    # Producer runs
    producer_runs <- ont_get_dataset_runs("ds_out", role = "producer")
    expect_equal(nrow(producer_runs), 2)

    # Consumer runs (none for output)
    consumer_runs <- ont_get_dataset_runs("ds_out", role = "consumer")
    expect_equal(nrow(consumer_runs), 0)

    # Consumer runs for input
    input_consumer <- ont_get_dataset_runs("ds_data", role = "consumer")
    expect_equal(nrow(input_consumer), 2)

    ont_disconnect()
})

test_that("compare runs works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(
        id = c("A", "B"),
        value = c(1, 2)
    ))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    result1 <- ont_execute_transform("txf1")

    # Add more data
    DBI::dbExecute(ont_get_connection(),
                   "INSERT INTO data VALUES ('C', 3)")
    ont_update_dataset_count("ds_data")

    result2 <- ont_execute_transform("txf1")

    comparison <- ont_compare_runs(result1$run_id, result2$run_id)

    expect_true(comparison$same_transform)
    expect_equal(comparison$output_row_diff, 1)  # 3 - 2 = 1

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Edge Cases
# -----------------------------------------------------------------------------

test_that("transform with no inputs executes", {
    ont_connect(":memory:")

    ont_define_transform("txf_standalone", "Standalone", "ds_standalone", "sql",
                           "SELECT 1 as value, 'hello' as text")

    result <- ont_execute_transform("txf_standalone")

    expect_equal(result$status, "completed")
    expect_equal(result$row_count, 1)

    ont_disconnect()
})

test_that("transform execution updates output dataset", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "data", tibble::tibble(
        id = c("A", "B", "C")
    ))
    ont_register_dataset("ds_data", "Data", "data", "source")

    ont_define_transform("txf1", "Transform", "ds_out", "sql",
                           "SELECT * FROM data",
                           input_datasets = "ds_data")

    ont_execute_transform("txf1")

    # Check output dataset was updated
    ds <- ont_get_dataset("ds_out")
    expect_equal(ds$row_count, 3)
    expect_equal(ds$dataset_type, "derived")

    ont_disconnect()
})

test_that("failed transform updates run status", {
    ont_connect(":memory:")

    ont_define_transform("txf_bad", "Bad Transform", "ds_bad", "sql",
                           "SELECT * FROM nonexistent_table")

    expect_error(
        ont_execute_transform("txf_bad"),
        "failed"
    )

    # Check run was recorded as failed
    runs <- ont_list_runs(status = "failed")
    expect_equal(nrow(runs), 1)
    expect_true(!is.na(runs$log))

    ont_disconnect()
})
