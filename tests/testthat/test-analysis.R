# =============================================================================
# Tests for Observations and Analysis
# =============================================================================

# -----------------------------------------------------------------------------
# Observations
# -----------------------------------------------------------------------------

test_that("observation recording works", {
  ont_connect(":memory:")

  # Setup
  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D", "E"),
    value = c(10, 20, 30, 40, 50)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 25", status = "active")

  # Record observation
  obs <- ont_observe(
    concept_id = "high_value",
    scope = "test",
    triggered_by = "test_run"
  )

  expect_true(grepl("^OBS-", obs$observation_id))
  expect_equal(obs$total_objects, 5)
  expect_equal(obs$concept_true, 3)  # C, D, E
  expect_equal(obs$concept_false, 2)  # A, B
  expect_equal(obs$prevalence_rate, 0.6)

  ont_disconnect()
})

test_that("observation with filter works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D"),
    value = c(10, 20, 30, 40),
    category = c("X", "X", "Y", "Y")
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  # Observe only category X
  obs <- ont_observe(
    concept_id = "high_value",
    scope = "test",
    filter_expr = "category = 'X'"
  )

  expect_equal(obs$total_objects, 2)  # Only A and B
  expect_equal(obs$concept_true, 1)   # Only B (value 20 > 15)

  ont_disconnect()
})

test_that("observation with details storage works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  # Record with details
  obs <- ont_observe(
    concept_id = "high_value",
    scope = "test",
    store_details = TRUE
  )

  details <- ont_get_observation_details(obs$observation_id)
  expect_equal(nrow(details), 3)
  expect_true("A" %in% details$object_key)

  ont_disconnect()
})

test_that("list observations works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B"),
    value = c(10, 20)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "value > 5", status = "active")

  # Record multiple observations
  ont_observe("test_concept", "test")
  ont_observe("test_concept", "test")
  ont_observe("test_concept", "test")

  observations <- ont_list_observations(concept_id = "test_concept")
  expect_equal(nrow(observations), 3)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Cohorts
# -----------------------------------------------------------------------------

test_that("cohort definition works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    category = c("X", "X", "Y")
  ))
  ont_register_object("Item", "items", "item_id")

  # Define SQL-based cohort
  ont_define_cohort(
    cohort_id = "category_x",
    cohort_name = "Category X Items",
    object_type = "Item",
    sql_expr = "category = 'X'"
  )

  cohorts <- ont_list_cohorts()
  expect_equal(nrow(cohorts), 1)
  expect_equal(cohorts$cohort_name, "Category X Items")

  # Get members
  members <- ont_get_cohort_members("category_x")
  expect_equal(length(members), 2)
  expect_true("A" %in% members)
  expect_true("B" %in% members)

  ont_disconnect()
})

test_that("explicit cohort works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D"),
    value = c(1, 2, 3, 4)
  ))
  ont_register_object("Item", "items", "item_id")

  # Define explicit cohort
  ont_define_cohort(
    cohort_id = "selected_items",
    cohort_name = "Selected Items",
    object_type = "Item"
  )

  # Add members
  ont_add_cohort_members("selected_items", c("A", "C"))

  members <- ont_get_cohort_members("selected_items")
  expect_equal(length(members), 2)
  expect_true("A" %in% members)
  expect_true("C" %in% members)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Trend Analysis
# -----------------------------------------------------------------------------

test_that("trend analysis works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C"),
    value = c(10, 20, 30)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  # Record multiple observations
  ont_observe("high_value", "test")
  ont_observe("high_value", "test")
  ont_observe("high_value", "test")

  # Run trend analysis
  trend <- ont_trend_analysis("high_value", "test")

  expect_true(nrow(trend) >= 1)
  expect_true("avg_prevalence" %in% names(trend))

  ont_disconnect()
})

test_that("trend analysis with no observations returns empty", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  # No observations recorded
  trend <- ont_trend_analysis("test_concept", "test")
  expect_equal(nrow(trend), 0)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Version Comparison
# -----------------------------------------------------------------------------

test_that("version comparison works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D"),
    value = c(10, 20, 30, 40)
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")

  # Two versions with different thresholds
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")
  ont_add_version("high_value", "test", 2, "value > 25", status = "draft")

  comparison <- ont_version_compare("high_value", "test", versions = c(1, 2))

  expect_equal(comparison$concept_id, "high_value")
  expect_equal(length(comparison$versions), 2)
  expect_true("comparison" %in% names(comparison))
  expect_true("agreements" %in% names(comparison))

  # v1: B, C, D are TRUE (3)
  # v2: C, D are TRUE (2)
  expect_equal(unname(comparison$summary$true_count[1]), 3)
  expect_equal(unname(comparison$summary$true_count[2]), 2)

  ont_disconnect()
})

test_that("version comparison requires at least 2 versions", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(item_id = "A"))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")
  ont_add_version("test_concept", "test", 1, "TRUE", status = "active")

  expect_error(
    ont_version_compare("test_concept", "test", versions = c(1)),
    "at least 2 versions"
  )

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Cohort Comparison
# -----------------------------------------------------------------------------

test_that("cohort comparison works", {
  ont_connect(":memory:")

  DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(
    item_id = c("A", "B", "C", "D"),
    value = c(10, 20, 30, 40),
    category = c("X", "X", "Y", "Y")
  ))
  ont_register_object("Item", "items", "item_id")
  ont_define_concept("high_value", "Item")
  ont_add_version("high_value", "test", 1, "value > 15", status = "active")

  # Define cohorts
  ont_define_cohort("cat_x", "Category X", "Item", sql_expr = "category = 'X'")
  ont_define_cohort("cat_y", "Category Y", "Item", sql_expr = "category = 'Y'")

  comparison <- ont_cohort_compare(
    concept_id = "high_value",
    scope = "test",
    cohort_ids = c("cat_x", "cat_y")
  )

  expect_equal(nrow(comparison), 2)

  # Category X: A=10 (F), B=20 (T) -> 1/2 = 50%
  # Category Y: C=30 (T), D=40 (T) -> 2/2 = 100%
  cat_x_row <- comparison[comparison$cohort_id == "cat_x", ]
  cat_y_row <- comparison[comparison$cohort_id == "cat_y", ]

  expect_equal(cat_x_row$prevalence_rate, 0.5)
  expect_equal(cat_y_row$prevalence_rate, 1.0)

  ont_disconnect()
})

# -----------------------------------------------------------------------------
# Analysis Logging
# -----------------------------------------------------------------------------

test_that("analysis logging works", {
  ont_connect(":memory:")

  ont_register_object("Item", "items", "item_id")
  ont_define_concept("test_concept", "Item")

  analysis_id <- ont_log_analysis(
    analysis_type = "trend",
    concept_id = "test_concept",
    scope = "test",
    parameters = list(granularity = "day", from = "2024-01-01"),
    results_summary = list(periods = 30, avg_prevalence = 0.65),
    executed_by = "analyst"
  )

  expect_true(grepl("^ANALYSIS-", analysis_id))

  analyses <- ont_list_analyses(concept_id = "test_concept")
  expect_equal(nrow(analyses), 1)
  expect_equal(analyses$analysis_type, "trend")

  ont_disconnect()
})
