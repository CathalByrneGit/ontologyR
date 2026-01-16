# =============================================================================
# Tests for RBAC-lite and Governance Gates
# =============================================================================

# -----------------------------------------------------------------------------
# Role Management
# -----------------------------------------------------------------------------

test_that("default roles are created", {
    ont_connect(":memory:")

    roles <- ont_list_roles()
    expect_true(nrow(roles) >= 4)
    expect_true("viewer" %in% roles$role_id)
    expect_true("editor" %in% roles$role_id)
    expect_true("approver" %in% roles$role_id)
    expect_true("admin" %in% roles$role_id)

    ont_disconnect()
})

test_that("get role returns parsed permissions", {
    ont_connect(":memory:")

    role <- ont_get_role("editor")
    expect_equal(role$role_id, "editor")
    expect_true(is.character(role$permissions_list))
    expect_true("concept:write" %in% role$permissions_list)

    ont_disconnect()
})

test_that("create custom role works", {
    ont_connect(":memory:")

    ont_create_role(
        "data_steward",
        "Data Steward",
        c("concept:read", "concept:write", "audit:read", "audit:write"),
        "Can manage concept audits"
    )

    role <- ont_get_role("data_steward")
    expect_equal(role$role_id, "data_steward")
    expect_true("audit:write" %in% role$permissions_list)

    ont_disconnect()
})

test_that("duplicate role creation fails", {
    ont_connect(":memory:")

    expect_error(
        ont_create_role("viewer", "Duplicate Viewer", c("concept:read")),
        "already exists"
    )

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# User Role Assignment
# -----------------------------------------------------------------------------

test_that("grant role works", {
    ont_connect(":memory:")

    ont_grant_role("user123", "editor", granted_by = "admin")

    roles <- ont_get_user_roles("user123")
    expect_equal(nrow(roles), 1)
    expect_equal(roles$role_id, "editor")

    ont_disconnect()
})

test_that("grant scoped role works", {
    ont_connect(":memory:")

    ont_grant_role("user456", "editor", scope_type = "domain", scope_value = "clinical")

    roles <- ont_get_user_roles("user456")
    expect_equal(roles$scope_type, "domain")
    expect_equal(roles$scope_value, "clinical")

    ont_disconnect()
})

test_that("revoke role works", {
    ont_connect(":memory:")

    ont_grant_role("user789", "viewer")
    expect_equal(nrow(ont_get_user_roles("user789")), 1)

    ont_revoke_role("user789", "viewer")
    expect_equal(nrow(ont_get_user_roles("user789")), 0)

    ont_disconnect()
})

test_that("multiple roles per user works", {
    ont_connect(":memory:")

    ont_grant_role("multiuser", "viewer")
    ont_grant_role("multiuser", "editor")

    roles <- ont_get_user_roles("multiuser")
    expect_equal(nrow(roles), 2)

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Permission Checking
# -----------------------------------------------------------------------------

test_that("check permission for viewer", {
    ont_connect(":memory:")

    ont_grant_role("view_user", "viewer")

    expect_true(ont_check_permission("view_user", "concept:read"))
    expect_false(ont_check_permission("view_user", "concept:write"))

    ont_disconnect()
})

test_that("check permission for editor", {
    ont_connect(":memory:")

    ont_grant_role("edit_user", "editor")

    expect_true(ont_check_permission("edit_user", "concept:read"))
    expect_true(ont_check_permission("edit_user", "concept:write"))
    expect_false(ont_check_permission("edit_user", "concept:activate"))

    ont_disconnect()
})

test_that("admin has wildcard access", {
    ont_connect(":memory:")

    ont_grant_role("admin_user", "admin")

    expect_true(ont_check_permission("admin_user", "concept:read"))
    expect_true(ont_check_permission("admin_user", "concept:write"))
    expect_true(ont_check_permission("admin_user", "anything:at:all"))

    ont_disconnect()
})

test_that("no role returns no permission", {
    ont_connect(":memory:")

    expect_false(ont_check_permission("unknown_user", "concept:read"))

    ont_disconnect()
})

test_that("require permission throws on missing", {
    ont_connect(":memory:")

    ont_grant_role("limited_user", "viewer")

    expect_silent(ont_require_permission("limited_user", "concept:read"))
    expect_error(
        ont_require_permission("limited_user", "concept:write"),
        "lacks permission"
    )

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Governance Gates
# -----------------------------------------------------------------------------

test_that("default gates are created", {
    ont_connect(":memory:")

    gates <- ont_list_gates()
    expect_true(nrow(gates) >= 3)
    expect_true("gate_audit_coverage" %in% gates$gate_id)
    expect_true("gate_no_open_drift" %in% gates$gate_id)
    expect_true("gate_approval_required" %in% gates$gate_id)

    ont_disconnect()
})

test_that("get gate returns parsed conditions", {
    ont_connect(":memory:")

    gate <- ont_get_gate("gate_audit_coverage")
    expect_equal(gate$gate_type, "audit_coverage")
    expect_true("min_audits" %in% names(gate$conditions))

    ont_disconnect()
})

test_that("create custom gate works", {
    ont_connect(":memory:")

    ont_create_gate(
        "custom_gate",
        "Custom Quality Gate",
        "custom",
        "activation",
        list(custom_check = TRUE),
        severity = "warning"
    )

    gate <- ont_get_gate("custom_gate")
    expect_equal(gate$gate_id, "custom_gate")
    expect_equal(gate$severity, "warning")

    ont_disconnect()
})

test_that("toggle gate works", {
    ont_connect(":memory:")

    ont_toggle_gate("gate_audit_coverage", FALSE)

    gate <- ont_get_gate("gate_audit_coverage")
    expect_false(as.logical(gate$enabled))

    ont_toggle_gate("gate_audit_coverage", TRUE)
    gate <- ont_get_gate("gate_audit_coverage")
    expect_true(as.logical(gate$enabled))

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Gate Checking
# -----------------------------------------------------------------------------

test_that("audit coverage gate fails without audits", {
    ont_connect(":memory:")

    # Setup concept
    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_gate(
        "gate_audit_coverage",
        "test_concept", "prod", 1,
        "activation"
    )

    expect_false(result$passed)
    expect_equal(result$details$audit_count, 0)

    ont_disconnect()
})

test_that("audit coverage gate passes with sufficient audits", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    # Create enough audits with high agreement
    for (i in 1:15) {
        ont_record_audit(
            "test_concept", "prod", 1,
            paste0("key", i),
            system_value = TRUE,
            reviewer_value = TRUE,
            reviewer_id = "reviewer1"
        )
    }

    result <- ont_check_gate(
        "gate_audit_coverage",
        "test_concept", "prod", 1,
        "activation"
    )

    expect_true(result$passed)
    expect_equal(result$details$audit_count, 15)
    expect_equal(result$details$agreement_rate, 1.0)

    ont_disconnect()
})

test_that("drift threshold gate passes with no drift", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_gate(
        "gate_no_open_drift",
        "test_concept", "prod", 1,
        "activation"
    )

    expect_true(result$passed)
    expect_equal(result$details$open_drift_events, 0)

    ont_disconnect()
})

test_that("approval gate fails without approvals", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_gate(
        "gate_approval_required",
        "test_concept", "prod", 1,
        "activation"
    )

    expect_false(result$passed)
    expect_equal(result$details$approvals, 0)

    ont_disconnect()
})

test_that("check all gates works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_all_gates("test_concept", "prod", 1, "activation")

    expect_false(result$overall_passed)
    expect_true(length(result$blocking_failures) > 0)
    expect_true(length(result$all_checks) >= 3)

    ont_disconnect()
})

test_that("disabled gate is skipped", {
    ont_connect(":memory:")

    ont_toggle_gate("gate_audit_coverage", FALSE)

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_gate(
        "gate_audit_coverage",
        "test_concept", "prod", 1,
        "activation"
    )

    expect_true(result$passed)
    expect_equal(result$reason, "Gate is disabled")

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Gate Override
# -----------------------------------------------------------------------------

test_that("override gate check works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    result <- ont_check_gate(
        "gate_audit_coverage",
        "test_concept", "prod", 1,
        "activation",
        checked_by = "user1"
    )

    ont_override_gate(
        result$check_id,
        "Emergency deployment approved by CTO",
        "admin_user"
    )

    # Verify override was recorded
    history <- ont_get_gate_history("test_concept", "prod", 1)
    override_check <- history[history$check_id == result$check_id, ]
    expect_equal(override_check$check_result, "overridden")

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Approval Workflow
# -----------------------------------------------------------------------------

test_that("request approval works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    request_id <- ont_request_approval(
        "test_concept", "prod", 1,
        "activate",
        requested_by = "developer1"
    )

    expect_true(grepl("^REQ-", request_id))

    pending <- ont_list_pending_approvals()
    expect_equal(nrow(pending), 1)
    expect_equal(pending$status, "pending")

    ont_disconnect()
})

test_that("approve request works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    request_id <- ont_request_approval("test_concept", "prod", 1, "activate")

    ont_approve_request(request_id, "approver1", "Looks good")

    pending <- ont_list_pending_approvals()
    expect_equal(nrow(pending), 0)

    # Check approval gate now passes
    result <- ont_check_gate(
        "gate_approval_required",
        "test_concept", "prod", 1,
        "activation"
    )
    expect_true(result$passed)

    ont_disconnect()
})

test_that("reject request works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    request_id <- ont_request_approval("test_concept", "prod", 1, "activate")

    ont_reject_request(request_id, "approver1", "Needs more testing")

    pending <- ont_list_pending_approvals()
    expect_equal(nrow(pending), 0)

    # Approval gate still fails
    result <- ont_check_gate(
        "gate_approval_required",
        "test_concept", "prod", 1,
        "activation"
    )
    expect_false(result$passed)

    ont_disconnect()
})

test_that("list pending approvals with filter works", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("concept1", "Item")
    ont_add_version("concept1", "prod", 1, "TRUE", status = "draft")
    ont_define_concept("concept2", "Item")
    ont_add_version("concept2", "prod", 1, "TRUE", status = "draft")

    ont_request_approval("concept1", "prod", 1, "activate")
    ont_request_approval("concept2", "prod", 1, "activate")

    all_pending <- ont_list_pending_approvals()
    expect_equal(nrow(all_pending), 2)

    filtered <- ont_list_pending_approvals(concept_id = "concept1")
    expect_equal(nrow(filtered), 1)

    ont_disconnect()
})

# -----------------------------------------------------------------------------
# Gate Check History
# -----------------------------------------------------------------------------

test_that("gate history is recorded", {
    ont_connect(":memory:")

    DBI::dbWriteTable(ont_get_connection(), "items", tibble::tibble(id = "A"))
    ont_register_object("Item", "items", "id")
    ont_define_concept("test_concept", "Item")
    ont_add_version("test_concept", "prod", 1, "TRUE", status = "draft")

    # Multiple gate checks
    ont_check_gate("gate_audit_coverage", "test_concept", "prod", 1, "activation")
    ont_check_gate("gate_no_open_drift", "test_concept", "prod", 1, "activation")

    history <- ont_get_gate_history("test_concept", "prod", 1)
    expect_equal(nrow(history), 2)
    expect_true("gate_name" %in% names(history))

    ont_disconnect()
})
