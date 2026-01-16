#' @title RBAC-lite and Governance Gates
#' @description Functions for role-based access control and governance gates.
#' Provides lightweight permission management and quality gates for concept lifecycle.
#' @name rbac
NULL

# =============================================================================
# Role Management
# =============================================================================

#' List Available Roles
#'
#' Returns all defined roles with their permissions.
#'
#' @param con Optional DBI connection.
#'
#' @return A tibble of roles.
#'
#' @export
ont_list_roles <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con, "SELECT * FROM ont_roles ORDER BY role_id")
    tibble::as_tibble(result)
}

#' Get Role Details
#'
#' Retrieves a role with its parsed permissions.
#'
#' @param role_id The role identifier.
#' @param con Optional DBI connection.
#'
#' @return A list with role details and permissions as a character vector.
#'
#' @export
ont_get_role <- function(role_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_roles WHERE role_id = ?",
        params = list(role_id)
    )

    if (nrow(result) == 0) {
        return(NULL)
    }

    role <- as.list(result[1, ])
    role$permissions_list <- jsonlite::fromJSON(role$permissions)
    role
}

#' Create Custom Role
#'
#' Creates a new role with specified permissions.
#'
#' @param role_id Unique identifier for the role.
#' @param role_name Human-readable name.
#' @param permissions Character vector of permission strings.
#' @param description Optional description.
#' @param con Optional DBI connection.
#'
#' @return The role_id (invisibly).
#'
#' @export
ont_create_role <- function(role_id,
                              role_name,
                              permissions,
                              description = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    existing <- ont_get_role(role_id, con)
    if (!is.null(existing)) {
        cli::cli_abort("Role {.val {role_id}} already exists.")
    }

    DBI::dbExecute(
        con,
        "INSERT INTO ont_roles (role_id, role_name, description, permissions)
         VALUES (?, ?, ?, ?)",
        params = list(
            role_id,
            role_name,
            null_to_na(description),
            jsonlite::toJSON(permissions)
        )
    )

    cli::cli_alert_success("Created role: {.val {role_id}}")
    invisible(role_id)
}

# =============================================================================
# User Role Assignment
# =============================================================================

#' Grant Role to User
#'
#' Assigns a role to a user, optionally scoped to a domain or concept.
#'
#' @param user_id The user identifier.
#' @param role_id The role to grant.
#' @param scope_type Scope type: "global", "domain", or "concept".
#' @param scope_value The domain name or concept_id if scoped.
#' @param granted_by Who granted this role.
#' @param expires_at Optional expiration timestamp.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_grant_role <- function(user_id,
                             role_id,
                             scope_type = "global",
                             scope_value = NULL,
                             granted_by = NULL,
                             expires_at = NULL,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    # Verify role exists
    role <- ont_get_role(role_id, con)
    if (is.null(role)) {
        cli::cli_abort("Role {.val {role_id}} not found.")
    }

    # Use empty string for NULL scope_value in primary key
    scope_value_db <- scope_value %||% ""

    # Check for existing grant
    existing <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_user_roles WHERE user_id = ? AND role_id = ? AND scope_type = ? AND scope_value = ?",
        params = list(user_id, role_id, scope_type, scope_value_db)
    )

    if (nrow(existing) > 0) {
        cli::cli_warn("User {.val {user_id}} already has role {.val {role_id}} in this scope.")
        return(invisible(TRUE))
    }

    DBI::dbExecute(
        con,
        "INSERT INTO ont_user_roles (user_id, role_id, scope_type, scope_value, granted_by, expires_at)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
            user_id,
            role_id,
            scope_type,
            scope_value_db,
            null_to_na(granted_by),
            null_to_na(expires_at)
        )
    )

    cli::cli_alert_success("Granted role {.val {role_id}} to user {.val {user_id}}")
    invisible(TRUE)
}

#' Revoke Role from User
#'
#' Removes a role assignment from a user.
#'
#' @param user_id The user identifier.
#' @param role_id The role to revoke.
#' @param scope_type Scope type.
#' @param scope_value The scope value if scoped.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_revoke_role <- function(user_id,
                              role_id,
                              scope_type = "global",
                              scope_value = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    scope_value_db <- scope_value %||% ""

    DBI::dbExecute(
        con,
        "DELETE FROM ont_user_roles WHERE user_id = ? AND role_id = ? AND scope_type = ? AND scope_value = ?",
        params = list(user_id, role_id, scope_type, scope_value_db)
    )

    cli::cli_alert_success("Revoked role {.val {role_id}} from user {.val {user_id}}")
    invisible(TRUE)
}

#' Get User Roles
#'
#' Returns all roles assigned to a user.
#'
#' @param user_id The user identifier.
#' @param include_expired Include expired role assignments.
#' @param con Optional DBI connection.
#'
#' @return A tibble of role assignments.
#'
#' @export
ont_get_user_roles <- function(user_id, include_expired = FALSE, con = NULL) {
    con <- con %||% ont_get_connection()

    if (include_expired) {
        query <- "SELECT ur.*, r.role_name, r.permissions
                  FROM ont_user_roles ur
                  JOIN ont_roles r ON ur.role_id = r.role_id
                  WHERE ur.user_id = ?"
    } else {
        query <- "SELECT ur.*, r.role_name, r.permissions
                  FROM ont_user_roles ur
                  JOIN ont_roles r ON ur.role_id = r.role_id
                  WHERE ur.user_id = ?
                  AND (ur.expires_at IS NULL OR ur.expires_at > CURRENT_TIMESTAMP)"
    }

    result <- DBI::dbGetQuery(con, query, params = list(user_id))
    tibble::as_tibble(result)
}

#' Check User Permission
#'
#' Checks if a user has a specific permission, considering all their roles.
#'
#' @param user_id The user identifier.
#' @param permission The permission to check (e.g., "concept:write").
#' @param scope_type Optional scope type for context-specific check.
#' @param scope_value Optional scope value for context-specific check.
#' @param con Optional DBI connection.
#'
#' @return TRUE if user has permission, FALSE otherwise.
#'
#' @export
ont_check_permission <- function(user_id,
                                   permission,
                                   scope_type = NULL,
                                   scope_value = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    roles <- ont_get_user_roles(user_id, con = con)

    if (nrow(roles) == 0) {
        return(FALSE)
    }

    # Check each role's permissions
    for (i in seq_len(nrow(roles))) {
        perms <- jsonlite::fromJSON(roles$permissions[i])

        # Admin wildcard
        if ("*" %in% perms) {
            return(TRUE)
        }

        # Direct match
        if (permission %in% perms) {
            # Check scope if specified
            if (!is.null(scope_type)) {
                role_scope <- roles$scope_type[i]
                role_value <- roles$scope_value[i]

                # Global scope grants access to everything
                if (role_scope == "global") {
                    return(TRUE)
                }

                # Scoped role must match
                if (role_scope == scope_type && (is.null(scope_value) || role_value == scope_value)) {
                    return(TRUE)
                }
            } else {
                return(TRUE)
            }
        }
    }

    FALSE
}

#' Require Permission
#'
#' Throws an error if user doesn't have the required permission.
#' Use this as a guard in functions that need permission checks.
#'
#' @param user_id The user identifier.
#' @param permission The required permission.
#' @param scope_type Optional scope type.
#' @param scope_value Optional scope value.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE if permitted.
#'
#' @export
ont_require_permission <- function(user_id,
                                     permission,
                                     scope_type = NULL,
                                     scope_value = NULL,
                                     con = NULL) {
    has_perm <- ont_check_permission(user_id, permission, scope_type, scope_value, con)

    if (!has_perm) {
        cli::cli_abort("User {.val {user_id}} lacks permission {.val {permission}}.")
    }

    invisible(TRUE)
}

# =============================================================================
# Governance Gates
# =============================================================================

#' List Governance Gates
#'
#' Returns all defined governance gates.
#'
#' @param applies_to Optional filter by action type.
#' @param enabled_only Only return enabled gates.
#' @param con Optional DBI connection.
#'
#' @return A tibble of gates.
#'
#' @export
ont_list_gates <- function(applies_to = NULL, enabled_only = TRUE, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_governance_gates WHERE 1=1"
    params <- list()

    if (!is.null(applies_to)) {
        query <- paste(query, "AND (applies_to = ? OR applies_to = 'all')")
        params <- c(params, applies_to)
    }

    if (enabled_only) {
        query <- paste(query, "AND enabled = TRUE")
    }

    query <- paste(query, "ORDER BY gate_id")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Get Gate Details
#'
#' Retrieves a gate with parsed conditions.
#'
#' @param gate_id The gate identifier.
#' @param con Optional DBI connection.
#'
#' @return A list with gate details and parsed conditions.
#'
#' @export
ont_get_gate <- function(gate_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_governance_gates WHERE gate_id = ?",
        params = list(gate_id)
    )

    if (nrow(result) == 0) {
        return(NULL)
    }

    gate <- as.list(result[1, ])
    gate$conditions <- jsonlite::fromJSON(gate$condition_json)
    gate
}

#' Create Governance Gate
#'
#' Creates a new governance gate.
#'
#' @param gate_id Unique identifier.
#' @param gate_name Human-readable name.
#' @param gate_type Type: "audit_coverage", "drift_threshold", "approval_required", "custom".
#' @param applies_to Action: "activation", "deprecation", "materialization", "all".
#' @param conditions List of conditions for the gate.
#' @param severity "blocking" or "warning".
#' @param scope_filter Optional scope filter.
#' @param domain_filter Optional domain filter.
#' @param description Optional description.
#' @param created_by Optional user.
#' @param con Optional DBI connection.
#'
#' @return The gate_id (invisibly).
#'
#' @export
ont_create_gate <- function(gate_id,
                              gate_name,
                              gate_type,
                              applies_to,
                              conditions,
                              severity = "blocking",
                              scope_filter = NULL,
                              domain_filter = NULL,
                              description = NULL,
                              created_by = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    existing <- ont_get_gate(gate_id, con)
    if (!is.null(existing)) {
        cli::cli_abort("Gate {.val {gate_id}} already exists.")
    }

    DBI::dbExecute(
        con,
        "INSERT INTO ont_governance_gates (gate_id, gate_name, gate_type, applies_to,
         condition_json, severity, scope_filter, domain_filter, description, created_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            gate_id,
            gate_name,
            gate_type,
            applies_to,
            jsonlite::toJSON(conditions, auto_unbox = TRUE),
            severity,
            null_to_na(scope_filter),
            null_to_na(domain_filter),
            null_to_na(description),
            null_to_na(created_by)
        )
    )

    cli::cli_alert_success("Created gate: {.val {gate_id}}")
    invisible(gate_id)
}

#' Enable/Disable Gate
#'
#' Toggles a gate's enabled status.
#'
#' @param gate_id The gate identifier.
#' @param enabled TRUE to enable, FALSE to disable.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_toggle_gate <- function(gate_id, enabled, con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "UPDATE ont_governance_gates SET enabled = ? WHERE gate_id = ?",
        params = list(enabled, gate_id)
    )

    status <- if (enabled) "enabled" else "disabled"
    cli::cli_alert_success("Gate {.val {gate_id}} is now {status}")
    invisible(TRUE)
}

#' Check Gate
#'
#' Evaluates a gate for a concept version.
#'
#' @param gate_id The gate to check.
#' @param concept_id The concept.
#' @param scope The scope.
#' @param version The version.
#' @param action_type The action being attempted.
#' @param checked_by User performing the check.
#' @param con Optional DBI connection.
#'
#' @return A list with passed (boolean), details, and check_id.
#'
#' @export
ont_check_gate <- function(gate_id,
                             concept_id,
                             scope,
                             version,
                             action_type,
                             checked_by = NULL,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    gate <- ont_get_gate(gate_id, con)
    if (is.null(gate)) {
        cli::cli_abort("Gate {.val {gate_id}} not found.")
    }

    if (!gate$enabled) {
        return(list(passed = TRUE, reason = "Gate is disabled", check_id = NULL))
    }

    # Evaluate based on gate type
    result <- switch(
        gate$gate_type,
        "audit_coverage" = check_audit_coverage_gate(gate, concept_id, scope, version, con),
        "drift_threshold" = check_drift_threshold_gate(gate, concept_id, scope, version, con),
        "approval_required" = check_approval_gate(gate, concept_id, scope, version, con),
        "custom" = list(passed = TRUE, details = "Custom gates require manual check")
    )

    # Record the check
    check_id <- paste0("CHK-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))

    DBI::dbExecute(
        con,
        "INSERT INTO ont_gate_checks (check_id, gate_id, concept_id, scope, version,
         action_type, check_result, check_details, checked_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            check_id,
            gate_id,
            concept_id,
            scope,
            version,
            action_type,
            if (result$passed) "passed" else "failed",
            jsonlite::toJSON(result$details, auto_unbox = TRUE),
            null_to_na(checked_by)
        )
    )

    list(
        passed = result$passed,
        gate_id = gate_id,
        gate_name = gate$gate_name,
        severity = gate$severity,
        details = result$details,
        check_id = check_id
    )
}

# Internal: Check audit coverage gate
check_audit_coverage_gate <- function(gate, concept_id, scope, version, con) {
    conditions <- gate$conditions

    # Get audit stats
    stats <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) as total,
                SUM(CASE WHEN system_value = reviewer_value THEN 1 ELSE 0 END) as agreements
         FROM ont_audits
         WHERE concept_id = ? AND scope = ? AND version = ?",
        params = list(concept_id, scope, version)
    )

    total <- stats$total
    agreements <- stats$agreements %||% 0
    agreement_rate <- if (total > 0) agreements / total else 0

    min_audits <- conditions$min_audits %||% 0
    min_rate <- conditions$min_agreement_rate %||% 0

    passed <- total >= min_audits && agreement_rate >= min_rate

    list(
        passed = passed,
        details = list(
            audit_count = total,
            required_audits = min_audits,
            agreement_rate = agreement_rate,
            required_rate = min_rate
        )
    )
}

# Internal: Check drift threshold gate
check_drift_threshold_gate <- function(gate, concept_id, scope, version, con) {
    conditions <- gate$conditions

    # Count open drift events
    open_drift <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) as n FROM ont_drift_events
         WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'",
        params = list(concept_id, scope, version)
    )$n

    max_open <- conditions$max_open_drift_events %||% 0

    passed <- open_drift <= max_open

    list(
        passed = passed,
        details = list(
            open_drift_events = open_drift,
            max_allowed = max_open
        )
    )
}

# Internal: Check approval gate
check_approval_gate <- function(gate, concept_id, scope, version, con) {
    conditions <- gate$conditions

    # Count approved requests
    approved <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) as n FROM ont_approval_requests
         WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'approved'",
        params = list(concept_id, scope, version)
    )$n

    min_approvals <- conditions$min_approvals %||% 1

    passed <- approved >= min_approvals

    list(
        passed = passed,
        details = list(
            approvals = approved,
            required = min_approvals
        )
    )
}

#' Check All Gates
#'
#' Evaluates all applicable gates for an action on a concept version.
#'
#' @param concept_id The concept.
#' @param scope The scope.
#' @param version The version.
#' @param action_type The action being attempted.
#' @param checked_by User performing the check.
#' @param con Optional DBI connection.
#'
#' @return A list with overall_passed, blocking_failures, warnings, and all_checks.
#'
#' @export
ont_check_all_gates <- function(concept_id,
                                  scope,
                                  version,
                                  action_type,
                                  checked_by = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    gates <- ont_list_gates(applies_to = action_type, enabled_only = TRUE, con = con)

    if (nrow(gates) == 0) {
        return(list(
            overall_passed = TRUE,
            blocking_failures = list(),
            warnings = list(),
            all_checks = list()
        ))
    }

    all_checks <- list()
    blocking_failures <- list()
    warnings <- list()

    for (i in seq_len(nrow(gates))) {
        gate_id <- gates$gate_id[i]

        result <- ont_check_gate(
            gate_id, concept_id, scope, version,
            action_type, checked_by, con
        )

        all_checks[[gate_id]] <- result

        if (!result$passed) {
            if (result$severity == "blocking") {
                blocking_failures[[gate_id]] <- result
            } else {
                warnings[[gate_id]] <- result
            }
        }
    }

    overall_passed <- length(blocking_failures) == 0

    list(
        overall_passed = overall_passed,
        blocking_failures = blocking_failures,
        warnings = warnings,
        all_checks = all_checks
    )
}

#' Override Gate Check
#'
#' Records a gate override with reason. Requires gate:override permission.
#'
#' @param check_id The check to override.
#' @param override_reason Reason for the override.
#' @param overridden_by User overriding.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_override_gate <- function(check_id, override_reason, overridden_by, con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "UPDATE ont_gate_checks SET check_result = 'overridden', override_reason = ?, checked_by = ?
         WHERE check_id = ?",
        params = list(override_reason, overridden_by, check_id)
    )

    cli::cli_alert_success("Gate check {.val {check_id}} overridden")
    invisible(TRUE)
}

# =============================================================================
# Approval Workflow
# =============================================================================

#' Request Approval
#'
#' Creates an approval request for a concept version action.
#'
#' @param concept_id The concept.
#' @param scope The scope.
#' @param version The version.
#' @param requested_action The action: "activate", "deprecate", "retire".
#' @param requested_by User requesting.
#' @param con Optional DBI connection.
#'
#' @return The request_id.
#'
#' @export
ont_request_approval <- function(concept_id,
                                   scope,
                                   version,
                                   requested_action,
                                   requested_by = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    # Verify concept version exists
    cv <- ont_get_version(concept_id, scope, version, con = con)
    if (is.null(cv)) {
        cli::cli_abort("Concept version not found.")
    }

    request_id <- paste0("REQ-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))

    DBI::dbExecute(
        con,
        "INSERT INTO ont_approval_requests (request_id, concept_id, scope, version,
         requested_action, requested_by)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
            request_id,
            concept_id,
            scope,
            version,
            requested_action,
            null_to_na(requested_by)
        )
    )

    cli::cli_alert_success("Created approval request: {.val {request_id}}")
    request_id
}

#' List Pending Approvals
#'
#' Returns all pending approval requests.
#'
#' @param concept_id Optional filter by concept.
#' @param con Optional DBI connection.
#'
#' @return A tibble of pending approvals.
#'
#' @export
ont_list_pending_approvals <- function(concept_id = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_approval_requests WHERE status = 'pending'"
    params <- list()

    if (!is.null(concept_id)) {
        query <- paste(query, "AND concept_id = ?")
        params <- c(params, concept_id)
    }

    query <- paste(query, "ORDER BY requested_at DESC")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Approve Request
#'
#' Approves a pending approval request.
#'
#' @param request_id The request to approve.
#' @param decided_by User approving.
#' @param decision_notes Optional notes.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_approve_request <- function(request_id,
                                  decided_by,
                                  decision_notes = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "UPDATE ont_approval_requests
         SET status = 'approved', decided_at = CURRENT_TIMESTAMP, decided_by = ?, decision_notes = ?
         WHERE request_id = ? AND status = 'pending'",
        params = list(decided_by, null_to_na(decision_notes), request_id)
    )

    cli::cli_alert_success("Approved request: {.val {request_id}}")
    invisible(TRUE)
}

#' Reject Request
#'
#' Rejects a pending approval request.
#'
#' @param request_id The request to reject.
#' @param decided_by User rejecting.
#' @param decision_notes Reason for rejection.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_reject_request <- function(request_id,
                                 decided_by,
                                 decision_notes,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "UPDATE ont_approval_requests
         SET status = 'rejected', decided_at = CURRENT_TIMESTAMP, decided_by = ?, decision_notes = ?
         WHERE request_id = ? AND status = 'pending'",
        params = list(decided_by, decision_notes, request_id)
    )

    cli::cli_alert_success("Rejected request: {.val {request_id}}")
    invisible(TRUE)
}

#' Get Gate Check History
#'
#' Returns the history of gate checks for a concept version.
#'
#' @param concept_id The concept.
#' @param scope The scope.
#' @param version The version.
#' @param con Optional DBI connection.
#'
#' @return A tibble of gate checks.
#'
#' @export
ont_get_gate_history <- function(concept_id, scope, version, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT gc.*, g.gate_name, g.gate_type, g.severity
         FROM ont_gate_checks gc
         JOIN ont_governance_gates g ON gc.gate_id = g.gate_id
         WHERE gc.concept_id = ? AND gc.scope = ? AND gc.version = ?
         ORDER BY gc.checked_at DESC",
        params = list(concept_id, scope, version)
    )

    tibble::as_tibble(result)
}
