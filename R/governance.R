#' @title Governance Functions
#' @description Functions for logging governance actions, enforcing policies,
#'   and tracking the audit trail of ontology changes.
#' @name governance
NULL

#' Log a Governance Action
#'
#' Records a governance action in the audit trail. This is called internally
#' by other functions (e.g., `ont_activate_version`, `ont_resolve_drift`) but
#' can also be called directly for custom actions.
#'
#' @param action_type Character. Type of action: "adopt", "activate",
#'   "deprecate", "retire", "review", "block", "unblock", "drift_detected",
#'   "drift_resolved", etc.
#' @param concept_id Character. The concept involved.
#' @param scope Character. Optional scope.
#' @param version Integer. Optional version.
#' @param actor Character. Who performed the action.
#' @param rationale Character. Optional explanation.
#' @param evidence Character. Optional JSON string with supporting data.
#' @param blocked_by Character. Optional reference to blocking condition.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the log_id.
#'
#' @export
ont_log_governance <- function(action_type,
                                concept_id,
                                scope = NULL,
                                version = NULL,
                                actor,
                                rationale = NULL,
                                evidence = NULL,
                                blocked_by = NULL,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    log_id <- paste0("GOV-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                     substr(digest::digest(runif(1)), 1, 6))

    DBI::dbExecute(con, "
        INSERT INTO ont_governance_log
        (log_id, action_type, concept_id, scope, version, actor, action_at, rationale, evidence, blocked_by)
        VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?, ?, ?)
    ", params = list(
        log_id, action_type, concept_id, scope,
        if (!is.null(version)) as.integer(version) else NULL,
        actor, rationale, evidence, blocked_by
    ))

    invisible(log_id)
}

#' Get Governance Log
#'
#' Retrieves governance action history.
#'
#' @param concept_id Character. Optional filter by concept.
#' @param action_type Character. Optional filter by action type.
#' @param actor Character. Optional filter by actor.
#' @param from POSIXct or Date. Start of date range.
#' @param to POSIXct or Date. End of date range.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of governance log entries.
#'
#' @export
ont_get_governance_log <- function(concept_id = NULL,
                                    action_type = NULL,
                                    actor = NULL,
                                    from = NULL,
                                    to = NULL,
                                    con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_governance_log")

    if (!is.null(concept_id)) {
        result <- result |> dplyr::filter(.data$concept_id == !!concept_id)
    }

    if (!is.null(action_type)) {
        result <- result |> dplyr::filter(.data$action_type == !!action_type)
    }

    if (!is.null(actor)) {
        result <- result |> dplyr::filter(.data$actor == !!actor)
    }

    if (!is.null(from)) {
        result <- result |> dplyr::filter(.data$action_at >= !!from)
    }

    if (!is.null(to)) {
        result <- result |> dplyr::filter(.data$action_at <= !!to)
    }

    result |>
        dplyr::arrange(dplyr::desc(.data$action_at)) |>
        dplyr::collect()
}

#' Register Dashboard Dependency
#'
#' Records that a dashboard or report depends on a specific concept version.
#' This enables impact analysis when definitions change.
#'
#' @param dashboard_id Character. Unique identifier for the dashboard.
#' @param concept_id Character. The concept being used.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param dashboard_name Character. Optional human-readable name.
#' @param registered_by Character. Optional registrant identifier.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
ont_register_dashboard <- function(dashboard_id,
                                    concept_id,
                                    scope,
                                    version,
                                    dashboard_name = NULL,
                                    registered_by = NULL,
                                    con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate concept version exists
    ont_get_version(concept_id, scope, version, con)

    DBI::dbExecute(con, "
        INSERT OR REPLACE INTO ont_dashboard_registry
        (dashboard_id, dashboard_name, concept_id, scope, version, registered_at, registered_by)
        VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP, ?)
    ", params = list(
        dashboard_id, dashboard_name, concept_id, scope,
        as.integer(version), registered_by
    ))

    cli::cli_alert_success(
        "Registered dashboard {.val {dashboard_id}} -> {concept_id}@{scope} v{version}"
    )
    invisible(TRUE)
}

#' List Dashboard Dependencies
#'
#' Returns dashboards that depend on a concept version.
#'
#' @param concept_id Character. Optional filter by concept.
#' @param scope Character. Optional filter by scope.
#' @param version Integer. Optional filter by version.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of dashboard dependencies.
#'
#' @export
ont_list_dashboards <- function(concept_id = NULL,
                                 scope = NULL,
                                 version = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_dashboard_registry")

    if (!is.null(concept_id)) {
        result <- result |> dplyr::filter(.data$concept_id == !!concept_id)
    }

    if (!is.null(scope)) {
        result <- result |> dplyr::filter(.data$scope == !!scope)
    }

    if (!is.null(version)) {
        result <- result |> dplyr::filter(.data$version == !!as.integer(version))
    }

    result |> dplyr::collect()
}

#' Analyze Impact of Deprecation
#'
#' Shows what dashboards/reports would be affected if a concept version
#' is deprecated.
#'
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version to analyze.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with impact analysis.
#'
#' @export
ont_deprecation_impact <- function(concept_id, scope, version, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get dependent dashboards
    dashboards <- ont_list_dashboards(concept_id, scope, version, con)

    # Get open drift events
    drift_events <- ont_list_drift_events(concept_id, status = "open", con) |>
        dplyr::filter(.data$scope == !!scope, .data$version == !!version)

    # Get audit summary
    audit_summary <- ont_audit_summary(concept_id, scope, version, con = con)

    result <- list(
        concept_id = concept_id,
        scope = scope,
        version = version,
        affected_dashboards = dashboards,
        open_drift_events = drift_events,
        audit_summary = audit_summary,
        can_deprecate = nrow(drift_events) == 0,
        blocking_reason = if (nrow(drift_events) > 0) "Open drift events" else NULL
    )

    # Display impact
    cli::cli_h2("Deprecation Impact: {concept_id}@{scope} v{version}")

    if (nrow(dashboards) > 0) {
        cli::cli_alert_warning("Affected dashboards: {nrow(dashboards)}")
        cli::cli_ul(dashboards$dashboard_id)
    } else {
        cli::cli_alert_success("No registered dashboards affected")
    }

    if (nrow(drift_events) > 0) {
        cli::cli_alert_danger("Open drift events: {nrow(drift_events)} - BLOCKING")
    } else {
        cli::cli_alert_success("No open drift events")
    }

    if (!is.na(audit_summary$disagreement_rate)) {
        cli::cli_alert_info(
            "Audit disagreement rate: {round(audit_summary$disagreement_rate*100,1)}%"
        )
    }

    result
}

#' Check Governance Policies
#'
#' Validates that a proposed action complies with governance policies.
#'
#' @param action Character. Proposed action: "activate", "deprecate", "adopt".
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with `allowed` (logical) and `reasons` (character vector).
#'
#' @export
ont_check_policy <- function(action, concept_id, scope, version, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- list(
        action = action,
        concept_id = concept_id,
        scope = scope,
        version = version,
        allowed = TRUE,
        warnings = character(),
        blockers = character()
    )

    # Get version details
    cv <- tryCatch(
        ont_get_version(concept_id, scope, version, con),
        error = function(e) NULL
    )

    if (is.null(cv)) {
        result$allowed <- FALSE
        result$blockers <- c(result$blockers, "Concept version does not exist")
        return(result)
    }

    # Action-specific policies
    if (action == "deprecate") {
        # Check for open drift events
        open_drift <- DBI::dbGetQuery(con, "
            SELECT COUNT(*) as n FROM ont_drift_events
            WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'
        ", params = list(concept_id, scope, as.integer(version)))$n

        if (open_drift > 0) {
            result$allowed <- FALSE
            result$blockers <- c(result$blockers,
                glue::glue("{open_drift} open drift event(s) must be resolved first"))
        }

        # Check for dependent dashboards
        dashboards <- ont_list_dashboards(concept_id, scope, version, con)
        if (nrow(dashboards) > 0) {
            result$warnings <- c(result$warnings,
                glue::glue("{nrow(dashboards)} dashboard(s) depend on this version"))
        }
    }

    if (action == "activate") {
        # Check if version is in draft status
        if (cv$status != "draft") {
            result$warnings <- c(result$warnings,
                glue::glue("Version is already in '{cv$status}' status"))
        }

        # Check for minimum audit evidence (recommended, not blocking)
        audit_count <- nrow(ont_get_audits(concept_id, scope, version, con = con))
        if (audit_count < 10) {
            result$warnings <- c(result$warnings,
                glue::glue("Only {audit_count} audits recorded (recommend >= 10)"))
        }
    }

    if (action == "adopt") {
        # Check drift status before adopting
        drift_check <- ont_check_drift(concept_id, scope, version, con = con)
        if (drift_check$drift_detected) {
            result$warnings <- c(result$warnings,
                glue::glue("Definition shows drift ({round(drift_check$disagreement_rate*100,1)}% disagreement)"))
        }
    }

    # Display results
    if (result$allowed) {
        if (length(result$warnings) > 0) {
            cli::cli_alert_warning("Action allowed with warnings:")
            cli::cli_ul(result$warnings)
        } else {
            cli::cli_alert_success("Action allowed")
        }
    } else {
        cli::cli_alert_danger("Action BLOCKED:")
        cli::cli_ul(result$blockers)
    }

    result
}

#' Generate Governance Report
#'
#' Creates a summary report of ontology governance status.
#'
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with report sections.
#'
#' @export
ont_governance_report <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    report <- list(
        generated_at = Sys.time(),
        summary = list(),
        concepts = list(),
        drift = list(),
        recent_actions = list()
    )

    # Summary counts
    report$summary$object_types <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) as n FROM ont_object_types")$n
    report$summary$concepts <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) as n FROM ont_concepts")$n
    report$summary$active_versions <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) as n FROM ont_concept_versions WHERE status = 'active'")$n
    report$summary$total_audits <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) as n FROM ont_audits")$n
    report$summary$open_drift_events <- DBI::dbGetQuery(con,
        "SELECT COUNT(*) as n FROM ont_drift_events WHERE status = 'open'")$n

    # Concepts by status
    report$concepts$by_status <- DBI::dbGetQuery(con, "
        SELECT status, COUNT(*) as count
        FROM ont_concept_versions
        GROUP BY status
    ")

    # Recent drift events
    report$drift$recent <- dplyr::tbl(con, "ont_drift_events") |>
        dplyr::arrange(dplyr::desc(.data$detected_at)) |>
        utils::head(10) |>
        dplyr::collect()

    # Recent governance actions
    report$recent_actions <- dplyr::tbl(con, "ont_governance_log") |>
        dplyr::arrange(dplyr::desc(.data$action_at)) |>
        utils::head(20) |>
        dplyr::collect()

    # Print report
    cli::cli_h1("Ontology Governance Report")
    cli::cli_text("Generated: {report$generated_at}")

    cli::cli_h2("Summary")
    cli::cli_ul(c(
        "Object types: {report$summary$object_types}",
        "Concepts: {report$summary$concepts}",
        "Active versions: {report$summary$active_versions}",
        "Total audits: {report$summary$total_audits}",
        "Open drift events: {report$summary$open_drift_events}"
    ))

    if (report$summary$open_drift_events > 0) {
        cli::cli_h2("Open Drift Events")
        for (i in seq_len(min(5, nrow(report$drift$recent)))) {
            row <- report$drift$recent[i, ]
            if (row$status == "open") {
                cli::cli_alert_danger(
                    "{row$drift_id}: {row$concept_id}@{row$scope} v{row$version} - {round(row$disagreement_rate*100,1)}%"
                )
            }
        }
    }

    invisible(report)
}
