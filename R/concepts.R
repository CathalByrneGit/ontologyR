#' @title Concept Definition and Evaluation
#' @description Functions for defining versioned concepts (SQL-based definitions)
#'   and evaluating them against object data.
#' @name concepts
NULL

#' Define a New Concept
#'
#' Creates a new concept that can have multiple versioned definitions. A concept
#' represents a meaningful category or classification (e.g., "ready_for_discharge",
#' "blocked_bed") that can be evaluated against objects of a specific type.
#'
#' @param concept_id Character. Unique identifier for the concept.
#' @param object_type Character. The object type this concept applies to.
#'   Must be a registered object type.
#' @param description Character. Human-readable description of what this
#'   concept represents.
#' @param owner_domain Character. Optional domain/team that owns this concept.
#' @param created_by Character. Optional creator identifier.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#' ont_register_object("Encounter", "encounters", "encounter_id")
#'
#' # Define a concept
#' ont_define_concept(
#'     concept_id = "ready_for_discharge",
#'     object_type = "Encounter",
#'     description = "Patient is clinically ready to leave hospital",
#'     owner_domain = "patient_flow"
#' )
#'
#' ont_disconnect()
#' }
#'
#' @export
ont_define_concept <- function(concept_id,
                                object_type,
                                description = NULL,
                                owner_domain = NULL,
                                created_by = NULL,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type exists
    ont_get_object(object_type, con)

    # Check for existing concept
    existing <- DBI::dbGetQuery(
        con,
        "SELECT concept_id FROM ont_concepts WHERE concept_id = ?",
        params = list(concept_id)
    )

    if (nrow(existing) > 0) {
        cli::cli_abort(
            "Concept {.val {concept_id}} already exists. Use a different ID or update existing."
        )
    }

    DBI::dbExecute(con, "
        INSERT INTO ont_concepts
        (concept_id, object_type, description, owner_domain, created_by, created_at)
        VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
        concept_id, object_type,
        null_to_na(description), null_to_na(owner_domain), null_to_na(created_by)
    ))

    cli::cli_alert_success(
        "Defined concept {.val {concept_id}} for object type {.val {object_type}}"
    )
    invisible(TRUE)
}

#' Add a Concept Version
#'
#' Adds a new versioned definition for an existing concept. Each version is
#' scoped (e.g., "flow", "clinical", "regulatory") and contains a SQL expression
#' that evaluates to TRUE/FALSE or a numeric value.
#'
#' @param concept_id Character. The concept to add a version to.
#' @param scope Character. The scope/context for this version (e.g., "flow",
#'   "clinical", "regulatory"). Different scopes can have different definitions.
#' @param version Integer. Version number. Should be monotonically increasing
#'   within a concept/scope combination.
#' @param sql_expr Character. SQL expression that evaluates to BOOLEAN or numeric.
#'   Can reference columns from the concept's object type table.
#' @param status Character. Initial status: "draft" (default), "active",
#'   "deprecated", or "retired".
#' @param rationale Character. Explanation of why this version exists or how
#'   it differs from previous versions.
#' @param valid_from Date. When this version becomes applicable. If `NULL`,
#'   applicable immediately.
#' @param valid_to Date. When this version stops being applicable. If `NULL`,
#'   no end date.
#' @param created_by Character. Optional creator identifier.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#' ont_register_object("Encounter", "encounters", "encounter_id")
#' ont_define_concept("ready_for_discharge", "Encounter")
#'
#' # Add version 1: simple proxy definition
#' ont_add_version(
#'     concept_id = "ready_for_discharge",
#'     scope = "flow",
#'     version = 1,
#'     sql_expr = "NOT planned_intervention_24h",
#'     status = "active",
#'     rationale = "Operational proxy: no planned interventions in next 24h"
#' )
#'
#' # Add version 2: refined definition
#' ont_add_version(
#'     concept_id = "ready_for_discharge",
#'     scope = "flow",
#'     version = 2,
#'     sql_expr = "NOT planned_intervention_24h AND arrangements_confirmed",
#'     status = "draft",
#'     rationale = "Added arrangements requirement based on audit findings"
#' )
#'
#' ont_disconnect()
#' }
#'
#' @export
ont_add_version <- function(concept_id,
                             scope,
                             version,
                             sql_expr,
                             status = "draft",
                             rationale = NULL,
                             valid_from = NULL,
                             valid_to = NULL,
                             created_by = NULL,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate concept exists
    concept <- ont_get_concept(concept_id, con)

    # Validate status
    valid_statuses <- c("draft", "active", "deprecated", "retired")
    if (!status %in% valid_statuses) {
        cli::cli_abort(
            "Invalid status {.val {status}}. Must be one of: {.val {valid_statuses}}"
        )
    }

    # Check for existing version
    existing <- DBI::dbGetQuery(con, "
        SELECT version FROM ont_concept_versions
        WHERE concept_id = ? AND scope = ? AND version = ?
    ", params = list(concept_id, scope, as.integer(version)))

    if (nrow(existing) > 0) {
        cli::cli_abort(
            "Version {version} already exists for {.val {concept_id}}@{scope}. Use a new version number."
        )
    }

    DBI::dbExecute(con, "
        INSERT INTO ont_concept_versions
        (concept_id, scope, version, sql_expr, status, rationale, valid_from, valid_to, created_by, created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
        concept_id, scope, as.integer(version), sql_expr, status,
        null_to_na(rationale), null_to_na(valid_from), null_to_na(valid_to), null_to_na(created_by)
    ))

    cli::cli_alert_success(
        "Added version {version} for {.val {concept_id}}@{scope} [{status}]"
    )
    invisible(TRUE)
}

#' Activate a Concept Version
#'
#' Sets a concept version's status to "active". This is a governance action
#' that is logged.
#'
#' @param concept_id Character. The concept ID.
#' @param scope Character. The scope.
#' @param version Integer. The version to activate.
#' @param approved_by Character. Who is approving this activation.
#' @param rationale Character. Optional reason for activation.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
ont_activate_version <- function(concept_id,
                                  scope,
                                  version,
                                  approved_by,
                                  rationale = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate version exists
    cv <- ont_get_version(concept_id, scope, version, con)

    if (cv$status == "active") {
        cli::cli_alert_info("Version is already active.")
        return(invisible(TRUE))
    }

    # Update status
    DBI::dbExecute(con, "
        UPDATE ont_concept_versions
        SET status = 'active', approved_at = CURRENT_TIMESTAMP, approved_by = ?
        WHERE concept_id = ? AND scope = ? AND version = ?
    ", params = list(approved_by, concept_id, scope, as.integer(version)))

    # Log governance action
    ont_log_governance(
        action_type = "activate",
        concept_id = concept_id,
        scope = scope,
        version = version,
        actor = approved_by,
        rationale = rationale,
        con = con
    )

    cli::cli_alert_success(
        "Activated {.val {concept_id}}@{scope} v{version}"
    )
    invisible(TRUE)
}

#' Deprecate a Concept Version
#'
#' Marks a concept version as deprecated. Checks for open drift events
#' that might block deprecation.
#'
#' @param concept_id Character. The concept ID.
#' @param scope Character. The scope.
#' @param version Integer. The version to deprecate.
#' @param deprecated_by Character. Who is deprecating this version.
#' @param rationale Character. Reason for deprecation.
#' @param force Logical. If `TRUE`, deprecate even with open drift events.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
ont_deprecate_version <- function(concept_id,
                                   scope,
                                   version,
                                   deprecated_by,
                                   rationale = NULL,
                                   force = FALSE,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    # Check for open drift events
    open_drift <- DBI::dbGetQuery(con, "
        SELECT drift_id FROM ont_drift_events
        WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'
    ", params = list(concept_id, scope, as.integer(version)))

    if (nrow(open_drift) > 0 && !force) {
        cli::cli_abort(c(
            "Cannot deprecate: {nrow(open_drift)} open drift event(s) exist.",
            "i" = "Resolve drift events first or use {.arg force = TRUE}."
        ))
    }

    # Update status
    DBI::dbExecute(con, "
        UPDATE ont_concept_versions
        SET status = 'deprecated'
        WHERE concept_id = ? AND scope = ? AND version = ?
    ", params = list(concept_id, scope, as.integer(version)))

    # Log governance action
    ont_log_governance(
        action_type = "deprecate",
        concept_id = concept_id,
        scope = scope,
        version = version,
        actor = deprecated_by,
        rationale = rationale,
        con = con
    )

    cli::cli_alert_success(
        "Deprecated {.val {concept_id}}@{scope} v{version}"
    )
    invisible(TRUE)
}

#' List Concepts
#'
#' Returns all registered concepts with their metadata.
#'
#' @param object_type Character. Optional filter by object type.
#' @param owner_domain Character. Optional filter by owner domain.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of concepts.
#'
#' @export
ont_list_concepts <- function(object_type = NULL, owner_domain = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_concepts")

    if (!is.null(object_type)) {
        result <- result |> dplyr::filter(.data$object_type == !!object_type)
    }

    if (!is.null(owner_domain)) {
        result <- result |> dplyr::filter(.data$owner_domain == !!owner_domain)
    }

    result |> dplyr::collect()
}

#' List Concept Versions
#'
#' Returns versions for a concept, optionally filtered by scope and status.
#'
#' @param concept_id Character. The concept to list versions for.
#' @param scope Character. Optional filter by scope.
#' @param status Character. Optional filter by status.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of concept versions.
#'
#' @export
ont_list_versions <- function(concept_id, scope = NULL, status = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_concept_versions") |>
        dplyr::filter(.data$concept_id == !!concept_id)

    if (!is.null(scope)) {
        result <- result |> dplyr::filter(.data$scope == !!scope)
    }

    if (!is.null(status)) {
        result <- result |> dplyr::filter(.data$status == !!status)
    }

    result |>
        dplyr::arrange(.data$scope, dplyr::desc(.data$version)) |>
        dplyr::collect()
}

#' Get Concept Metadata
#'
#' @param concept_id Character. The concept ID.
#' @param con A DBI connection.
#'
#' @return A single-row tibble.
#'
#' @keywords internal
#' @export
ont_get_concept <- function(concept_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_concepts") |>
        dplyr::filter(.data$concept_id == !!concept_id) |>
        dplyr::collect()

    if (nrow(result) != 1) {
        cli::cli_abort("Unknown concept: {.val {concept_id}}")
    }

    result
}

#' Get Concept Version
#'
#' @param concept_id Character. The concept ID.
#' @param scope Character. The scope.
#' @param version Integer. The version number.
#' @param con A DBI connection.
#'
#' @return A single-row tibble with version details.
#'
#' @keywords internal
#' @export
ont_get_version <- function(concept_id, scope, version, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_concept_versions") |>
        dplyr::filter(
            .data$concept_id == !!concept_id,
            .data$scope == !!scope,
            .data$version == !!as.integer(version)
        ) |>
        dplyr::collect()

    if (nrow(result) != 1) {
        cli::cli_abort("Unknown version: {.val {concept_id}}@{scope} v{version}")
    }

    result
}

#' Get Active Version
#'
#' Returns the currently active version for a concept/scope combination.
#'
#' @param concept_id Character. The concept ID.
#' @param scope Character. The scope.
#' @param as_of Date. Optional date for temporal validity. Defaults to today.
#' @param con A DBI connection.
#'
#' @return A single-row tibble, or error if no active version found.
#'
#' @export
ont_get_active_version <- function(concept_id, scope, as_of = Sys.Date(), con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_concept_versions") |>
        dplyr::filter(
            .data$concept_id == !!concept_id,
            .data$scope == !!scope,
            .data$status == "active"
        ) |>
        dplyr::filter(
            is.na(.data$valid_from) | .data$valid_from <= !!as_of,
            is.na(.data$valid_to) | .data$valid_to >= !!as_of
        ) |>
        dplyr::arrange(dplyr::desc(.data$version)) |>
        utils::head(1) |>
        dplyr::collect()

    if (nrow(result) != 1) {
        cli::cli_abort("No active version found for {.val {concept_id}}@{scope}")
    }

    result
}

#' Evaluate a Concept
#'
#' Evaluates a concept version against its object type table, returning
#' the original data plus a `concept_value` column with the evaluation result.
#'
#' The result includes provenance metadata as attributes.
#'
#' @param concept_id Character. The concept to evaluate.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, uses the active version.
#' @param filter_expr Character. Optional SQL WHERE clause to filter objects
#'   before evaluation.
#' @param collect Logical. If `TRUE` (default), collect results. If `FALSE`,
#'   return lazy tbl.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble (or lazy tbl) with original columns plus `concept_value`.
#'   Has "ontology_provenance" attribute with evaluation metadata.
#'
#' @examples
#' ont_connect(":memory:")
#'
#' # Setup
#' DBI::dbWriteTable(ont_get_connection(), "encounters", tibble::tibble(
#'     encounter_id = c("E1", "E2", "E3"),
#'     planned_intervention_24h = c(TRUE, FALSE, FALSE),
#'     arrangements_confirmed = c(FALSE, TRUE, FALSE)
#' ))
#' ont_register_object("Encounter", "encounters", "encounter_id")
#' ont_define_concept("ready_for_discharge", "Encounter")
#' ont_add_version("ready_for_discharge", "flow", 1,
#'     sql_expr = "NOT planned_intervention_24h",
#'     status = "active")
#'
#' # Evaluate
#' result <- ont_evaluate("ready_for_discharge", "flow", 1)
#' print(result)
#'
#' # Check provenance
#' attr(result, "ontology_provenance")
#'
#' ont_disconnect()
#'
#' @export
ont_evaluate <- function(concept_id,
                          scope,
                          version = NULL,
                          filter_expr = NULL,
                          collect = TRUE,
                          con = NULL) {
    con <- con %||% ont_get_connection()

    # Get version (active if not specified)
    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        version <- cv$version
    } else {
        cv <- ont_get_version(concept_id, scope, version, con)
    }

    # Get object type metadata
    concept <- ont_get_concept(concept_id, con)
    obj_meta <- ont_get_object(concept$object_type, con)

    # Build query
    base_tbl <- dplyr::tbl(con, obj_meta$table_name)

    # Apply filter if provided
    if (!is.null(filter_expr)) {
        base_tbl <- base_tbl |>
            dplyr::filter(dplyr::sql(filter_expr))
    }

    # Add concept evaluation
    result <- base_tbl |>
        dplyr::mutate(concept_value = dplyr::sql(cv$sql_expr))

    if (collect) {
        result <- dplyr::collect(result)
    }

    # Attach provenance
    attr(result, "ontology_provenance") <- list(
        concept_id = concept_id,
        scope = scope,
        version = version,
        sql_expr = cv$sql_expr,
        status = cv$status,
        object_type = concept$object_type,
        pk_column = obj_meta$pk_column,
        table_name = obj_meta$table_name,
        evaluated_at = Sys.time()
    )

    result
}

#' Compare Concept Versions
#'
#' Evaluates two versions of a concept and shows where they disagree.
#'
#' @param concept_id Character. The concept to compare.
#' @param scope Character. The scope.
#' @param version_a Integer. First version.
#' @param version_b Integer. Second version.
#' @param filter_expr Character. Optional SQL WHERE clause.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with columns from the object table plus `value_a`,
#'   `value_b`, and `agree`.
#'
#' @export
ont_compare_versions <- function(concept_id,
                                  scope,
                                  version_a,
                                  version_b,
                                  filter_expr = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    cv_a <- ont_get_version(concept_id, scope, version_a, con)
    cv_b <- ont_get_version(concept_id, scope, version_b, con)

    concept <- ont_get_concept(concept_id, con)
    obj_meta <- ont_get_object(concept$object_type, con)

    base_tbl <- dplyr::tbl(con, obj_meta$table_name)

    if (!is.null(filter_expr)) {
        base_tbl <- base_tbl |>
            dplyr::filter(dplyr::sql(filter_expr))
    }

    result <- base_tbl |>
        dplyr::mutate(
            value_a = dplyr::sql(cv_a$sql_expr),
            value_b = dplyr::sql(cv_b$sql_expr)
        ) |>
        dplyr::mutate(
            agree = .data$value_a == .data$value_b
        ) |>
        dplyr::collect()

    cli::cli_h3("Comparison: {concept_id}@{scope} v{version_a} vs v{version_b}")
    cli::cli_alert_info("Total objects: {nrow(result)}")
    cli::cli_alert_info("Agreement: {sum(result$agree)} ({round(100*mean(result$agree),1)}%)")
    cli::cli_alert_info("Disagreement: {sum(!result$agree)} ({round(100*mean(!result$agree),1)}%)")

    result
}
