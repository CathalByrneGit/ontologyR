# =============================================================================
# Actions & Writeback
# =============================================================================
# Foundry-inspired actions enable users to take governed actions based on
# concept evaluations. This bridges analytics to operations.
# =============================================================================

#' Define an Action Type
#'
#' Creates a new action type that users can execute on objects. Action types
#' define what parameters are needed, what concept triggers the action (optional),
#' and where results are written.
#'
#' @param action_type_id Unique identifier for this action type.
#' @param action_name Human-readable name.
#' @param object_type Which object type this action applies to.
#' @param description Optional description.
#' @param trigger_concept Optional concept that enables this action.
#' @param trigger_scope Scope of the trigger concept.
#' @param trigger_condition SQL condition on concept_value (e.g., "concept_value = TRUE").
#' @param parameters List defining parameters: list(param_name = list(type, required, default, values)).
#' @param writeback_table Table to write action results to.
#' @param writeback_columns List mapping action fields to table columns.
#' @param require_note Whether a note is required when executing.
#' @param require_approval Whether approval is needed before execution.
#' @param allowed_roles Character vector of roles allowed to execute this action.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the action_type_id.
#'
#' @examples
#' \dontrun{
#' ont_define_action(
#'   action_type_id = "escalate_to_manager",
#'   action_name = "Escalate to Manager",
#'   object_type = "Patient",
#'   trigger_concept = "high_risk_patient",
#'   trigger_scope = "clinical",
#'   trigger_condition = "concept_value = TRUE",
#'   parameters = list(
#'     reason = list(type = "text", required = TRUE),
#'     priority = list(type = "enum", values = c("normal", "urgent"), default = "normal")
#'   ),
#'   writeback_table = "escalations"
#' )
#' }
#'
#' @export
ont_define_action <- function(action_type_id,
                               action_name,
                               object_type,
                               description = NULL,
                               trigger_concept = NULL,
                               trigger_scope = NULL,
                               trigger_condition = NULL,
                               parameters = NULL,
                               writeback_table = NULL,
                               writeback_columns = NULL,
                               require_note = FALSE,
                               require_approval = FALSE,
                               allowed_roles = NULL,
                               con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type exists
    obj <- ont_get_object(object_type, con)
    if (is.null(obj)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Validate trigger concept if provided
    if (!is.null(trigger_concept)) {
        concept <- ont_get_concept(trigger_concept, con)
        if (is.null(concept)) {
            cli::cli_abort("Trigger concept {.val {trigger_concept}} not found.")
        }
    }

    # Convert lists to JSON
    params_json <- if (!is.null(parameters)) jsonlite::toJSON(parameters, auto_unbox = TRUE) else NA
    columns_json <- if (!is.null(writeback_columns)) jsonlite::toJSON(writeback_columns, auto_unbox = TRUE) else NA
    roles_json <- if (!is.null(allowed_roles)) jsonlite::toJSON(allowed_roles, auto_unbox = TRUE) else NA

    DBI::dbExecute(
        con,
        "INSERT INTO ont_action_types (action_type_id, action_name, description, object_type,
         trigger_concept, trigger_scope, trigger_condition, parameters, writeback_table,
         writeback_columns, require_note, require_approval, allowed_roles)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            action_type_id,
            action_name,
            null_to_na(description),
            object_type,
            null_to_na(trigger_concept),
            null_to_na(trigger_scope),
            null_to_na(trigger_condition),
            params_json,
            null_to_na(writeback_table),
            columns_json,
            require_note,
            require_approval,
            roles_json
        )
    )

    cli::cli_alert_success("Defined action type {.val {action_type_id}}")
    invisible(action_type_id)
}

#' Execute an Action
#'
#' Executes an action on an object, recording the action with full audit trail.
#' If the action type has a trigger concept, the concept is evaluated first.
#'
#' @param action_type_id The action type to execute.
#' @param object_key Primary key of the object to act upon.
#' @param params List of parameter values.
#' @param actor User executing the action.
#' @param notes Optional notes.
#' @param con Optional DBI connection.
#'
#' @return A list with action_id, status, and result.
#'
#' @examples
#' \dontrun{
#' result <- ont_execute_action(
#'   "escalate_to_manager",
#'   object_key = "P123",
#'   params = list(reason = "Multiple risk factors", priority = "urgent"),
#'   actor = "nurse_jones"
#' )
#' }
#'
#' @export
ont_execute_action <- function(action_type_id,
                                object_key,
                                params = list(),
                                actor,
                                notes = NULL,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    # Get action type
    action_type <- ont_get_action_type(action_type_id, con)
    if (is.null(action_type)) {
        cli::cli_abort("Action type {.val {action_type_id}} not found.")
    }

    if (!action_type$enabled) {
        cli::cli_abort("Action type {.val {action_type_id}} is disabled.")
    }

    # Validate required parameters
    if (!is.na(action_type$parameters)) {
        param_defs <- jsonlite::fromJSON(action_type$parameters)
        for (param_name in names(param_defs)) {
            def <- param_defs[[param_name]]
            if (isTRUE(def$required) && is.null(params[[param_name]])) {
                cli::cli_abort("Required parameter {.val {param_name}} is missing.")
            }
        }
    }

    # Check if note is required
    if (action_type$require_note && (is.null(notes) || notes == "")) {
        cli::cli_abort("A note is required for this action.")
    }

    # Evaluate trigger concept if defined
    concept_value <- NA
    if (!is.na(action_type$trigger_concept)) {
        # Get the object type's metadata
        obj_meta <- ont_get_object(action_type$object_type, con)

        # Get active version of trigger concept
        cv <- ont_get_active_version(action_type$trigger_concept, action_type$trigger_scope, con = con)
        if (!is.null(cv)) {
            # Evaluate for this specific object
            query <- glue::glue("
                SELECT ({cv$sql_expr}) AS concept_value
                FROM {obj_meta$table_name}
                WHERE {obj_meta$pk_column} = ?
            ")
            result <- DBI::dbGetQuery(con, query, params = list(object_key))
            if (nrow(result) > 0) {
                concept_value <- result$concept_value[1]
            }
        }

        # Check trigger condition if defined
        if (!is.na(action_type$trigger_condition) && !is.na(concept_value)) {
            # Simple evaluation of trigger condition
            condition_met <- eval(parse(text = gsub("concept_value", concept_value, action_type$trigger_condition)))
            if (!isTRUE(condition_met)) {
                cli::cli_abort("Trigger condition not met: {action_type$trigger_condition}")
            }
        }
    }

    # Generate action ID
    action_id <- paste0("ACT-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))

    # Determine initial status
    status <- if (action_type$require_approval) "pending_approval" else "completed"

    # Record the action
    DBI::dbExecute(
        con,
        "INSERT INTO ont_action_log (action_id, action_type_id, object_key, parameters,
         concept_value, status, executed_by, notes)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            action_id,
            action_type_id,
            object_key,
            jsonlite::toJSON(params, auto_unbox = TRUE),
            concept_value,
            status,
            actor,
            null_to_na(notes)
        )
    )

    # Write to writeback table if defined and action is completed
    result <- NULL
    if (status == "completed" && !is.na(action_type$writeback_table)) {
        result <- tryCatch({
            ont_writeback_action(action_id, action_type, object_key, params, actor, con)
        }, error = function(e) {
            # Update status to failed
            DBI::dbExecute(con,
                "UPDATE ont_action_log SET status = 'failed', error_message = ? WHERE action_id = ?",
                params = list(as.character(e$message), action_id)
            )
            cli::cli_alert_danger("Action failed: {e$message}")
            list(error = e$message)
        })
    }

    cli::cli_alert_success("Executed action {.val {action_id}} ({action_type$action_name})")

    list(
        action_id = action_id,
        status = status,
        concept_value = concept_value,
        result = result
    )
}

#' Write Action Results to Table
#'
#' Internal function to write action results to the configured writeback table.
#'
#' @param action_id The action ID.
#' @param action_type The action type record.
#' @param object_key The object key.
#' @param params The action parameters.
#' @param actor The user who executed the action.
#' @param con DBI connection.
#'
#' @return The result of the writeback.
#'
#' @keywords internal
ont_writeback_action <- function(action_id, action_type, object_key, params, actor, con) {
    # Build column list and values
    columns <- c("action_id", "object_key", "executed_by", "executed_at")
    values <- list(action_id, object_key, actor, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

    # Add parameters as columns
    for (param_name in names(params)) {
        columns <- c(columns, param_name)
        values <- c(values, list(params[[param_name]]))
    }

    # Build and execute INSERT
    placeholders <- paste(rep("?", length(columns)), collapse = ", ")
    col_names <- paste(columns, collapse = ", ")

    query <- glue::glue("INSERT INTO {action_type$writeback_table} ({col_names}) VALUES ({placeholders})")

    DBI::dbExecute(con, query, params = values)

    list(table = action_type$writeback_table, columns = columns)
}

#' Approve a Pending Action
#'
#' Approves an action that requires approval, then executes the writeback.
#'
#' @param action_id The action to approve.
#' @param approved_by User approving the action.
#' @param con Optional DBI connection.
#'
#' @return TRUE if successful.
#'
#' @export
ont_approve_action <- function(action_id, approved_by, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get the action
    action <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_action_log WHERE action_id = ?",
        params = list(action_id)
    )

    if (nrow(action) == 0) {
        cli::cli_abort("Action {.val {action_id}} not found.")
    }

    if (action$status[1] != "pending_approval") {
        cli::cli_abort("Action {.val {action_id}} is not pending approval (status: {action$status[1]}).")
    }

    # Update status
    DBI::dbExecute(con,
        "UPDATE ont_action_log SET status = 'completed', approved_at = CURRENT_TIMESTAMP, approved_by = ?
         WHERE action_id = ?",
        params = list(approved_by, action_id)
    )

    # Execute writeback
    action_type <- ont_get_action_type(action$action_type_id[1], con)
    if (!is.na(action_type$writeback_table)) {
        params <- jsonlite::fromJSON(action$parameters[1])
        ont_writeback_action(action_id, action_type, action$object_key[1], params, action$executed_by[1], con)
    }

    cli::cli_alert_success("Approved action {.val {action_id}}")
    TRUE
}

#' Reject a Pending Action
#'
#' Rejects an action that requires approval.
#'
#' @param action_id The action to reject.
#' @param rejected_by User rejecting the action.
#' @param reason Reason for rejection.
#' @param con Optional DBI connection.
#'
#' @return TRUE if successful.
#'
#' @export
ont_reject_action <- function(action_id, rejected_by, reason = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(con,
        "UPDATE ont_action_log SET status = 'rejected', approved_at = CURRENT_TIMESTAMP,
         approved_by = ?, notes = COALESCE(notes || ' | Rejected: ', 'Rejected: ') || ?
         WHERE action_id = ? AND status = 'pending_approval'",
        params = list(rejected_by, null_to_na(reason), action_id)
    )

    cli::cli_alert_info("Rejected action {.val {action_id}}")
    TRUE
}

#' Get Action Type
#'
#' Retrieves an action type definition.
#'
#' @param action_type_id The action type ID.
#' @param con Optional DBI connection.
#'
#' @return A single-row data frame, or NULL if not found.
#'
#' @export
ont_get_action_type <- function(action_type_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_action_types WHERE action_type_id = ?",
        params = list(action_type_id)
    )

    if (nrow(result) == 0) return(NULL)
    result[1, ]
}

#' List Action Types
#'
#' Lists all defined action types.
#'
#' @param object_type Optional filter by object type.
#' @param enabled_only If TRUE, only return enabled action types.
#' @param con Optional DBI connection.
#'
#' @return A tibble of action types.
#'
#' @export
ont_list_action_types <- function(object_type = NULL, enabled_only = TRUE, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_action_types WHERE 1=1"
    params <- list()

    if (!is.null(object_type)) {
        query <- paste(query, "AND object_type = ?")
        params <- c(params, object_type)
    }

    if (enabled_only) {
        query <- paste(query, "AND enabled = TRUE")
    }

    query <- paste(query, "ORDER BY action_name")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Get Action History
#'
#' Retrieves the history of actions for a concept, object, or action type.
#'
#' @param action_type_id Optional filter by action type.
#' @param object_key Optional filter by object.
#' @param actor Optional filter by user who executed.
#' @param status Optional filter by status.
#' @param since Optional datetime to filter actions after.
#' @param limit Maximum number of records to return.
#' @param con Optional DBI connection.
#'
#' @return A tibble of action records.
#'
#' @export
ont_action_history <- function(action_type_id = NULL,
                                object_key = NULL,
                                actor = NULL,
                                status = NULL,
                                since = NULL,
                                limit = 100,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT al.*, at.action_name, at.object_type
              FROM ont_action_log al
              JOIN ont_action_types at ON al.action_type_id = at.action_type_id
              WHERE 1=1"
    params <- list()

    if (!is.null(action_type_id)) {
        query <- paste(query, "AND al.action_type_id = ?")
        params <- c(params, action_type_id)
    }

    if (!is.null(object_key)) {
        query <- paste(query, "AND al.object_key = ?")
        params <- c(params, object_key)
    }

    if (!is.null(actor)) {
        query <- paste(query, "AND al.executed_by = ?")
        params <- c(params, actor)
    }

    if (!is.null(status)) {
        query <- paste(query, "AND al.status = ?")
        params <- c(params, status)
    }

    if (!is.null(since)) {
        query <- paste(query, "AND al.executed_at >= ?")
        params <- c(params, since)
    }

    query <- paste(query, "ORDER BY al.executed_at DESC LIMIT ?")
    params <- c(params, limit)

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Get Pending Actions
#'
#' Retrieves actions that are pending approval.
#'
#' @param action_type_id Optional filter by action type.
#' @param con Optional DBI connection.
#'
#' @return A tibble of pending actions.
#'
#' @export
ont_pending_actions <- function(action_type_id = NULL, con = NULL) {
    ont_action_history(action_type_id = action_type_id, status = "pending_approval", con = con)
}

#' Get Available Actions for Object
#'
#' Returns action types that can be executed on a specific object,
#' based on current concept evaluations and trigger conditions.
#'
#' @param object_key The object to check.
#' @param object_type The type of the object.
#' @param con Optional DBI connection.
#'
#' @return A tibble of available action types with their trigger status.
#'
#' @export
ont_available_actions <- function(object_key, object_type, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get all action types for this object type
    action_types <- ont_list_action_types(object_type = object_type, con = con)

    if (nrow(action_types) == 0) {
        return(tibble::tibble())
    }

    # Get object metadata
    obj_meta <- ont_get_object(object_type, con)

    # For each action type, check if trigger condition is met
    results <- lapply(seq_len(nrow(action_types)), function(i) {
        at <- action_types[i, ]

        trigger_met <- TRUE
        concept_value <- NA

        if (!is.na(at$trigger_concept)) {
            cv <- ont_get_active_version(at$trigger_concept, at$trigger_scope, con = con)
            if (!is.null(cv)) {
                query <- glue::glue("
                    SELECT ({cv$sql_expr}) AS concept_value
                    FROM {obj_meta$table_name}
                    WHERE {obj_meta$pk_column} = ?
                ")
                result <- DBI::dbGetQuery(con, query, params = list(object_key))
                if (nrow(result) > 0) {
                    concept_value <- result$concept_value[1]

                    # Check trigger condition
                    if (!is.na(at$trigger_condition)) {
                        trigger_met <- tryCatch({
                            eval(parse(text = gsub("concept_value", concept_value, at$trigger_condition)))
                        }, error = function(e) FALSE)
                    }
                }
            }
        }

        tibble::tibble(
            action_type_id = at$action_type_id,
            action_name = at$action_name,
            trigger_met = trigger_met,
            concept_value = concept_value,
            require_approval = at$require_approval
        )
    })

    dplyr::bind_rows(results)
}

#' Action Summary
#'
#' Returns summary statistics for actions.
#'
#' @param action_type_id Optional filter by action type.
#' @param since Optional datetime to filter actions after.
#' @param con Optional DBI connection.
#'
#' @return A tibble with summary statistics.
#'
#' @export
ont_action_summary <- function(action_type_id = NULL, since = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT
        at.action_type_id,
        at.action_name,
        at.object_type,
        COUNT(*) as total_actions,
        SUM(CASE WHEN al.status = 'completed' THEN 1 ELSE 0 END) as completed,
        SUM(CASE WHEN al.status = 'pending_approval' THEN 1 ELSE 0 END) as pending,
        SUM(CASE WHEN al.status = 'rejected' THEN 1 ELSE 0 END) as rejected,
        SUM(CASE WHEN al.status = 'failed' THEN 1 ELSE 0 END) as failed,
        COUNT(DISTINCT al.object_key) as unique_objects,
        COUNT(DISTINCT al.executed_by) as unique_actors,
        MIN(al.executed_at) as first_action,
        MAX(al.executed_at) as last_action
    FROM ont_action_log al
    JOIN ont_action_types at ON al.action_type_id = at.action_type_id
    WHERE 1=1"
    params <- list()

    if (!is.null(action_type_id)) {
        query <- paste(query, "AND al.action_type_id = ?")
        params <- c(params, action_type_id)
    }

    if (!is.null(since)) {
        query <- paste(query, "AND al.executed_at >= ?")
        params <- c(params, since)
    }

    query <- paste(query, "GROUP BY at.action_type_id, at.action_name, at.object_type")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}
