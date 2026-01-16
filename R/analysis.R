#' @title Observations and Analysis
#' @description Functions for recording observations and performing analysis
#'   on concept evaluations over time. Observations capture point-in-time
#'   snapshots, enabling trend analysis, cohort comparison, and version comparison.
#' @name analysis
NULL

# =============================================================================
# OBSERVATIONS
# =============================================================================

#' Record a Concept Observation
#'
#' Captures a point-in-time snapshot of a concept evaluation. Unlike audits
#' (which compare human judgment to system), observations are recorded facts
#' about what the system evaluated at a specific moment.
#'
#' This enables:
#' - Trend analysis: How does concept prevalence change over time?
#' - Comparison: How do different versions or cohorts differ?
#' - Monitoring: Track concept behavior in production
#'
#' @param concept_id Character. The concept to observe.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, uses active version.
#' @param filter_expr Character. Optional SQL WHERE clause to filter objects.
#' @param observation_type Character. Type of observation: "snapshot" (default),
#'   "scheduled", or "triggered".
#' @param store_details Logical. If `TRUE`, store object-level details (can be
#'   large for big datasets). Default `FALSE`.
#' @param triggered_by Character. What triggered this observation (e.g., "manual",
#'   "schedule", "drift_check").
#' @param observer_id Character. Who/what initiated this observation.
#' @param notes Character. Optional notes.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with observation summary including observation_id, counts, and
#'   prevalence rate.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#' # ... setup ...
#'
#' # Take a snapshot observation
#' obs <- ont_observe(
#'   concept_id = "ready_for_discharge",
#'   scope = "flow",
#'   triggered_by = "daily_monitor"
#' )
#'
#' # Observe with a filter
#' obs <- ont_observe(
#'   concept_id = "ready_for_discharge",
#'   scope = "flow",
#'   filter_expr = "ward = 'ICU'",
#'   notes = "ICU-specific observation"
#' )
#' }
#'
#' @export
ont_observe <- function(concept_id,
                         scope,
                         version = NULL,
                         filter_expr = NULL,
                         observation_type = "snapshot",
                         store_details = FALSE,
                         triggered_by = "manual",
                         observer_id = NULL,
                         notes = NULL,
                         con = NULL) {
    con <- con %||% ont_get_connection()

    # Get version if not specified
    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        version <- cv$version
    }

    # Evaluate concept
    result <- ont_evaluate(
        concept_id = concept_id,
        scope = scope,
        version = version,
        filter_expr = filter_expr,
        collect = TRUE,
        con = con
    )

    # Compute statistics
    total_objects <- nrow(result)
    concept_true <- sum(result$concept_value == TRUE, na.rm = TRUE)
    concept_false <- sum(result$concept_value == FALSE, na.rm = TRUE)
    concept_null <- sum(is.na(result$concept_value))
    prevalence_rate <- if (total_objects > 0) concept_true / total_objects else NA_real_

    # Generate observation ID
    observation_id <- paste0("OBS-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                              substr(digest::digest(stats::runif(1)), 1, 8))

    # Insert observation record
    DBI::dbExecute(con, "
        INSERT INTO ont_observations
        (observation_id, concept_id, scope, version, observation_type,
         total_objects, concept_true, concept_false, concept_null,
         prevalence_rate, filter_expr, triggered_by, observer_id, notes)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
        observation_id, concept_id, scope, as.integer(version), observation_type,
        as.integer(total_objects), as.integer(concept_true), as.integer(concept_false),
        as.integer(concept_null), prevalence_rate,
        null_to_na(filter_expr), null_to_na(triggered_by),
        null_to_na(observer_id), null_to_na(notes)
    ))

    # Store details if requested
    if (store_details && total_objects > 0) {
        prov <- attr(result, "ontology_provenance")
        pk_col <- prov$pk_column

        for (i in seq_len(nrow(result))) {
            DBI::dbExecute(con, "
                INSERT INTO ont_observation_details
                (observation_id, object_key, concept_value)
                VALUES (?, ?, ?)
            ", params = list(
                observation_id,
                as.character(result[[pk_col]][i]),
                result$concept_value[i]
            ))
        }
    }

    cli::cli_alert_success(
        "Recorded observation {.val {observation_id}}: {concept_true}/{total_objects} TRUE ({round(prevalence_rate*100, 1)}%)"
    )

    invisible(list(
        observation_id = observation_id,
        concept_id = concept_id,
        scope = scope,
        version = version,
        total_objects = total_objects,
        concept_true = concept_true,
        concept_false = concept_false,
        concept_null = concept_null,
        prevalence_rate = prevalence_rate,
        observed_at = Sys.time()
    ))
}

#' List Observations
#'
#' Retrieves observation records for a concept.
#'
#' @param concept_id Character. Filter by concept.
#' @param scope Character. Filter by scope.
#' @param version Integer. Filter by version.
#' @param from POSIXct or Date. Start of date range.
#' @param to POSIXct or Date. End of date range.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of observation records.
#'
#' @export
ont_list_observations <- function(concept_id = NULL,
                                   scope = NULL,
                                   version = NULL,
                                   from = NULL,
                                   to = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_observations")

    if (!is.null(concept_id)) {
        result <- result |> dplyr::filter(.data$concept_id == !!concept_id)
    }
    if (!is.null(scope)) {
        result <- result |> dplyr::filter(.data$scope == !!scope)
    }
    if (!is.null(version)) {
        result <- result |> dplyr::filter(.data$version == !!as.integer(version))
    }
    if (!is.null(from)) {
        result <- result |> dplyr::filter(.data$observed_at >= !!from)
    }
    if (!is.null(to)) {
        result <- result |> dplyr::filter(.data$observed_at <= !!to)
    }

    result |>
        dplyr::arrange(dplyr::desc(.data$observed_at)) |>
        dplyr::collect()
}

#' Get Observation Details
#'
#' Retrieves object-level details for an observation (if stored).
#'
#' @param observation_id Character. The observation ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with object_key and concept_value columns.
#'
#' @export
ont_get_observation_details <- function(observation_id, con = NULL) {
    con <- con %||% ont_get_connection()

    dplyr::tbl(con, "ont_observation_details") |>
        dplyr::filter(.data$observation_id == !!observation_id) |>
        dplyr::collect()
}

# =============================================================================
# COHORTS
# =============================================================================

#' Define a Cohort
#'
#' Creates a named group of objects for comparative analysis. Cohorts can be
#' defined dynamically via SQL expression or explicitly by listing members.
#'
#' @param cohort_id Character. Unique identifier for the cohort.
#' @param cohort_name Character. Human-readable name.
#' @param object_type Character. The object type for this cohort.
#' @param sql_expr Character. SQL WHERE clause defining cohort membership
#'   (for dynamic cohorts).
#' @param description Character. Optional description.
#' @param created_by Character. Optional creator identifier.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the cohort_id.
#'
#' @examples
#' \dontrun{
#' # Define a cohort of ICU patients
#' ont_define_cohort(
#'   cohort_id = "icu_patients",
#'   cohort_name = "ICU Patients",
#'   object_type = "Encounter",
#'   sql_expr = "ward = 'ICU'"
#' )
#' }
#'
#' @export
ont_define_cohort <- function(cohort_id,
                               cohort_name,
                               object_type,
                               sql_expr = NULL,
                               description = NULL,
                               created_by = NULL,
                               con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type
    ont_get_object(object_type, con)

    definition_type <- if (!is.null(sql_expr)) "sql" else "explicit"

    DBI::dbExecute(con, "
        INSERT INTO ont_cohorts
        (cohort_id, cohort_name, object_type, definition_type, sql_expr, description, created_by)
        VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
        cohort_id, cohort_name, object_type, definition_type,
        null_to_na(sql_expr), null_to_na(description), null_to_na(created_by)
    ))

    cli::cli_alert_success("Defined cohort {.val {cohort_name}} ({cohort_id})")
    invisible(cohort_id)
}

#' Add Members to a Cohort
#'
#' Adds explicit members to a cohort (for explicit cohorts).
#'
#' @param cohort_id Character. The cohort ID.
#' @param object_keys Character vector. Primary keys of objects to add.
#' @param added_by Character. Optional identifier of who added these.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the number of members added.
#'
#' @export
ont_add_cohort_members <- function(cohort_id,
                                    object_keys,
                                    added_by = NULL,
                                    con = NULL) {
    con <- con %||% ont_get_connection()

    count <- 0
    for (key in object_keys) {
        tryCatch({
            DBI::dbExecute(con, "
                INSERT INTO ont_cohort_members (cohort_id, object_key, added_by)
                VALUES (?, ?, ?)
            ", params = list(cohort_id, as.character(key), null_to_na(added_by)))
            count <- count + 1
        }, error = function(e) {
            # Ignore duplicates
            if (!grepl("UNIQUE constraint", e$message, ignore.case = TRUE)) {
                cli::cli_warn("Failed to add {key}: {e$message}")
            }
        })
    }

    cli::cli_alert_success("Added {count} members to cohort {.val {cohort_id}}")
    invisible(count)
}

#' Get Cohort Members
#'
#' Retrieves the members of a cohort (evaluates SQL for dynamic cohorts).
#'
#' @param cohort_id Character. The cohort ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A character vector of object keys.
#'
#' @export
ont_get_cohort_members <- function(cohort_id, con = NULL) {
    con <- con %||% ont_get_connection()

    cohort <- DBI::dbGetQuery(con, "
        SELECT * FROM ont_cohorts WHERE cohort_id = ?
    ", params = list(cohort_id))

    if (nrow(cohort) == 0) {
        cli::cli_abort("Unknown cohort: {.val {cohort_id}}")
    }

    if (cohort$definition_type == "explicit") {
        # Get explicit members
        members <- DBI::dbGetQuery(con, "
            SELECT object_key FROM ont_cohort_members WHERE cohort_id = ?
        ", params = list(cohort_id))
        return(members$object_key)
    } else {
        # Evaluate SQL expression
        obj_meta <- ont_get_object(cohort$object_type, con)
        query <- glue::glue(
            "SELECT {obj_meta$pk_column} AS object_key FROM {obj_meta$table_name} WHERE {cohort$sql_expr}"
        )
        members <- DBI::dbGetQuery(con, query)
        return(as.character(members$object_key))
    }
}

#' List Cohorts
#'
#' Returns all defined cohorts.
#'
#' @param object_type Character. Optional filter by object type.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of cohort definitions.
#'
#' @export
ont_list_cohorts <- function(object_type = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_cohorts")

    if (!is.null(object_type)) {
        result <- result |> dplyr::filter(.data$object_type == !!object_type)
    }

    result |> dplyr::collect()
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

#' Trend Analysis
#'
#' Analyzes how a concept's prevalence changes over time using observation data.
#'
#' @param concept_id Character. The concept to analyze.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, analyzes all versions.
#' @param from POSIXct or Date. Start of analysis period.
#' @param to POSIXct or Date. End of analysis period.
#' @param granularity Character. Time granularity: "day", "week", "month".
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with time period, observation counts, and prevalence statistics.
#'
#' @export
ont_trend_analysis <- function(concept_id,
                                scope,
                                version = NULL,
                                from = NULL,
                                to = NULL,
                                granularity = "day",
                                con = NULL) {
    con <- con %||% ont_get_connection()

    observations <- ont_list_observations(
        concept_id = concept_id,
        scope = scope,
        version = version,
        from = from,
        to = to,
        con = con
    )

    if (nrow(observations) == 0) {
        cli::cli_warn("No observations found for trend analysis.")
        return(tibble::tibble())
    }

    # Add time period column based on granularity
    observations <- observations |>
        dplyr::mutate(
            period = switch(granularity,
                "day" = as.Date(.data$observed_at),
                "week" = as.Date(cut(.data$observed_at, "week")),
                "month" = as.Date(cut(.data$observed_at, "month")),
                as.Date(.data$observed_at)
            )
        )

    # Aggregate by period
    trend <- observations |>
        dplyr::group_by(.data$period, .data$version) |>
        dplyr::summarise(
            observation_count = dplyr::n(),
            total_objects = sum(.data$total_objects),
            concept_true = sum(.data$concept_true),
            concept_false = sum(.data$concept_false),
            avg_prevalence = mean(.data$prevalence_rate, na.rm = TRUE),
            min_prevalence = min(.data$prevalence_rate, na.rm = TRUE),
            max_prevalence = max(.data$prevalence_rate, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::arrange(.data$period)

    cli::cli_alert_info(
        "Trend analysis: {nrow(trend)} periods, {sum(trend$observation_count)} observations"
    )

    trend
}

#' Version Comparison
#'
#' Compares how different versions of a concept evaluate the same objects.
#' Useful for understanding the impact of definition changes.
#'
#' @param concept_id Character. The concept to analyze.
#' @param scope Character. The scope.
#' @param versions Integer vector. Versions to compare (default: all).
#' @param filter_expr Character. Optional SQL WHERE clause.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with comparison results including agreement matrix.
#'
#' @export
ont_version_compare <- function(concept_id,
                                 scope,
                                 versions = NULL,
                                 filter_expr = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    # Get all versions if not specified
    if (is.null(versions)) {
        all_versions <- DBI::dbGetQuery(con, "
            SELECT DISTINCT version FROM ont_concept_versions
            WHERE concept_id = ? AND scope = ?
            ORDER BY version
        ", params = list(concept_id, scope))
        versions <- all_versions$version
    }

    if (length(versions) < 2) {
        cli::cli_abort("Need at least 2 versions to compare.")
    }

    # Evaluate each version
    results <- list()
    for (v in versions) {
        result <- ont_evaluate(
            concept_id = concept_id,
            scope = scope,
            version = v,
            filter_expr = filter_expr,
            collect = TRUE,
            con = con
        )
        prov <- attr(result, "ontology_provenance")
        pk_col <- prov$pk_column

        results[[as.character(v)]] <- tibble::tibble(
            object_key = as.character(result[[pk_col]]),
            concept_value = result$concept_value
        )
    }

    # Build comparison matrix
    # Start with first version's objects
    comparison <- results[[1]] |>
        dplyr::rename(!!paste0("v", versions[1]) := .data$concept_value)

    for (i in 2:length(versions)) {
        v <- versions[i]
        comparison <- comparison |>
            dplyr::full_join(
                results[[as.character(v)]] |>
                    dplyr::rename(!!paste0("v", v) := .data$concept_value),
                by = "object_key"
            )
    }

    # Compute agreement statistics
    version_cols <- paste0("v", versions)
    agreements <- list()
    for (i in 1:(length(versions) - 1)) {
        for (j in (i + 1):length(versions)) {
            col_i <- version_cols[i]
            col_j <- version_cols[j]
            agree <- sum(comparison[[col_i]] == comparison[[col_j]], na.rm = TRUE)
            total <- sum(!is.na(comparison[[col_i]]) & !is.na(comparison[[col_j]]))
            agreements[[paste0(col_i, "_vs_", col_j)]] <- list(
                agreements = agree,
                total = total,
                agreement_rate = if (total > 0) agree / total else NA_real_
            )
        }
    }

    cli::cli_alert_info("Compared {length(versions)} versions across {nrow(comparison)} objects")

    list(
        concept_id = concept_id,
        scope = scope,
        versions = versions,
        comparison = comparison,
        agreements = agreements,
        summary = tibble::tibble(
            version = versions,
            true_count = sapply(version_cols, function(col) sum(comparison[[col]] == TRUE, na.rm = TRUE)),
            false_count = sapply(version_cols, function(col) sum(comparison[[col]] == FALSE, na.rm = TRUE)),
            null_count = sapply(version_cols, function(col) sum(is.na(comparison[[col]])))
        )
    )
}

#' Cohort Comparison
#'
#' Compares concept prevalence across different cohorts.
#'
#' @param concept_id Character. The concept to analyze.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param cohort_ids Character vector. Cohort IDs to compare.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with per-cohort prevalence statistics.
#'
#' @export
ont_cohort_compare <- function(concept_id,
                                scope,
                                version = NULL,
                                cohort_ids,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        version <- cv$version
    }

    results <- list()
    for (cohort_id in cohort_ids) {
        cohort <- DBI::dbGetQuery(con, "
            SELECT * FROM ont_cohorts WHERE cohort_id = ?
        ", params = list(cohort_id))

        if (nrow(cohort) == 0) {
            cli::cli_warn("Unknown cohort: {.val {cohort_id}}")
            next
        }

        # Get cohort filter
        if (cohort$definition_type == "sql") {
            filter <- cohort$sql_expr
        } else {
            # For explicit cohorts, build IN clause
            members <- ont_get_cohort_members(cohort_id, con)
            obj_meta <- ont_get_object(cohort$object_type, con)
            members_str <- paste0("'", members, "'", collapse = ", ")
            filter <- glue::glue("{obj_meta$pk_column} IN ({members_str})")
        }

        # Evaluate concept for this cohort
        eval_result <- ont_evaluate(
            concept_id = concept_id,
            scope = scope,
            version = version,
            filter_expr = filter,
            collect = TRUE,
            con = con
        )

        total <- nrow(eval_result)
        true_count <- sum(eval_result$concept_value == TRUE, na.rm = TRUE)

        results[[cohort_id]] <- tibble::tibble(
            cohort_id = cohort_id,
            cohort_name = cohort$cohort_name,
            total_objects = total,
            concept_true = true_count,
            concept_false = total - true_count - sum(is.na(eval_result$concept_value)),
            prevalence_rate = if (total > 0) true_count / total else NA_real_
        )
    }

    comparison <- dplyr::bind_rows(results)

    cli::cli_alert_info("Compared {length(cohort_ids)} cohorts for {concept_id}@{scope} v{version}")

    comparison
}

#' Log Analysis Run
#'
#' Records an analysis execution for reproducibility and audit trail.
#'
#' @param analysis_type Character. Type of analysis.
#' @param concept_id Character. The concept analyzed.
#' @param scope Character. The scope (if applicable).
#' @param parameters List. Analysis parameters (will be converted to JSON).
#' @param results_summary List. Key findings (will be converted to JSON).
#' @param executed_by Character. Who ran the analysis.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the analysis_id.
#'
#' @keywords internal
#' @export
ont_log_analysis <- function(analysis_type,
                              concept_id,
                              scope = NULL,
                              parameters = NULL,
                              results_summary = NULL,
                              executed_by = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    analysis_id <- paste0("ANALYSIS-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                           substr(digest::digest(stats::runif(1)), 1, 6))

    params_json <- if (!is.null(parameters)) {
        as.character(jsonlite::toJSON(parameters, auto_unbox = TRUE))
    } else {
        NA_character_
    }

    results_json <- if (!is.null(results_summary)) {
        as.character(jsonlite::toJSON(results_summary, auto_unbox = TRUE))
    } else {
        NA_character_
    }

    DBI::dbExecute(con, "
        INSERT INTO ont_analysis_runs
        (analysis_id, analysis_type, concept_id, scope, parameters, results_summary, executed_by)
        VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
        analysis_id, analysis_type, concept_id, null_to_na(scope),
        params_json, results_json, null_to_na(executed_by)
    ))

    invisible(analysis_id)
}

#' List Analysis Runs
#'
#' Retrieves analysis execution history.
#'
#' @param concept_id Character. Optional filter by concept.
#' @param analysis_type Character. Optional filter by type.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of analysis runs.
#'
#' @export
ont_list_analyses <- function(concept_id = NULL, analysis_type = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_analysis_runs")

    if (!is.null(concept_id)) {
        result <- result |> dplyr::filter(.data$concept_id == !!concept_id)
    }
    if (!is.null(analysis_type)) {
        result <- result |> dplyr::filter(.data$analysis_type == !!analysis_type)
    }

    result |>
        dplyr::arrange(dplyr::desc(.data$executed_at)) |>
        dplyr::collect()
}
