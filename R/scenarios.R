# =============================================================================
# Scenario Analysis
# =============================================================================
# Scenario analysis allows comparing concept definitions before deployment.
# This supports "what-if" analysis to understand the impact of definition
# changes before they go into production.
# =============================================================================

#' Scenario Analysis
#'
#' Compares the current concept definition with a proposed new definition,
#' showing which objects would be newly included, newly excluded, or unchanged.
#'
#' @param concept_id The concept to analyze.
#' @param scope The scope to analyze.
#' @param proposed_sql The proposed new SQL expression.
#' @param current_version Optional specific version to compare against.
#'   If NULL, uses the active version.
#' @param scenario_name Optional name for this scenario.
#' @param store If TRUE, stores the scenario for later review.
#' @param analyzed_by Optional identifier for who ran the analysis.
#' @param con Optional DBI connection.
#'
#' @return A list with:
#'   - summary: counts of current_matches, proposed_matches, newly_included, newly_excluded, unchanged
#'   - newly_included: objects that would be added
#'   - newly_excluded: objects that would be removed
#'   - current_only: objects matching only current definition
#'   - proposed_only: objects matching only proposed definition
#'   - both: objects matching both definitions
#'   - scenario_id: if stored
#'
#' @examples
#' \dontrun{
#' scenario <- ont_scenario_analysis(
#'   concept_id = "ready_for_discharge",
#'   scope = "clinical",
#'   proposed_sql = "los_days >= 2 AND NOT pending_results AND mobility_score > 3"
#' )
#'
#' # View impact summary
#' scenario$summary
#'
#' # See specific cases affected
#' scenario$newly_excluded
#' }
#'
#' @export
ont_scenario_analysis <- function(concept_id,
                                   scope,
                                   proposed_sql,
                                   current_version = NULL,
                                   scenario_name = NULL,
                                   store = TRUE,
                                   analyzed_by = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    # Get concept
    concept <- ont_get_concept(concept_id, con)
    if (is.null(concept)) {
        cli::cli_abort("Concept {.val {concept_id}} not found.")
    }

    # Get current version
    if (is.null(current_version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        if (is.null(cv)) {
            cli::cli_abort("No active version for {.val {concept_id}} in scope {.val {scope}}.")
        }
        current_version <- cv$version
        current_sql <- cv$sql_expr
    } else {
        cv <- ont_get_version(concept_id, scope, current_version, con)
        if (is.null(cv)) {
            cli::cli_abort("Version {current_version} not found.")
        }
        current_sql <- cv$sql_expr
    }

    # Get object type metadata
    obj_meta <- ont_get_object(concept$object_type, con)

    # Build comparison query
    query <- glue::glue("
        SELECT
            {obj_meta$pk_column} AS object_key,
            ({current_sql}) AS current_value,
            ({proposed_sql}) AS proposed_value,
            CASE
                WHEN ({current_sql}) AND ({proposed_sql}) THEN 'both'
                WHEN ({current_sql}) AND NOT ({proposed_sql}) THEN 'current_only'
                WHEN NOT ({current_sql}) AND ({proposed_sql}) THEN 'proposed_only'
                ELSE 'neither'
            END AS comparison
        FROM {obj_meta$table_name}
    ")

    comparison <- DBI::dbGetQuery(con, query)

    # Calculate summary
    current_matches <- sum(comparison$current_value, na.rm = TRUE)
    proposed_matches <- sum(comparison$proposed_value, na.rm = TRUE)
    both_count <- sum(comparison$comparison == "both", na.rm = TRUE)
    current_only_count <- sum(comparison$comparison == "current_only", na.rm = TRUE)
    proposed_only_count <- sum(comparison$comparison == "proposed_only", na.rm = TRUE)

    summary <- list(
        current_matches = current_matches,
        proposed_matches = proposed_matches,
        newly_included = proposed_only_count,
        newly_excluded = current_only_count,
        unchanged = both_count,
        net_change = proposed_matches - current_matches,
        percent_change = if (current_matches > 0) {
            round(100 * (proposed_matches - current_matches) / current_matches, 2)
        } else {
            NA
        }
    )

    # Extract object lists
    newly_included <- comparison[comparison$comparison == "proposed_only", "object_key", drop = FALSE]
    newly_excluded <- comparison[comparison$comparison == "current_only", "object_key", drop = FALSE]
    both <- comparison[comparison$comparison == "both", "object_key", drop = FALSE]

    # Store scenario if requested
    scenario_id <- NULL
    if (store) {
        scenario_id <- paste0("SCEN-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))

        results_json <- jsonlite::toJSON(list(
            newly_included = newly_included$object_key,
            newly_excluded = newly_excluded$object_key,
            both = both$object_key
        ), auto_unbox = TRUE)

        DBI::dbExecute(
            con,
            "INSERT INTO ont_scenarios (scenario_id, scenario_name, concept_id, scope,
             current_version, proposed_sql, analyzed_by, current_matches, proposed_matches,
             newly_included, newly_excluded, unchanged, results_json)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(
                scenario_id,
                null_to_na(scenario_name),
                concept_id,
                scope,
                current_version,
                proposed_sql,
                null_to_na(analyzed_by),
                current_matches,
                proposed_matches,
                proposed_only_count,
                current_only_count,
                both_count,
                results_json
            )
        )
    }

    cli::cli_alert_info("Scenario analysis for {.val {concept_id}}/{scope}:")
    cli::cli_alert_info("  Current: {current_matches} matches")
    cli::cli_alert_info("  Proposed: {proposed_matches} matches")
    cli::cli_alert_info("  Newly included: {proposed_only_count}")
    cli::cli_alert_info("  Newly excluded: {current_only_count}")
    cli::cli_alert_info("  Unchanged: {both_count}")

    list(
        summary = summary,
        newly_included = tibble::as_tibble(newly_included),
        newly_excluded = tibble::as_tibble(newly_excluded),
        both = tibble::as_tibble(both),
        comparison = tibble::as_tibble(comparison),
        current_sql = current_sql,
        proposed_sql = proposed_sql,
        scenario_id = scenario_id
    )
}

#' Get Scenario
#'
#' Retrieves a stored scenario analysis.
#'
#' @param scenario_id The scenario ID.
#' @param con Optional DBI connection.
#'
#' @return A list with scenario details and results, or NULL if not found.
#'
#' @export
ont_get_scenario <- function(scenario_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_scenarios WHERE scenario_id = ?",
        params = list(scenario_id)
    )

    if (nrow(result) == 0) return(NULL)

    scenario <- as.list(result[1, ])

    # Parse results JSON
    if (!is.na(scenario$results_json)) {
        scenario$results <- jsonlite::fromJSON(scenario$results_json)
    }

    scenario
}

#' List Scenarios
#'
#' Lists stored scenario analyses.
#'
#' @param concept_id Optional filter by concept.
#' @param scope Optional filter by scope.
#' @param status Optional filter by status.
#' @param limit Maximum number of scenarios to return.
#' @param con Optional DBI connection.
#'
#' @return A tibble of scenarios.
#'
#' @export
ont_list_scenarios <- function(concept_id = NULL,
                                scope = NULL,
                                status = NULL,
                                limit = 100,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT scenario_id, scenario_name, concept_id, scope, current_version,
              current_matches, proposed_matches, newly_included, newly_excluded,
              unchanged, status, analysis_date, analyzed_by
              FROM ont_scenarios WHERE 1=1"
    params <- list()

    if (!is.null(concept_id)) {
        query <- paste(query, "AND concept_id = ?")
        params <- c(params, concept_id)
    }

    if (!is.null(scope)) {
        query <- paste(query, "AND scope = ?")
        params <- c(params, scope)
    }

    if (!is.null(status)) {
        query <- paste(query, "AND status = ?")
        params <- c(params, status)
    }

    query <- paste(query, "ORDER BY analysis_date DESC LIMIT ?")
    params <- c(params, limit)

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Approve Scenario
#'
#' Marks a scenario as approved, optionally implementing it as a new version.
#'
#' @param scenario_id The scenario to approve.
#' @param decided_by User approving the scenario.
#' @param decision_notes Optional notes on the decision.
#' @param implement If TRUE, creates a new version with the proposed SQL.
#' @param con Optional DBI connection.
#'
#' @return If implement=TRUE, returns the new version number. Otherwise TRUE.
#'
#' @export
ont_approve_scenario <- function(scenario_id,
                                  decided_by,
                                  decision_notes = NULL,
                                  implement = FALSE,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    scenario <- ont_get_scenario(scenario_id, con)
    if (is.null(scenario)) {
        cli::cli_abort("Scenario {.val {scenario_id}} not found.")
    }

    if (scenario$status != "analyzed") {
        cli::cli_abort("Scenario {.val {scenario_id}} is not in 'analyzed' status.")
    }

    new_version <- NULL

    if (implement) {
        # Get next version number
        versions <- ont_list_versions(scenario$concept_id, con = con)
        scope_versions <- versions[versions$scope == scenario$scope, ]
        next_version <- max(scope_versions$version, 0) + 1

        # Create new version
        ont_add_version(
            concept_id = scenario$concept_id,
            scope = scenario$scope,
            version = next_version,
            sql_expr = scenario$proposed_sql,
            status = "draft",
            rationale = glue::glue("Implemented from scenario {scenario_id}"),
            con = con
        )

        new_version <- next_version
        status <- "implemented"
    } else {
        status <- "approved"
    }

    # Update scenario
    DBI::dbExecute(
        con,
        "UPDATE ont_scenarios SET status = ?, decided_by = ?, decision_notes = ?,
         decided_at = CURRENT_TIMESTAMP WHERE scenario_id = ?",
        params = list(status, decided_by, null_to_na(decision_notes), scenario_id)
    )

    cli::cli_alert_success("Approved scenario {.val {scenario_id}}")
    if (!is.null(new_version)) {
        cli::cli_alert_success("Created version {new_version} for {scenario$concept_id}/{scenario$scope}")
    }

    if (implement) new_version else TRUE
}

#' Reject Scenario
#'
#' Marks a scenario as rejected.
#'
#' @param scenario_id The scenario to reject.
#' @param decided_by User rejecting the scenario.
#' @param decision_notes Reason for rejection.
#' @param con Optional DBI connection.
#'
#' @return TRUE if successful.
#'
#' @export
ont_reject_scenario <- function(scenario_id,
                                 decided_by,
                                 decision_notes = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "UPDATE ont_scenarios SET status = 'rejected', decided_by = ?, decision_notes = ?,
         decided_at = CURRENT_TIMESTAMP WHERE scenario_id = ?",
        params = list(decided_by, null_to_na(decision_notes), scenario_id)
    )

    cli::cli_alert_info("Rejected scenario {.val {scenario_id}}")
    TRUE
}

#' Compare Multiple Scenarios
#'
#' Compares multiple proposed definitions against the current version.
#'
#' @param concept_id The concept to analyze.
#' @param scope The scope to analyze.
#' @param proposals Named list of proposed SQL expressions.
#' @param con Optional DBI connection.
#'
#' @return A tibble comparing all proposals.
#'
#' @examples
#' \dontrun{
#' comparison <- ont_compare_scenarios(
#'   concept_id = "high_risk",
#'   scope = "clinical",
#'   proposals = list(
#'     conservative = "risk_score >= 80",
#'     moderate = "risk_score >= 70",
#'     aggressive = "risk_score >= 60"
#'   )
#' )
#' }
#'
#' @export
ont_compare_scenarios <- function(concept_id, scope, proposals, con = NULL) {
    con <- con %||% ont_get_connection()

    results <- lapply(names(proposals), function(name) {
        scenario <- ont_scenario_analysis(
            concept_id = concept_id,
            scope = scope,
            proposed_sql = proposals[[name]],
            scenario_name = name,
            store = FALSE,
            con = con
        )

        tibble::tibble(
            proposal_name = name,
            proposed_sql = proposals[[name]],
            current_matches = scenario$summary$current_matches,
            proposed_matches = scenario$summary$proposed_matches,
            newly_included = scenario$summary$newly_included,
            newly_excluded = scenario$summary$newly_excluded,
            net_change = scenario$summary$net_change,
            percent_change = scenario$summary$percent_change
        )
    })

    dplyr::bind_rows(results)
}

#' Impact Analysis
#'
#' Analyzes the downstream impact of a proposed definition change,
#' including affected dashboards, materializations, and dependent concepts.
#'
#' @param concept_id The concept being changed.
#' @param scope The scope being changed.
#' @param proposed_sql The proposed new SQL expression.
#' @param con Optional DBI connection.
#'
#' @return A list with:
#'   - scenario: the scenario analysis results
#'   - affected_dashboards: dashboards using this concept version
#'   - affected_datasets: materialized datasets from this concept
#'   - recommendations: suggested actions
#'
#' @export
ont_impact_analysis <- function(concept_id, scope, proposed_sql, con = NULL) {
    con <- con %||% ont_get_connection()

    # Run scenario analysis
    scenario <- ont_scenario_analysis(
        concept_id = concept_id,
        scope = scope,
        proposed_sql = proposed_sql,
        store = TRUE,
        con = con
    )

    # Get active version
    cv <- ont_get_active_version(concept_id, scope, con = con)
    version <- if (!is.null(cv)) cv$version else NULL

    # Find affected dashboards
    affected_dashboards <- if (!is.null(version)) {
        DBI::dbGetQuery(con,
            "SELECT * FROM ont_dashboard_registry WHERE concept_id = ? AND scope = ? AND version = ?",
            params = list(concept_id, scope, version)
        )
    } else {
        data.frame()
    }

    # Find affected materialized datasets
    affected_datasets <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_datasets WHERE source_concept_id = ? AND source_scope = ?",
        params = list(concept_id, scope)
    )

    # Generate recommendations
    recommendations <- character(0)

    if (nrow(affected_dashboards) > 0) {
        recommendations <- c(recommendations,
            glue::glue("Review {nrow(affected_dashboards)} dashboard(s) that use this concept")
        )
    }

    if (nrow(affected_datasets) > 0) {
        recommendations <- c(recommendations,
            glue::glue("Plan to re-materialize {nrow(affected_datasets)} dataset(s)")
        )
    }

    if (scenario$summary$newly_excluded > 0) {
        recommendations <- c(recommendations,
            glue::glue("{scenario$summary$newly_excluded} objects will no longer match - verify this is intended")
        )
    }

    if (scenario$summary$newly_included > 0) {
        recommendations <- c(recommendations,
            glue::glue("{scenario$summary$newly_included} objects will newly match - sample and verify")
        )
    }

    if (abs(scenario$summary$percent_change) > 20) {
        recommendations <- c(recommendations,
            "Large change (>20%) - consider phased rollout or additional audit sampling"
        )
    }

    list(
        scenario = scenario,
        affected_dashboards = tibble::as_tibble(affected_dashboards),
        affected_datasets = tibble::as_tibble(affected_datasets),
        recommendations = recommendations
    )
}

#' Scenario Diff
#'
#' Returns a detailed diff between current and proposed definitions,
#' including sample objects from each category.
#'
#' @param scenario_id The scenario to diff.
#' @param sample_size Number of sample objects to include per category.
#' @param con Optional DBI connection.
#'
#' @return A list with detailed diff information.
#'
#' @export
ont_scenario_diff <- function(scenario_id, sample_size = 5, con = NULL) {
    con <- con %||% ont_get_connection()

    scenario <- ont_get_scenario(scenario_id, con)
    if (is.null(scenario)) {
        cli::cli_abort("Scenario {.val {scenario_id}} not found.")
    }

    # Get samples from stored results
    results <- scenario$results

    newly_included_sample <- if (length(results$newly_included) > 0) {
        head(results$newly_included, sample_size)
    } else {
        character(0)
    }

    newly_excluded_sample <- if (length(results$newly_excluded) > 0) {
        head(results$newly_excluded, sample_size)
    } else {
        character(0)
    }

    # Get concept metadata
    concept <- ont_get_concept(scenario$concept_id, con)
    obj_meta <- ont_get_object(concept$object_type, con)

    # Fetch sample data if we have samples
    newly_included_data <- if (length(newly_included_sample) > 0) {
        placeholders <- paste(rep("?", length(newly_included_sample)), collapse = ", ")
        query <- glue::glue("SELECT * FROM {obj_meta$table_name} WHERE {obj_meta$pk_column} IN ({placeholders})")
        DBI::dbGetQuery(con, query, params = as.list(newly_included_sample))
    } else {
        data.frame()
    }

    newly_excluded_data <- if (length(newly_excluded_sample) > 0) {
        placeholders <- paste(rep("?", length(newly_excluded_sample)), collapse = ", ")
        query <- glue::glue("SELECT * FROM {obj_meta$table_name} WHERE {obj_meta$pk_column} IN ({placeholders})")
        DBI::dbGetQuery(con, query, params = as.list(newly_excluded_sample))
    } else {
        data.frame()
    }

    list(
        scenario_id = scenario_id,
        concept_id = scenario$concept_id,
        scope = scenario$scope,
        current_version = scenario$current_version,
        proposed_sql = scenario$proposed_sql,
        summary = list(
            current_matches = scenario$current_matches,
            proposed_matches = scenario$proposed_matches,
            newly_included = scenario$newly_included,
            newly_excluded = scenario$newly_excluded,
            unchanged = scenario$unchanged
        ),
        samples = list(
            newly_included = tibble::as_tibble(newly_included_data),
            newly_excluded = tibble::as_tibble(newly_excluded_data)
        )
    )
}
