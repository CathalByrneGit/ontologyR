#' @title Drift Detection
#' @description Functions for detecting when concept definitions have drifted
#'   from reality, based on audit disagreement rates.
#' @name drift
NULL

#' Check for Drift
#'
#' Evaluates whether a concept version shows signs of drift based on audit
#' data. Drift is detected when the disagreement rate exceeds a threshold,
#' either in absolute terms or as a trend.
#'
#' @param concept_id Character. The concept to check.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, uses active version.
#' @param threshold Numeric. Disagreement rate threshold (0-1). Default 0.15.
#' @param min_audits Integer
#' . Minimum audits required before checking. Default 10.
#' @param window_days Integer. Days to look back for recent audits. Default 30.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with drift status and details.
#'
#' @export
ont_check_drift <- function(concept_id,
                             scope,
                             version = NULL,
                             threshold = 0.15,
                             min_audits = 10,
                             window_days = 30,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    # Get version
    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        version <- cv$version
    }

    # Get recent audits
    from_date <- Sys.time() - (window_days * 24 * 60 * 60)
    audits <- ont_get_audits(
        concept_id = concept_id,
        scope = scope,
        version = version,
        from = from_date,
        con = con
    )

    # Build result
    result <- list(
        concept_id = concept_id,
        scope = scope,
        version = version,
        checked_at = Sys.time(),
        window_days = window_days,
        threshold = threshold,
        audit_count = nrow(audits),
        has_sufficient_data = nrow(audits) >= min_audits,
        disagreement_rate = NA_real_,
        drift_detected = FALSE,
        drift_severity = NA_character_,
        message = NA_character_
    )

    # Check for sufficient data
    if (nrow(audits) < min_audits) {
        result$message <- glue::glue(
            "Insufficient audits ({nrow(audits)} < {min_audits}). Cannot assess drift."
        )
        cli::cli_alert_warning(result$message)
        return(result)
    }

    # Compute disagreement rate
    audits <- audits |>
        dplyr::mutate(agreement = .data$system_value == .data$reviewer_value)

    disagreement_rate <- mean(!audits$agreement)
    result$disagreement_rate <- disagreement_rate

    # Assess drift
    if (disagreement_rate >= threshold) {
        result$drift_detected <- TRUE

        # Severity classification
        if (disagreement_rate >= threshold * 2) {
            result$drift_severity <- "severe"
        } else if (disagreement_rate >= threshold * 1.5) {
            result$drift_severity <- "moderate"
        } else {
            result$drift_severity <- "mild"
        }

        result$message <- glue::glue(
            "DRIFT DETECTED: Disagreement rate {round(disagreement_rate*100,1)}% ",
            "exceeds threshold {round(threshold*100,1)}% ",
            "(severity: {result$drift_severity})"
        )
        cli::cli_alert_danger(result$message)
    } else {
        result$message <- glue::glue(
            "No drift detected. Disagreement rate {round(disagreement_rate*100,1)}% ",
            "is below threshold {round(threshold*100,1)}%"
        )
        cli::cli_alert_success(result$message)
    }

    result
}

#' Run Drift Detection with Alerting
#'
#' Checks for drift and optionally creates a drift event record if detected.
#' This is the main entry point for automated drift monitoring.
#'
#' @param concept_id Character. The concept to check.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, uses active version.
#' @param threshold Numeric. Disagreement rate threshold. Default 0.15.
#' @param min_audits Integer. Minimum audits required. Default 10.
#' @param window_days Integer. Days to look back. Default 30.
#' @param create_event Logical. If `TRUE` and drift detected, create a
#'   drift event record. Default `TRUE`.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with drift check results and event_id if created.
#'
#' @export
ont_detect_drift <- function(concept_id,
                              scope,
                              version = NULL,
                              threshold = 0.15,
                              min_audits = 10,
                              window_days = 30,
                              create_event = TRUE,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    # Run drift check
    check <- ont_check_drift(
        concept_id = concept_id,
        scope = scope,
        version = version,
        threshold = threshold,
        min_audits = min_audits,
        window_days = window_days,
        con = con
    )

    # Create event if drift detected
    if (check$drift_detected && create_event) {
        # Check for existing open event
        existing <- DBI::dbGetQuery(con, "
            SELECT drift_id FROM ont_drift_events
            WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'
        ", params = list(concept_id, scope, check$version))

        if (nrow(existing) > 0) {
            check$event_id <- existing$drift_id[1]
            cli::cli_alert_info("Open drift event already exists: {.val {check$event_id}}")
        } else {
            check$event_id <- ont_create_drift_event(
                concept_id = concept_id,
                scope = scope,
                version = check$version,
                detection_type = "threshold",
                disagreement_rate = check$disagreement_rate,
                window_days = window_days,
                audit_count = check$audit_count,
                con = con
            )
        }
    }

    check
}

#' Create a Drift Event
#'
#' Records a drift detection event for tracking and governance.
#'
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param detection_type Character. How drift was detected: "threshold",
#'   "trend", or "manual".
#' @param disagreement_rate Numeric. The disagreement rate at detection.
#' @param window_days Integer. Window size used for detection.
#' @param audit_count Integer. Number of audits in the window.
#' @param con A DBI connection.
#'
#' @return The drift_id of the created event.
#'
#' @keywords internal
#' @export
ont_create_drift_event <- function(concept_id,
                                    scope,
                                    version,
                                    detection_type,
                                    disagreement_rate,
                                    window_days,
                                    audit_count,
                                    con = NULL) {
    con <- con %||% ont_get_connection()

    drift_id <- paste0("DRIFT-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                       substr(digest::digest(stats::runif(1)), 1, 6))

    window_end <- Sys.time()
    window_start <- window_end - (window_days * 24 * 60 * 60)

    DBI::dbExecute(con, "
        INSERT INTO ont_drift_events
        (drift_id, concept_id, scope, version, detected_at, detection_type,
         disagreement_rate, window_start, window_end, audit_count, status)
        VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP, ?, ?, ?, ?, ?, 'open')
    ", params = list(
        drift_id, concept_id, scope, as.integer(version), detection_type,
        disagreement_rate, window_start, window_end, audit_count
    ))

    # Log governance action
    ont_log_governance(
        action_type = "drift_detected",
        concept_id = concept_id,
        scope = scope,
        version = version,
        actor = "system",
        rationale = glue::glue("Disagreement rate {round(disagreement_rate*100,1)}%"),
        evidence = as.character(jsonlite::toJSON(list(
            drift_id = drift_id,
            disagreement_rate = disagreement_rate,
            audit_count = audit_count
        ), auto_unbox = TRUE)),
        con = con
    )

    cli::cli_alert_warning("Created drift event: {.val {drift_id}}")
    drift_id
}

#' List Drift Events
#'
#' Returns drift events, optionally filtered by status.
#'
#' @param concept_id Character. Optional filter by concept.
#' @param status Character. Optional filter by status: "open", "investigating",
#'   "resolved", "accepted".
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of drift events.
#'
#' @export
ont_list_drift_events <- function(concept_id = NULL, status = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_drift_events")

    if (!is.null(concept_id)) {
        result <- result |> dplyr::filter(.data$concept_id == !!concept_id)
    }

    if (!is.null(status)) {
        result <- result |> dplyr::filter(.data$status == !!status)
    }

    result |>
        dplyr::arrange(dplyr::desc(.data$detected_at)) |>
        dplyr::collect()
}

#' Resolve a Drift Event
#'
#' Marks a drift event as resolved with a resolution note.
#'
#' @param drift_id Character. The drift event to resolve.
#' @param resolution Character. How the drift was resolved.
#' @param resolved_by Character. Who resolved it.
#' @param status Character. New status: "resolved" (drift was fixed) or
#'   "accepted" (drift is acceptable/expected).
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
ont_resolve_drift <- function(drift_id,
                               resolution,
                               resolved_by,
                               status = "resolved",
                               con = NULL) {
    con <- con %||% ont_get_connection()

    if (!status %in% c("resolved", "accepted", "investigating")) {
        cli::cli_abort("Invalid status. Must be 'resolved', 'accepted', or 'investigating'.")
    }

    # Get drift event details for logging
    event <- DBI::dbGetQuery(con, "
        SELECT concept_id, scope, version FROM ont_drift_events WHERE drift_id = ?
    ", params = list(drift_id))

    if (nrow(event) == 0) {
        cli::cli_abort("Unknown drift event: {.val {drift_id}}")
    }

    DBI::dbExecute(con, "
        UPDATE ont_drift_events
        SET status = ?, resolution = ?, resolved_at = CURRENT_TIMESTAMP, resolved_by = ?
        WHERE drift_id = ?
    ", params = list(status, resolution, resolved_by, drift_id))

    # Log governance action
    ont_log_governance(
        action_type = paste0("drift_", status),
        concept_id = event$concept_id,
        scope = event$scope,
        version = event$version,
        actor = resolved_by,
        rationale = resolution,
        evidence = as.character(jsonlite::toJSON(list(drift_id = drift_id), auto_unbox = TRUE)),
        con = con
    )

    cli::cli_alert_success("Drift event {.val {drift_id}} marked as {status}")
    invisible(TRUE)
}

#' Get Drift Status Summary
#'
#' Returns a summary of drift status across all concepts or a specific concept.
#'
#' @param concept_id Character. Optional filter by concept.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble summarizing drift status by concept/scope/version.
#'
#' @export
ont_drift_status <- function(concept_id = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get active concept versions
    versions <- dplyr::tbl(con, "ont_concept_versions") |>
        dplyr::filter(.data$status == "active")

    if (!is.null(concept_id)) {
        versions <- versions |> dplyr::filter(.data$concept_id == !!concept_id)
    }

    versions <- versions |> dplyr::collect()

    if (nrow(versions) == 0) {
        cli::cli_alert_info("No active concept versions found.")
        return(tibble::tibble())
    }

    # For each version, get latest drift status
    results_list <- lapply(seq_len(nrow(versions)), function(i) {
        row <- versions[i, ]
        concept_id <- row$concept_id
        scope <- row$scope
        version <- row$version
        
        # Get open drift events
        open_events <- DBI::dbGetQuery(con, "
            SELECT COUNT(*) as n FROM ont_drift_events
            WHERE concept_id = ? AND scope = ? AND version = ? AND status = 'open'
        ", params = list(concept_id, scope, version))$n

        # Get recent audit stats (suppress output)
        summary <- suppressMessages(ont_audit_summary(concept_id, scope, version, con = con))

        tibble::tibble(
            concept_id = concept_id,
            scope = scope,
            version = version,
            audit_count = summary$audit_count,
            disagreement_rate = summary$disagreement_rate,
            open_drift_events = open_events,
            status = dplyr::case_when(
                open_events > 0 ~ "DRIFTING",
                is.na(summary$disagreement_rate) ~ "NO_DATA",
                summary$disagreement_rate > 0.15 ~ "WARNING",
                TRUE ~ "OK"
            )
        )
    })
    
    results <- dplyr::bind_rows(results_list)

    # Display summary
    cli::cli_h2("Drift Status Summary")
    for (i in seq_len(nrow(results))) {
        row <- results[i, ]
        status_icon <- switch(row$status,
            "OK" = cli::col_green("\u2713"),
            "WARNING" = cli::col_yellow("\u26A0"),
            "DRIFTING" = cli::col_red("\u2717"),
            "NO_DATA" = cli::col_grey("?")
        )
        cli::cli_alert("{status_icon} {row$concept_id}@{row$scope} v{row$version}: {row$status}")
    }

    results
}
