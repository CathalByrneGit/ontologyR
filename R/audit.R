#' @title Audit Functions
#' @description Functions for sampling objects for audit, recording human
#'   judgments, and comparing system evaluations to reviewer assessments.
#' @name audit
NULL

#' Sample Objects for Audit
#'
#' Selects a random sample of objects that meet a concept definition for
#' human review. This is the foundation of drift detection: by regularly
#' auditing samples, you can detect when definitions diverge from reality.
#'
#' @param concept_id Character. The concept to sample from.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, uses active version.
#' @param n Integer. Number of objects to sample.
#' @param where Character. Optional SQL WHERE clause to filter the sample
#'   population (e.g., only sample from today's cases).
#' @param concept_value Logical. If `TRUE` (default), only sample objects
#'   where the concept evaluates to TRUE. Set to `FALSE` to sample
#'   regardless of concept value, or `NA` to sample where concept is FALSE.
#' @param seed Integer. Random seed for reproducibility.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of sampled objects with their concept values.
#'
#' @examples
#' ont_connect(":memory:")
#' # ... setup ...
#'
#' # Sample 20 cases that the system says are "ready for discharge"
#' sample <- ont_sample_for_audit(
#'     concept_id = "ready_for_discharge",
#'     scope = "flow",
#'     n = 20,
#'     concept_value = TRUE
#' )
#'
#' @export
ont_sample_for_audit <- function(concept_id,
                                  scope,
                                  version = NULL,
                                  n = 20,
                                  where = NULL,
                                  concept_value = TRUE,
                                  seed = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    # Get version
    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        version <- cv$version
    }

    # Evaluate concept
    evaluated <- ont_evaluate(
        concept_id, scope, version,
        filter_expr = where,
        collect = FALSE,
        con = con
    )

    # Filter by concept value if specified
    if (!is.na(concept_value)) {
        if (isTRUE(concept_value)) {
            evaluated <- evaluated |> dplyr::filter(.data$concept_value == TRUE)
        } else {
            evaluated <- evaluated |> dplyr::filter(.data$concept_value == FALSE)
        }
    }

    # Set seed if provided
    if (!is.null(seed)) {
        set.seed(seed)
    }

    # Sample using DuckDB's random ordering
    # Note: This collects then samples in R for reproducibility
    all_matches <- dplyr::collect(evaluated)

    if (nrow(all_matches) == 0) {
        cli::cli_warn("No objects match the criteria for sampling.")
        return(tibble::tibble())
    }

    sample_size <- min(n, nrow(all_matches))
    sample_idx <- sample(seq_len(nrow(all_matches)), sample_size)

    sample <- all_matches[sample_idx, ]

    # Add metadata
    attr(sample, "audit_sample_meta") <- list(
        concept_id = concept_id,
        scope = scope,
        version = version,
        requested_n = n,
        actual_n = nrow(sample),
        population_n = nrow(all_matches),
        sampled_at = Sys.time(),
        seed = seed
    )

    cli::cli_alert_success(
        "Sampled {nrow(sample)} of {nrow(all_matches)} objects for audit"
    )

    sample
}

#' Record an Audit Judgment
#'
#' Records a human reviewer's judgment about whether an object truly meets
#' a concept definition. The comparison between `system_value` (what the
#' concept evaluated to) and `reviewer_value` (what the human judged) is
#' the basis for drift detection.
#'
#' @param concept_id Character. The concept being audited.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param object_key Character. Primary key of the audited object.
#' @param system_value Logical. What the concept evaluated to.
#' @param reviewer_value Logical. What the reviewer judged.
#' @param reviewer_id Character. Identifier for the reviewer.
#' @param notes Character. Optional notes explaining the judgment.
#' @param audit_id Character. Optional custom audit ID. If `NULL`, auto-generated.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the audit_id.
#'
#' @export
ont_record_audit <- function(concept_id,
                              scope,
                              version,
                              object_key,
                              system_value,
                              reviewer_value,
                              reviewer_id,
                              notes = NULL,
                              audit_id = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    # Generate audit ID if not provided
    if (is.null(audit_id)) {
        audit_id <- paste0("AUD-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                           substr(digest::digest(stats::runif(1)), 1, 8))
    }

    DBI::dbExecute(con, "
        INSERT INTO ont_audits
        (audit_id, concept_id, scope, version, object_key,
         system_value, reviewer_value, reviewer_id, notes, audited_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
        audit_id, concept_id, scope, as.integer(version), object_key,
        system_value, reviewer_value, reviewer_id, notes
    ))

    agreement <- system_value == reviewer_value
    if (agreement) {
        cli::cli_alert_success(
            "Recorded audit {.val {audit_id}}: system and reviewer AGREE"
        )
    } else {
        cli::cli_alert_warning(
            "Recorded audit {.val {audit_id}}: system and reviewer DISAGREE"
        )
    }

    invisible(audit_id)
}

#' Record Multiple Audit Judgments
#'
#' Batch version of `ont_record_audit()` for recording multiple judgments
#' at once, typically from an audit session.
#'
#' @param audits A data frame with columns: object_key, system_value,
#'   reviewer_value, and optionally notes.
#' @param concept_id Character. The concept being audited.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param reviewer_id Character. Identifier for the reviewer.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns a vector of audit_ids.
#'
#' @export
ont_record_audits <- function(audits,
                               concept_id,
                               scope,
                               version,
                               reviewer_id,
                               con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate input
    required_cols <- c("object_key", "system_value", "reviewer_value")
    missing <- setdiff(required_cols, names(audits))
    if (length(missing) > 0) {
        cli::cli_abort("Missing required columns: {.val {missing}}")
    }

    audit_ids <- character(nrow(audits))

    for (i in seq_len(nrow(audits))) {
        audit_ids[i] <- ont_record_audit(
            concept_id = concept_id,
            scope = scope,
            version = version,
            object_key = audits$object_key[i],
            system_value = audits$system_value[i],
            reviewer_value = audits$reviewer_value[i],
            reviewer_id = reviewer_id,
            notes = if ("notes" %in% names(audits)) audits$notes[i] else NULL,
            con = con
        )
    }

    cli::cli_alert_success("Recorded {length(audit_ids)} audit judgments")
    invisible(audit_ids)
}

#' Get Audit History
#'
#' Retrieves audit records for a concept version, optionally filtered by
#' date range.
#'
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version. If `NULL`, returns all versions.
#' @param from Date or POSIXct. Start of date range.
#' @param to Date or POSIXct. End of date range.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of audit records.
#'
#' @export
ont_get_audits <- function(concept_id,
                            scope = NULL,
                            version = NULL,
                            from = NULL,
                            to = NULL,
                            con = NULL) {
    con <- con %||% ont_get_connection()

    result <- dplyr::tbl(con, "ont_audits") |>
        dplyr::filter(.data$concept_id == !!concept_id)

    if (!is.null(scope)) {
        result <- result |> dplyr::filter(.data$scope == !!scope)
    }

    if (!is.null(version)) {
        result <- result |> dplyr::filter(.data$version == !!as.integer(version))
    }

    if (!is.null(from)) {
        result <- result |> dplyr::filter(.data$audited_at >= !!from)
    }

    if (!is.null(to)) {
        result <- result |> dplyr::filter(.data$audited_at <= !!to)
    }

    result |>
        dplyr::arrange(dplyr::desc(.data$audited_at)) |>
        dplyr::collect()
}

#' Compute Audit Summary Statistics
#'
#' Computes agreement/disagreement statistics for audits of a concept version.
#'
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param from Date or POSIXct. Start of date range.
#' @param to Date or POSIXct. End of date range.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with audit statistics.
#'
#' @export
ont_audit_summary <- function(concept_id,
                               scope,
                               version,
                               from = NULL,
                               to = NULL,
                               con = NULL) {
    audits <- ont_get_audits(
        concept_id = concept_id,
        scope = scope,
        version = version,
        from = from,
        to = to,
        con = con
    )

    if (nrow(audits) == 0) {
        cli::cli_alert_warning("No audits found for this concept version.")
        return(list(
            concept_id = concept_id,
            scope = scope,
            version = version,
            audit_count = 0,
            agreements = NA,
            disagreements = NA,
            disagreement_rate = NA
        ))
    }

    # Compute agreement
    audits <- audits |>
        dplyr::mutate(agreement = .data$system_value == .data$reviewer_value)

    summary <- list(
        concept_id = concept_id,
        scope = scope,
        version = version,
        audit_count = nrow(audits),
        agreements = sum(audits$agreement),
        disagreements = sum(!audits$agreement),
        disagreement_rate = mean(!audits$agreement),
        first_audit = min(audits$audited_at),
        last_audit = max(audits$audited_at),
        reviewers = unique(audits$reviewer_id)
    )

    # Print summary
    cli::cli_h3("Audit Summary: {concept_id}@{scope} v{version}")
    cli::cli_alert_info("Total audits: {summary$audit_count}")
    cli::cli_alert_info("Agreements: {summary$agreements} ({round(100*(1-summary$disagreement_rate),1)}%)")
    cli::cli_alert_info("Disagreements: {summary$disagreements} ({round(100*summary$disagreement_rate,1)}%)")
    cli::cli_alert_info("Period: {summary$first_audit} to {summary$last_audit}")

    invisible(summary)
}

#' Compute Rolling Disagreement Rate
#'
#' Computes disagreement rate over a rolling window, useful for detecting
#' trends in drift over time.
#'
#' @param concept_id Character. The concept.
#' @param scope Character. The scope.
#' @param version Integer. The version.
#' @param window_days Integer. Size of rolling window in days.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with date and rolling_disagreement_rate columns.
#'
#' @export
ont_rolling_disagreement <- function(concept_id,
                                      scope,
                                      version,
                                      window_days = 7,
                                      con = NULL) {
    # Check for zoo package
    if (!requireNamespace("zoo", quietly = TRUE)) {
        cli::cli_abort(c(
            "Package {.pkg zoo} is required for rolling calculations.",
            "i" = "Install it with: {.code install.packages(\"zoo\")}"
        ))
    }
    
    audits <- ont_get_audits(
        concept_id = concept_id,
        scope = scope,
        version = version,
        con = con
    )

    if (nrow(audits) == 0) {
        return(tibble::tibble(date = as.Date(character()), rolling_disagreement_rate = numeric()))
    }

    audits <- audits |>
        dplyr::mutate(
            date = as.Date(.data$audited_at),
            agreement = .data$system_value == .data$reviewer_value
        )

    # Daily aggregation
    daily <- audits |>
        dplyr::group_by(.data$date) |>
        dplyr::summarise(
            audits = dplyr::n(),
            disagreements = sum(!.data$agreement),
            .groups = "drop"
        ) |>
        dplyr::arrange(.data$date)

    # Compute rolling window
    if (nrow(daily) < 2) {
        return(daily |> dplyr::mutate(rolling_disagreement_rate = .data$disagreements / .data$audits))
    }

    # Fill in missing dates
    date_range <- seq(min(daily$date), max(daily$date), by = "day")
    daily_full <- tibble::tibble(date = date_range) |>
        dplyr::left_join(daily, by = "date") |>
        dplyr::mutate(
            audits = dplyr::coalesce(.data$audits, 0L),
            disagreements = dplyr::coalesce(.data$disagreements, 0L)
        )

    # Rolling sum
    daily_full <- daily_full |>
        dplyr::mutate(
            rolling_audits = zoo::rollsum(.data$audits, window_days, fill = NA, align = "right"),
            rolling_disagreements = zoo::rollsum(.data$disagreements, window_days, fill = NA, align = "right"),
            rolling_disagreement_rate = .data$rolling_disagreements / .data$rolling_audits
        )

    daily_full |>
        dplyr::select("date", "rolling_disagreement_rate", "rolling_audits") |>
        dplyr::filter(!is.na(.data$rolling_disagreement_rate))
}
