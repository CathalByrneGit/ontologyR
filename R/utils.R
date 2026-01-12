#' @title Utility Functions
#' @description Helper functions used across the package.
#' @name utils
NULL

#' Null coalescing operator
#'
#' Returns the left-hand side if not NULL, otherwise the right-hand side.
#'
#' @param x Left-hand side value.
#' @param y Right-hand side value (default).
#'
#' @return x if not NULL, otherwise y.
#'
#' @keywords internal
#' @export
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Convert NULL to NA for DBI binding
#'
#' DuckDB parameter binding requires all values to have length 1.
#' This helper converts NULL to NA_character_ for text parameters.
#'
#' @param x Value that might be NULL.
#'
#' @return NA_character_ if x is NULL, otherwise x.
#'
#' @keywords internal
null_to_na <- function(x) {
    if (is.null(x)) NA_character_ else x
}

#' Generate a Unique ID
#'
#' Creates a unique identifier with optional prefix.
#'
#' @param prefix Character. Prefix for the ID.
#'
#' @return A character string.
#'
#' @keywords internal
#' @export
ont_generate_id <- function(prefix = "ID") {
    paste0(
        prefix, "-",
        format(Sys.time(), "%Y%m%d%H%M%S"), "-",
        substr(digest::digest(stats::runif(1)), 1, 8)
    )
}

#' Execute Raw SQL
#'
#' Executes arbitrary SQL against the ontology database. Use with caution.
#'
#' @param sql Character. SQL statement to execute.
#' @param params List. Optional parameters for parameterized queries.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return For SELECT queries, returns a tibble. For other queries, returns
#'   the number of affected rows.
#'
#' @export
ont_sql <- function(sql, params = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    # Detect query type
    sql_upper <- toupper(trimws(sql))
    is_select <- startsWith(sql_upper, "SELECT") ||
                 startsWith(sql_upper, "WITH")

    if (is_select) {
        if (!is.null(params)) {
            DBI::dbGetQuery(con, sql, params = params) |>
                tibble::as_tibble()
        } else {
            DBI::dbGetQuery(con, sql) |>
                tibble::as_tibble()
        }
    } else {
        if (!is.null(params)) {
            DBI::dbExecute(con, sql, params = params)
        } else {
            DBI::dbExecute(con, sql)
        }
    }
}

#' Export Ontology to JSON
#'
#' Exports the complete ontology (types, concepts, versions) to a JSON file
#' for backup, transfer, or documentation purposes.
#'
#' @param path Character. Path to output JSON file.
#' @param include_audits Logical. Include audit records? Default FALSE.
#' @param include_governance Logical. Include governance log? Default FALSE.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the path.
#'
#' @export
ont_export_json <- function(path,
                             include_audits = FALSE,
                             include_governance = FALSE,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    export <- list(
        exported_at = Sys.time(),
        version = utils::packageVersion("ontologyR"),
        object_types = ont_list_objects(con) |> as.list(),
        link_types = ont_list_links(con) |> as.list(),
        concepts = DBI::dbGetQuery(con, "SELECT * FROM ont_concepts"),
        concept_versions = DBI::dbGetQuery(con, "SELECT * FROM ont_concept_versions")
    )

    if (include_audits) {
        export$audits <- DBI::dbGetQuery(con, "SELECT * FROM ont_audits")
    }

    if (include_governance) {
        export$governance_log <- DBI::dbGetQuery(con, "SELECT * FROM ont_governance_log")
        export$drift_events <- DBI::dbGetQuery(con, "SELECT * FROM ont_drift_events")
    }

    jsonlite::write_json(export, path, pretty = TRUE, auto_unbox = TRUE)

    cli::cli_alert_success("Exported ontology to {.path {path}}")
    invisible(path)
}

#' Import Ontology from JSON
#'
#' Imports ontology definitions from a JSON file.
#'
#' @param path Character. Path to JSON file.
#' @param overwrite Logical. Overwrite existing definitions? Default FALSE.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_import_json <- function(path, overwrite = FALSE, con = NULL) {
    con <- con %||% ont_get_connection()

    if (!file.exists(path)) {
        cli::cli_abort("File not found: {.path {path}}")
    }

    import <- jsonlite::read_json(path, simplifyVector = TRUE)

    cli::cli_h2("Importing ontology from {.path {path}}")

    # Import object types
    if (!is.null(import$object_types) && length(import$object_types) > 0) {
        object_types <- tibble::as_tibble(import$object_types)
        for (i in seq_len(nrow(object_types))) {
            row <- object_types[i, ]
            tryCatch({
                ont_register_object(
                    object_type = row$object_type,
                    table_name = row$table_name,
                    pk_column = row$pk_column,
                    description = row$description,
                    owner_domain = row$owner_domain,
                    con = con
                )
            }, error = function(e) {
                if (!overwrite) cli::cli_warn("Skipping {row$object_type}: {e$message}")
            })
        }
    }

    # Import concepts
    if (!is.null(import$concepts) && nrow(import$concepts) > 0) {
        for (i in seq_len(nrow(import$concepts))) {
            row <- import$concepts[i, ]
            tryCatch({
                ont_define_concept(
                    concept_id = row$concept_id,
                    object_type = row$object_type,
                    description = row$description,
                    owner_domain = row$owner_domain,
                    con = con
                )
            }, error = function(e) {
                if (!overwrite) cli::cli_warn("Skipping concept {row$concept_id}: {e$message}")
            })
        }
    }

    # Import concept versions
    if (!is.null(import$concept_versions) && nrow(import$concept_versions) > 0) {
        for (i in seq_len(nrow(import$concept_versions))) {
            row <- import$concept_versions[i, ]
            tryCatch({
                ont_add_version(
                    concept_id = row$concept_id,
                    scope = row$scope,
                    version = row$version,
                    sql_expr = row$sql_expr,
                    status = row$status,
                    rationale = row$rationale,
                    con = con
                )
            }, error = function(e) {
                if (!overwrite) cli::cli_warn("Skipping version: {e$message}")
            })
        }
    }

    cli::cli_alert_success("Import complete")
    invisible(TRUE)
}

#' Validate SQL Expression
#'
#' Checks if a SQL expression is syntactically valid by attempting to
#' prepare it against the object type's table.
#'
#' @param sql_expr Character. SQL expression to validate.
#' @param object_type Character. Object type to validate against.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with `valid` (logical) and `error` (character or NULL).
#'
#' @export
ont_validate_sql <- function(sql_expr, object_type, con = NULL) {
    con <- con %||% ont_get_connection()

    meta <- ont_get_object(object_type, con)

    # Try to execute with LIMIT 0
    test_sql <- glue::glue(
        "SELECT ({sql_expr}) as test_value FROM {meta$table_name} LIMIT 0"
    )

    result <- tryCatch({
        DBI::dbGetQuery(con, test_sql)
        list(valid = TRUE, error = NULL)
    }, error = function(e) {
        list(valid = FALSE, error = e$message)
    })

    if (result$valid) {
        cli::cli_alert_success("SQL expression is valid")
    } else {
        cli::cli_alert_danger("SQL expression invalid: {result$error}")
    }

    result
}

#' Print Method for Ontology Provenance
#'
#' Prints provenance information attached to evaluated concept results.
#'
#' @param x Object with ontology_provenance attribute.
#'
#' @keywords internal
print_provenance <- function(x) {
    prov <- attr(x, "ontology_provenance")
    if (is.null(prov)) {
        return(invisible(NULL))
    }

    cli::cli_h3("Ontology Provenance")
    cli::cli_ul(c(
        "Concept: {prov$concept_id}@{prov$scope} v{prov$version}",
        "Status: {prov$status}",
        "Object type: {prov$object_type}",
        "Evaluated: {prov$evaluated_at}",
        "SQL: {prov$sql_expr}"
    ))
}
