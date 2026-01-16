#' @title Dataset Registry and Materialization
#' @description Functions for managing datasets, materialization, and lineage tracking.
#' @name datasets
NULL

# =============================================================================
# Dataset Registry Functions
# =============================================================================

#' Register a Dataset
#'
#' Registers a dataset (table) in the ontology registry. Datasets can be source
#' tables, materialized concept outputs, or transform outputs.
#'
#' @param dataset_id Unique identifier for the dataset.
#' @param dataset_name Human-readable name.
#' @param physical_name The actual table/view name in the database.
#' @param dataset_type Type of dataset: "source", "materialized", or "transform".
#' @param object_type Optional object type this dataset represents.
#' @param description Optional description.
#' @param schema_json Optional JSON string describing the schema.
#' @param owner Optional owner/domain.
#' @param created_by Optional user who registered the dataset.
#' @param con Optional DBI connection. Uses active connection if NULL.
#'
#' @return The dataset_id (invisibly).
#'
#' @export
ont_register_dataset <- function(dataset_id,
                                  dataset_name,
                                  physical_name,
                                  dataset_type = c("source", "materialized", "transform"),
                                  object_type = NULL,
                                  description = NULL,
                                  schema_json = NULL,
                                  owner = NULL,
                                  created_by = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()
    dataset_type <- match.arg(dataset_type)

    # Check if dataset already exists
    existing <- DBI::dbGetQuery(
        con,
        "SELECT dataset_id FROM ont_datasets WHERE dataset_id = ?",
        params = list(dataset_id)
    )

    if (nrow(existing) > 0) {
        cli::cli_abort("Dataset {.val {dataset_id}} already exists.")
    }

    # Get row count if table exists
    row_count <- tryCatch({
        result <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) as n FROM {physical_name}"))
        result$n
    }, error = function(e) NA_integer_)

    DBI::dbExecute(
        con,
        "INSERT INTO ont_datasets (dataset_id, dataset_name, dataset_type, physical_name,
         object_type, description, schema_json, row_count, owner, created_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            dataset_id,
            dataset_name,
            dataset_type,
            physical_name,
            null_to_na(object_type),
            null_to_na(description),
            null_to_na(schema_json),
            row_count,
            null_to_na(owner),
            null_to_na(created_by)
        )
    )

    cli::cli_alert_success("Registered dataset: {.val {dataset_id}}")
    invisible(dataset_id)
}

#' List Datasets
#'
#' Returns a list of all registered datasets.
#'
#' @param dataset_type Optional filter by dataset type.
#' @param object_type Optional filter by object type.
#' @param con Optional DBI connection.
#'
#' @return A tibble of datasets.
#'
#' @export
ont_list_datasets <- function(dataset_type = NULL, object_type = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_datasets WHERE 1=1"
    params <- list()

    if (!is.null(dataset_type)) {
        query <- paste(query, "AND dataset_type = ?")
        params <- c(params, dataset_type)
    }

    if (!is.null(object_type)) {
        query <- paste(query, "AND object_type = ?")
        params <- c(params, object_type)
    }

    query <- paste(query, "ORDER BY created_at DESC")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Get Dataset Metadata
#'
#' Retrieves metadata for a specific dataset.
#'
#' @param dataset_id The dataset identifier.
#' @param con Optional DBI connection.
#'
#' @return A list with dataset metadata, or NULL if not found.
#'
#' @export
ont_get_dataset <- function(dataset_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_datasets WHERE dataset_id = ?",
        params = list(dataset_id)
    )

    if (nrow(result) == 0) {
        return(NULL)
    }

    as.list(result[1, ])
}

#' Update Dataset Row Count
#'
#' Updates the row count for a dataset by querying its physical table.
#'
#' @param dataset_id The dataset identifier.
#' @param con Optional DBI connection.
#'
#' @return The new row count (invisibly).
#'
#' @keywords internal
#' @export
ont_update_dataset_count <- function(dataset_id, con = NULL) {
    con <- con %||% ont_get_connection()

    ds <- ont_get_dataset(dataset_id, con)
    if (is.null(ds)) {
        cli::cli_abort("Dataset {.val {dataset_id}} not found.")
    }

    row_count <- tryCatch({
        result <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) as n FROM {ds$physical_name}"))
        result$n
    }, error = function(e) NA_integer_)

    DBI::dbExecute(
        con,
        "UPDATE ont_datasets SET row_count = ?, updated_at = CURRENT_TIMESTAMP WHERE dataset_id = ?",
        params = list(row_count, dataset_id)
    )

    invisible(row_count)
}

# =============================================================================
# Materialization Functions
# =============================================================================

#' Materialize a Concept
#'
#' Evaluates a concept and materializes the results to a persistent table,
#' creating a run record and lineage edges for full traceability.
#'
#' @param concept_id The concept to materialize.
#' @param scope The scope.
#' @param version Optional version (uses active version if NULL).
#' @param output_table Name for the output table. Defaults to concept_id + scope.
#' @param filter_expr Optional SQL filter expression.
#' @param include_all_columns If TRUE, includes all columns from source table.
#' @param triggered_by What triggered this materialization (e.g., "manual", "scheduled").
#' @param executed_by User who executed the materialization.
#' @param con Optional DBI connection.
#'
#' @return A list with run details including run_id, dataset_id, and row_count.
#'
#' @export
ont_materialize <- function(concept_id,
                             scope,
                             version = NULL,
                             output_table = NULL,
                             filter_expr = NULL,
                             include_all_columns = TRUE,
                             triggered_by = "manual",
                             executed_by = NULL,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    # Get concept and version info
    concept <- ont_get_concept(concept_id, con = con)
    if (is.null(concept)) {
        cli::cli_abort("Concept {.val {concept_id}} not found.")
    }

    if (is.null(version)) {
        cv <- ont_get_active_version(concept_id, scope, con = con)
        if (is.null(cv)) {
            cli::cli_abort("No active version for {.val {concept_id}} in scope {.val {scope}}.")
        }
        version <- cv$version
    } else {
        cv <- ont_get_version(concept_id, scope, version, con = con)
        if (is.null(cv)) {
            cli::cli_abort("Version {version} not found for {.val {concept_id}} in scope {.val {scope}}.")
        }
    }

    # Get object type metadata
    obj_meta <- ont_get_object(concept$object_type, con = con)
    if (is.null(obj_meta)) {
        cli::cli_abort("Object type {.val {concept$object_type}} not found.")
    }

    # Generate IDs
    run_id <- paste0("RUN-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))
    output_table <- output_table %||% paste0("mat_", concept_id, "_", scope)
    dataset_id <- paste0("DS-", output_table)

    # Build the SQL
    if (include_all_columns) {
        select_clause <- glue::glue("t.*, ({cv$sql_expr}) AS concept_value")
    } else {
        select_clause <- glue::glue("{obj_meta$pk_column} AS object_key, ({cv$sql_expr}) AS concept_value")
    }

    sql <- glue::glue("SELECT {select_clause} FROM {obj_meta$table_name} t")

    if (!is.null(filter_expr)) {
        sql <- glue::glue("{sql} WHERE {filter_expr}")
    }

    full_sql <- glue::glue("CREATE OR REPLACE TABLE {output_table} AS {sql}")

    # Create run record (status = running)
    DBI::dbExecute(
        con,
        "INSERT INTO ont_runs (run_id, run_type, status, concept_id, scope, version,
         sql_executed, filter_expr, triggered_by, executed_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            run_id,
            "materialization",
            "running",
            concept_id,
            scope,
            version,
            full_sql,
            null_to_na(filter_expr),
            triggered_by,
            null_to_na(executed_by)
        )
    )

    # Execute materialization
    start_time <- Sys.time()
    tryCatch({
        DBI::dbExecute(con, full_sql)

        # Get row count
        row_count <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) as n FROM {output_table}"))$n

        # Compute simple hash (row count + concept info)
        output_hash <- digest::digest(paste(row_count, concept_id, scope, version, Sys.time()))

        # Register the output dataset
        existing_ds <- ont_get_dataset(dataset_id, con)
        if (is.null(existing_ds)) {
            DBI::dbExecute(
                con,
                "INSERT INTO ont_datasets (dataset_id, dataset_name, dataset_type, physical_name,
                 object_type, row_count, source_concept_id, source_scope, source_version, source_filter, created_by)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                    dataset_id,
                    paste("Materialized:", concept_id, scope),
                    "materialized",
                    output_table,
                    concept$object_type,
                    row_count,
                    concept_id,
                    scope,
                    version,
                    null_to_na(filter_expr),
                    null_to_na(executed_by)
                )
            )
        } else {
            DBI::dbExecute(
                con,
                "UPDATE ont_datasets SET row_count = ?, updated_at = CURRENT_TIMESTAMP WHERE dataset_id = ?",
                params = list(row_count, dataset_id)
            )
        }

        # Update run record (success)
        DBI::dbExecute(
            con,
            "UPDATE ont_runs SET status = ?, ended_at = CURRENT_TIMESTAMP,
             output_dataset_id = ?, output_row_count = ?, output_hash = ?
             WHERE run_id = ?",
            params = list("completed", dataset_id, row_count, output_hash, run_id)
        )

        # Create lineage edge from source table to materialized output
        source_ds_id <- paste0("DS-", obj_meta$table_name)

        # Register source dataset if not exists
        if (is.null(ont_get_dataset(source_ds_id, con))) {
            DBI::dbExecute(
                con,
                "INSERT INTO ont_datasets (dataset_id, dataset_name, dataset_type, physical_name, object_type)
                 VALUES (?, ?, ?, ?, ?)",
                params = list(
                    source_ds_id,
                    obj_meta$table_name,
                    "source",
                    obj_meta$table_name,
                    concept$object_type
                )
            )
        }

        # Create lineage edge
        edge_id <- paste0("EDGE-", substr(uuid::UUIDgenerate(), 1, 12))
        DBI::dbExecute(
            con,
            "INSERT INTO ont_lineage_edges (edge_id, run_id, from_dataset_id, to_dataset_id, edge_type, details_json)
             VALUES (?, ?, ?, ?, ?, ?)",
            params = list(
                edge_id,
                run_id,
                source_ds_id,
                dataset_id,
                "materialization",
                jsonlite::toJSON(list(
                    concept_id = concept_id,
                    scope = scope,
                    version = version,
                    sql_expr = cv$sql_expr
                ), auto_unbox = TRUE)
            )
        )

        end_time <- Sys.time()
        duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

        cli::cli_alert_success("Materialized {.val {concept_id}} to {.val {output_table}}")
        cli::cli_alert_info("{row_count} rows in {round(duration, 2)} seconds")

        list(
            run_id = run_id,
            dataset_id = dataset_id,
            output_table = output_table,
            row_count = row_count,
            status = "completed",
            duration_secs = duration
        )

    }, error = function(e) {
        # Update run record (failed)
        DBI::dbExecute(
            con,
            "UPDATE ont_runs SET status = ?, ended_at = CURRENT_TIMESTAMP, log = ? WHERE run_id = ?",
            params = list("failed", e$message, run_id)
        )

        cli::cli_abort("Materialization failed: {e$message}")
    })
}

# =============================================================================
# Run Management Functions
# =============================================================================

#' List Runs
#'
#' Returns a list of materialization/transform runs.
#'
#' @param concept_id Optional filter by concept.
#' @param status Optional filter by status ("running", "completed", "failed").
#' @param limit Maximum number of runs to return.
#' @param con Optional DBI connection.
#'
#' @return A tibble of runs.
#'
#' @export
ont_list_runs <- function(concept_id = NULL, status = NULL, limit = 100, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_runs WHERE 1=1"
    params <- list()

    if (!is.null(concept_id)) {
        query <- paste(query, "AND concept_id = ?")
        params <- c(params, concept_id)
    }

    if (!is.null(status)) {
        query <- paste(query, "AND status = ?")
        params <- c(params, status)
    }

    query <- paste(query, "ORDER BY started_at DESC LIMIT ?")
    params <- c(params, limit)

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Get Run Details
#'
#' Retrieves details for a specific run.
#'
#' @param run_id The run identifier.
#' @param con Optional DBI connection.
#'
#' @return A list with run details, or NULL if not found.
#'
#' @export
ont_get_run <- function(run_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_runs WHERE run_id = ?",
        params = list(run_id)
    )

    if (nrow(result) == 0) {
        return(NULL)
    }

    as.list(result[1, ])
}

# =============================================================================
# Lineage Functions
# =============================================================================

#' Get Upstream Lineage
#'
#' Returns all datasets that are upstream (inputs) of a given dataset.
#'
#' @param dataset_id The dataset to trace upstream from.
#' @param depth Maximum depth to traverse (default 10).
#' @param con Optional DBI connection.
#'
#' @return A tibble of upstream datasets with their lineage edges.
#'
#' @export
ont_get_upstream <- function(dataset_id, depth = 10, con = NULL) {
    con <- con %||% ont_get_connection()

    # Recursive CTE to get all upstream datasets
    query <- glue::glue("
        WITH RECURSIVE upstream AS (
            SELECT e.edge_id, e.run_id, e.from_dataset_id, e.to_dataset_id,
                   e.edge_type, e.details_json, 1 as depth
            FROM ont_lineage_edges e
            WHERE e.to_dataset_id = ?

            UNION ALL

            SELECT e.edge_id, e.run_id, e.from_dataset_id, e.to_dataset_id,
                   e.edge_type, e.details_json, u.depth + 1
            FROM ont_lineage_edges e
            JOIN upstream u ON e.to_dataset_id = u.from_dataset_id
            WHERE u.depth < {depth}
        )
        SELECT DISTINCT u.*, d.dataset_name, d.dataset_type, d.physical_name
        FROM upstream u
        JOIN ont_datasets d ON u.from_dataset_id = d.dataset_id
        ORDER BY u.depth
    ")

    result <- DBI::dbGetQuery(con, query, params = list(dataset_id))
    tibble::as_tibble(result)
}

#' Get Downstream Lineage
#'
#' Returns all datasets that are downstream (outputs) of a given dataset.
#'
#' @param dataset_id The dataset to trace downstream from.
#' @param depth Maximum depth to traverse (default 10).
#' @param con Optional DBI connection.
#'
#' @return A tibble of downstream datasets with their lineage edges.
#'
#' @export
ont_get_downstream <- function(dataset_id, depth = 10, con = NULL) {
    con <- con %||% ont_get_connection()

    # Recursive CTE to get all downstream datasets
    query <- glue::glue("
        WITH RECURSIVE downstream AS (
            SELECT e.edge_id, e.run_id, e.from_dataset_id, e.to_dataset_id,
                   e.edge_type, e.details_json, 1 as depth
            FROM ont_lineage_edges e
            WHERE e.from_dataset_id = ?

            UNION ALL

            SELECT e.edge_id, e.run_id, e.from_dataset_id, e.to_dataset_id,
                   e.edge_type, e.details_json, d.depth + 1
            FROM ont_lineage_edges e
            JOIN downstream d ON e.from_dataset_id = d.to_dataset_id
            WHERE d.depth < {depth}
        )
        SELECT DISTINCT d.*, ds.dataset_name, ds.dataset_type, ds.physical_name
        FROM downstream d
        JOIN ont_datasets ds ON d.to_dataset_id = ds.dataset_id
        ORDER BY d.depth
    ")

    result <- DBI::dbGetQuery(con, query, params = list(dataset_id))
    tibble::as_tibble(result)
}

#' Get Full Lineage Graph
#'
#' Returns the full lineage graph for a dataset (both upstream and downstream).
#'
#' @param dataset_id The central dataset.
#' @param depth Maximum depth in each direction.
#' @param con Optional DBI connection.
#'
#' @return A list with nodes (datasets) and edges (lineage connections).
#'
#' @export
ont_get_lineage_graph <- function(dataset_id, depth = 10, con = NULL) {
    con <- con %||% ont_get_connection()

    upstream <- ont_get_upstream(dataset_id, depth, con)
    downstream <- ont_get_downstream(dataset_id, depth, con)

    # Get the central dataset
    central <- ont_get_dataset(dataset_id, con)

    # Combine all unique datasets
    all_dataset_ids <- unique(c(
        dataset_id,
        upstream$from_dataset_id,
        downstream$to_dataset_id
    ))

    nodes <- ont_list_datasets(con = con)
    nodes <- nodes[nodes$dataset_id %in% all_dataset_ids, ]

    # Combine edges
    edges <- rbind(
        if (nrow(upstream) > 0) upstream[, c("edge_id", "from_dataset_id", "to_dataset_id", "edge_type", "run_id")] else NULL,
        if (nrow(downstream) > 0) downstream[, c("edge_id", "from_dataset_id", "to_dataset_id", "edge_type", "run_id")] else NULL
    )

    list(
        central_dataset = dataset_id,
        nodes = nodes,
        edges = if (is.null(edges)) tibble::tibble() else tibble::as_tibble(edges)
    )
}

#' Get Dataset Provenance
#'
#' Returns the complete provenance chain for a materialized dataset,
#' including the concept definition and all transformation steps.
#'
#' @param dataset_id The dataset to get provenance for.
#' @param con Optional DBI connection.
#'
#' @return A list with provenance information.
#'
#' @export
ont_get_provenance <- function(dataset_id, con = NULL) {
    con <- con %||% ont_get_connection()

    ds <- ont_get_dataset(dataset_id, con)
    if (is.null(ds)) {
        cli::cli_abort("Dataset {.val {dataset_id}} not found.")
    }

    # Get the run that created this dataset
    runs <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_runs WHERE output_dataset_id = ? ORDER BY ended_at DESC LIMIT 1",
        params = list(dataset_id)
    )

    # Get upstream lineage
    upstream <- ont_get_upstream(dataset_id, con = con)

    # If it's a materialized concept, get concept info
    concept_info <- NULL
    if (!is.na(ds$source_concept_id)) {
        concept_info <- list(
            concept_id = ds$source_concept_id,
            scope = ds$source_scope,
            version = ds$source_version,
            filter = ds$source_filter
        )

        # Get the concept version details
        cv <- ont_get_version(ds$source_concept_id, ds$source_scope, ds$source_version, con = con)
        if (!is.null(cv)) {
            concept_info$sql_expr <- cv$sql_expr
            concept_info$status <- cv$status
        }
    }

    list(
        dataset = ds,
        last_run = if (nrow(runs) > 0) as.list(runs[1, ]) else NULL,
        concept = concept_info,
        upstream = upstream
    )
}
