#' @title Transform Management and Execution
#' @description Functions for defining, managing, and executing transforms with
#' full lineage tracking. Transforms define how derived datasets are produced.
#' @name transforms
NULL

# =============================================================================
# Transform Definition Functions
# =============================================================================

#' Define a Transform
#'
#' Creates a transform definition that specifies how an output dataset is produced.
#' Transforms can be SQL queries or references to R functions.
#'
#' @param transform_id Unique identifier for the transform.
#' @param transform_name Human-readable name.
#' @param output_dataset_id The dataset this transform produces.
#' @param transform_type Type: "sql", "r_function", or "concept_eval".
#' @param code SQL query or R function name.
#' @param input_datasets Character vector of input dataset IDs.
#' @param description Optional description.
#' @param created_by Optional user.
#' @param con Optional DBI connection.
#'
#' @return The transform_id (invisibly).
#'
#' @export
ont_define_transform <- function(transform_id,
                                   transform_name,
                                   output_dataset_id,
                                   transform_type = c("sql", "r_function", "concept_eval"),
                                   code = NULL,
                                   input_datasets = NULL,
                                   description = NULL,
                                   created_by = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()
    transform_type <- match.arg(transform_type)

    # Check if transform already exists
    existing <- DBI::dbGetQuery(
        con,
        "SELECT transform_id FROM ont_transforms WHERE transform_id = ?",
        params = list(transform_id)
    )

    if (nrow(existing) > 0) {
        cli::cli_abort("Transform {.val {transform_id}} already exists.")
    }

    # Ensure output dataset exists (or register placeholder)
    output_ds <- ont_get_dataset(output_dataset_id, con)
    if (is.null(output_ds)) {
        # Create placeholder dataset
        DBI::dbExecute(
            con,
            "INSERT INTO ont_datasets (dataset_id, dataset_name, dataset_type, physical_name)
             VALUES (?, ?, ?, ?)",
            params = list(
                output_dataset_id,
                transform_name,
                "derived",
                gsub("^DS-", "", output_dataset_id)  # Use dataset_id as table name
            )
        )
    }

    # Insert transform
    DBI::dbExecute(
        con,
        "INSERT INTO ont_transforms (transform_id, transform_name, output_dataset_id,
         transform_type, code, description, created_by)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
            transform_id,
            transform_name,
            output_dataset_id,
            transform_type,
            null_to_na(code),
            null_to_na(description),
            null_to_na(created_by)
        )
    )

    # Register input datasets
    if (!is.null(input_datasets) && length(input_datasets) > 0) {
        for (input_ds in input_datasets) {
            ont_add_transform_input(transform_id, input_ds, con = con)
        }
    }

    cli::cli_alert_success("Defined transform: {.val {transform_id}}")
    invisible(transform_id)
}

#' Get Transform Details
#'
#' Retrieves a transform definition with its inputs.
#'
#' @param transform_id The transform identifier.
#' @param con Optional DBI connection.
#'
#' @return A list with transform details and inputs, or NULL if not found.
#'
#' @export
ont_get_transform <- function(transform_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_transforms WHERE transform_id = ?",
        params = list(transform_id)
    )

    if (nrow(result) == 0) {
        return(NULL)
    }

    transform <- as.list(result[1, ])

    # Get inputs
    inputs <- DBI::dbGetQuery(
        con,
        "SELECT ti.*, d.dataset_name, d.physical_name
         FROM ont_transform_inputs ti
         JOIN ont_datasets d ON ti.input_dataset_id = d.dataset_id
         WHERE ti.transform_id = ?",
        params = list(transform_id)
    )

    transform$inputs <- tibble::as_tibble(inputs)
    transform
}

#' List Transforms
#'
#' Returns all defined transforms.
#'
#' @param output_dataset_id Optional filter by output dataset.
#' @param transform_type Optional filter by type.
#' @param con Optional DBI connection.
#'
#' @return A tibble of transforms.
#'
#' @export
ont_list_transforms <- function(output_dataset_id = NULL,
                                  transform_type = NULL,
                                  con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_transforms WHERE 1=1"
    params <- list()

    if (!is.null(output_dataset_id)) {
        query <- paste(query, "AND output_dataset_id = ?")
        params <- c(params, output_dataset_id)
    }

    if (!is.null(transform_type)) {
        query <- paste(query, "AND transform_type = ?")
        params <- c(params, transform_type)
    }

    query <- paste(query, "ORDER BY created_at DESC")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Update Transform Code
#'
#' Updates the code/SQL for a transform.
#'
#' @param transform_id The transform identifier.
#' @param code New SQL or function name.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_update_transform <- function(transform_id, code, con = NULL) {
    con <- con %||% ont_get_connection()

    transform <- ont_get_transform(transform_id, con)
    if (is.null(transform)) {
        cli::cli_abort("Transform {.val {transform_id}} not found.")
    }

    DBI::dbExecute(
        con,
        "UPDATE ont_transforms SET code = ? WHERE transform_id = ?",
        params = list(code, transform_id)
    )

    cli::cli_alert_success("Updated transform: {.val {transform_id}}")
    invisible(TRUE)
}

# =============================================================================
# Transform Input Management
# =============================================================================

#' Add Transform Input
#'
#' Registers an input dataset for a transform.
#'
#' @param transform_id The transform identifier.
#' @param input_dataset_id The input dataset ID.
#' @param input_role Role of this input: "primary", "join", "lookup", "filter".
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_add_transform_input <- function(transform_id,
                                      input_dataset_id,
                                      input_role = "primary",
                                      con = NULL) {
    con <- con %||% ont_get_connection()

    # Verify transform exists
    transform <- DBI::dbGetQuery(
        con,
        "SELECT transform_id FROM ont_transforms WHERE transform_id = ?",
        params = list(transform_id)
    )

    if (nrow(transform) == 0) {
        cli::cli_abort("Transform {.val {transform_id}} not found.")
    }

    # Verify input dataset exists
    input_ds <- ont_get_dataset(input_dataset_id, con)
    if (is.null(input_ds)) {
        cli::cli_abort("Input dataset {.val {input_dataset_id}} not found.")
    }

    # Check for existing input
    existing <- DBI::dbGetQuery(
        con,
        "SELECT * FROM ont_transform_inputs WHERE transform_id = ? AND input_dataset_id = ?",
        params = list(transform_id, input_dataset_id)
    )

    if (nrow(existing) > 0) {
        cli::cli_warn("Input {.val {input_dataset_id}} already registered for transform.")
        return(invisible(TRUE))
    }

    DBI::dbExecute(
        con,
        "INSERT INTO ont_transform_inputs (transform_id, input_dataset_id, input_role)
         VALUES (?, ?, ?)",
        params = list(transform_id, input_dataset_id, input_role)
    )

    invisible(TRUE)
}

#' Remove Transform Input
#'
#' Removes an input dataset from a transform.
#'
#' @param transform_id The transform identifier.
#' @param input_dataset_id The input dataset ID to remove.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
ont_remove_transform_input <- function(transform_id, input_dataset_id, con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbExecute(
        con,
        "DELETE FROM ont_transform_inputs WHERE transform_id = ? AND input_dataset_id = ?",
        params = list(transform_id, input_dataset_id)
    )

    invisible(TRUE)
}

#' Get Transform Inputs
#'
#' Returns all input datasets for a transform.
#'
#' @param transform_id The transform identifier.
#' @param con Optional DBI connection.
#'
#' @return A tibble of input datasets with their roles.
#'
#' @export
ont_get_transform_inputs <- function(transform_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(
        con,
        "SELECT ti.*, d.dataset_name, d.physical_name, d.dataset_type, d.row_count
         FROM ont_transform_inputs ti
         JOIN ont_datasets d ON ti.input_dataset_id = d.dataset_id
         WHERE ti.transform_id = ?",
        params = list(transform_id)
    )

    tibble::as_tibble(result)
}

# =============================================================================
# Transform Execution
# =============================================================================

#' Execute Transform
#'
#' Runs a transform, creating the output dataset and recording the run with
#' full lineage tracking.
#'
#' @param transform_id The transform to execute.
#' @param triggered_by What triggered this execution.
#' @param executed_by User who executed.
#' @param con Optional DBI connection.
#'
#' @return A list with run details.
#'
#' @export
ont_execute_transform <- function(transform_id,
                                    triggered_by = "manual",
                                    executed_by = NULL,
                                    con = NULL) {
    con <- con %||% ont_get_connection()

    transform <- ont_get_transform(transform_id, con)
    if (is.null(transform)) {
        cli::cli_abort("Transform {.val {transform_id}} not found.")
    }

    if (transform$transform_type != "sql") {
        cli::cli_abort("Only SQL transforms can be executed directly. Got: {transform$transform_type}")
    }

    if (is.na(transform$code) || transform$code == "") {
        cli::cli_abort("Transform has no SQL code defined.")
    }

    # Get input datasets for snapshot
    inputs <- ont_get_transform_inputs(transform_id, con)

    # Create input snapshot
    input_snapshot <- list()
    for (i in seq_len(nrow(inputs))) {
        ds_id <- inputs$input_dataset_id[i]
        ds <- ont_get_dataset(ds_id, con)
        input_snapshot[[ds_id]] <- list(
            row_count = ds$row_count,
            physical_name = ds$physical_name
        )
    }

    # Generate run ID
    run_id <- paste0("RUN-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))

    # Get output table name
    output_ds <- ont_get_dataset(transform$output_dataset_id, con)
    output_table <- output_ds$physical_name

    # Build CREATE TABLE AS statement
    full_sql <- glue::glue("CREATE OR REPLACE TABLE {output_table} AS {transform$code}")

    # Create run record (status = running)
    DBI::dbExecute(
        con,
        "INSERT INTO ont_runs (run_id, transform_id, run_type, status, input_snapshot,
         output_dataset_id, sql_executed, triggered_by, executed_by)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            run_id,
            transform_id,
            "transform",
            "running",
            jsonlite::toJSON(input_snapshot, auto_unbox = TRUE),
            transform$output_dataset_id,
            full_sql,
            triggered_by,
            null_to_na(executed_by)
        )
    )

    # Execute
    start_time <- Sys.time()
    tryCatch({
        DBI::dbExecute(con, full_sql)

        # Get output row count and hash
        row_count <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) as n FROM {output_table}"))$n
        output_hash <- digest::digest(paste(row_count, transform_id, Sys.time()))

        # Update output dataset
        DBI::dbExecute(
            con,
            "UPDATE ont_datasets SET row_count = ?, updated_at = CURRENT_TIMESTAMP WHERE dataset_id = ?",
            params = list(row_count, transform$output_dataset_id)
        )

        # Update run record (success)
        DBI::dbExecute(
            con,
            "UPDATE ont_runs SET status = ?, ended_at = CURRENT_TIMESTAMP,
             output_row_count = ?, output_hash = ? WHERE run_id = ?",
            params = list("completed", row_count, output_hash, run_id)
        )

        # Create lineage edges from each input to output
        for (i in seq_len(nrow(inputs))) {
            edge_id <- paste0("EDGE-", substr(uuid::UUIDgenerate(), 1, 12))
            DBI::dbExecute(
                con,
                "INSERT INTO ont_lineage_edges (edge_id, run_id, from_dataset_id, to_dataset_id, edge_type, details_json)
                 VALUES (?, ?, ?, ?, ?, ?)",
                params = list(
                    edge_id,
                    run_id,
                    inputs$input_dataset_id[i],
                    transform$output_dataset_id,
                    inputs$input_role[i],
                    jsonlite::toJSON(list(
                        transform_id = transform_id,
                        transform_name = transform$transform_name
                    ), auto_unbox = TRUE)
                )
            )
        }

        end_time <- Sys.time()
        duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

        cli::cli_alert_success("Executed transform {.val {transform_id}}")
        cli::cli_alert_info("{row_count} rows in {round(duration, 2)} seconds")

        list(
            run_id = run_id,
            transform_id = transform_id,
            output_dataset_id = transform$output_dataset_id,
            output_table = output_table,
            row_count = row_count,
            status = "completed",
            duration_secs = duration
        )

    }, error = function(e) {
        DBI::dbExecute(
            con,
            "UPDATE ont_runs SET status = ?, ended_at = CURRENT_TIMESTAMP, log = ? WHERE run_id = ?",
            params = list("failed", e$message, run_id)
        )

        cli::cli_abort("Transform execution failed: {e$message}")
    })
}

# =============================================================================
# Impact Analysis Functions
# =============================================================================

#' Get Downstream Impact
#'
#' Returns all datasets and transforms that would be affected if a dataset changes.
#' This is critical for change impact analysis.
#'
#' @param dataset_id The dataset to analyze.
#' @param depth Maximum traversal depth.
#' @param con Optional DBI connection.
#'
#' @return A list with affected datasets and transforms.
#'
#' @export
ont_get_impact <- function(dataset_id, depth = 10, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get downstream datasets
    downstream <- ont_get_downstream(dataset_id, depth, con)

    # Get transforms that use this dataset as input
    direct_transforms <- DBI::dbGetQuery(
        con,
        "SELECT t.*, ti.input_role
         FROM ont_transforms t
         JOIN ont_transform_inputs ti ON t.transform_id = ti.transform_id
         WHERE ti.input_dataset_id = ?",
        params = list(dataset_id)
    )

    # Get all affected transforms (through downstream datasets)
    all_affected_datasets <- unique(c(dataset_id, downstream$to_dataset_id))

    affected_transforms <- DBI::dbGetQuery(
        con,
        glue::glue("
            SELECT DISTINCT t.*, ti.input_dataset_id
            FROM ont_transforms t
            JOIN ont_transform_inputs ti ON t.transform_id = ti.transform_id
            WHERE ti.input_dataset_id IN ({paste(rep('?', length(all_affected_datasets)), collapse = ', ')})
        "),
        params = as.list(all_affected_datasets)
    )

    list(
        source_dataset = dataset_id,
        downstream_datasets = downstream,
        direct_transforms = tibble::as_tibble(direct_transforms),
        all_affected_transforms = tibble::as_tibble(affected_transforms),
        total_affected_datasets = length(all_affected_datasets) - 1,  # Exclude source
        total_affected_transforms = nrow(affected_transforms)
    )
}

#' Get Transform DAG
#'
#' Returns the directed acyclic graph of transforms for visualization.
#'
#' @param con Optional DBI connection.
#'
#' @return A list with nodes (transforms/datasets) and edges.
#'
#' @export
ont_get_transform_dag <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    # Get all transforms
    transforms <- DBI::dbGetQuery(con, "SELECT * FROM ont_transforms")

    # Get all transform inputs
    inputs <- DBI::dbGetQuery(
        con,
        "SELECT ti.*, d.dataset_name as input_name
         FROM ont_transform_inputs ti
         JOIN ont_datasets d ON ti.input_dataset_id = d.dataset_id"
    )

    # Get all datasets
    datasets <- ont_list_datasets(con = con)

    # Build nodes
    transform_nodes <- tibble::tibble(
        node_id = transforms$transform_id,
        node_type = "transform",
        label = transforms$transform_name
    )

    dataset_nodes <- tibble::tibble(
        node_id = datasets$dataset_id,
        node_type = datasets$dataset_type,
        label = datasets$dataset_name
    )

    nodes <- rbind(transform_nodes, dataset_nodes)

    # Build edges: input -> transform and transform -> output
    edges <- tibble::tibble(
        from = character(),
        to = character(),
        edge_type = character()
    )

    if (nrow(inputs) > 0) {
        input_edges <- tibble::tibble(
            from = inputs$input_dataset_id,
            to = inputs$transform_id,
            edge_type = inputs$input_role
        )
        edges <- rbind(edges, input_edges)
    }

    if (nrow(transforms) > 0) {
        output_edges <- tibble::tibble(
            from = transforms$transform_id,
            to = transforms$output_dataset_id,
            edge_type = "output"
        )
        edges <- rbind(edges, output_edges)
    }

    list(
        nodes = nodes,
        edges = edges
    )
}

#' Validate Transform DAG
#'
#' Checks for cycles in the transform graph (which would indicate invalid definitions).
#'
#' @param con Optional DBI connection.
#'
#' @return A list with is_valid (boolean) and any cycles found.
#'
#' @export
ont_validate_dag <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    dag <- ont_get_transform_dag(con)

    if (nrow(dag$edges) == 0) {
        return(list(is_valid = TRUE, cycles = list()))
    }

    # Build adjacency list
    adj <- list()
    for (node in dag$nodes$node_id) {
        adj[[node]] <- character()
    }

    for (i in seq_len(nrow(dag$edges))) {
        from <- dag$edges$from[i]
        to <- dag$edges$to[i]
        adj[[from]] <- c(adj[[from]], to)
    }

    # DFS-based cycle detection
    visited <- rep(FALSE, length(adj))
    names(visited) <- names(adj)
    rec_stack <- rep(FALSE, length(adj))
    names(rec_stack) <- names(adj)
    cycles <- list()

    dfs_cycle <- function(node, path) {
        visited[node] <<- TRUE
        rec_stack[node] <<- TRUE
        path <- c(path, node)

        for (neighbor in adj[[node]]) {
            if (!visited[neighbor]) {
                dfs_cycle(neighbor, path)
            } else if (rec_stack[neighbor]) {
                # Found cycle
                cycle_start <- which(path == neighbor)
                cycles <<- c(cycles, list(path[cycle_start:length(path)]))
            }
        }

        rec_stack[node] <<- FALSE
    }

    for (node in names(adj)) {
        if (!visited[node]) {
            dfs_cycle(node, character())
        }
    }

    list(
        is_valid = length(cycles) == 0,
        cycles = cycles
    )
}

#' Get Run History for Dataset
#'
#' Returns all runs that produced or used a dataset.
#'
#' @param dataset_id The dataset identifier.
#' @param role "producer" for runs that created this dataset, "consumer" for runs that used it.
#' @param limit Maximum runs to return.
#' @param con Optional DBI connection.
#'
#' @return A tibble of runs.
#'
#' @export
ont_get_dataset_runs <- function(dataset_id,
                                   role = c("all", "producer", "consumer"),
                                   limit = 100,
                                   con = NULL) {
    con <- con %||% ont_get_connection()
    role <- match.arg(role)

    if (role == "producer") {
        query <- "SELECT * FROM ont_runs WHERE output_dataset_id = ? ORDER BY started_at DESC LIMIT ?"
        params <- list(dataset_id, limit)
    } else if (role == "consumer") {
        query <- "
            SELECT DISTINCT r.*
            FROM ont_runs r
            JOIN ont_lineage_edges le ON r.run_id = le.run_id
            WHERE le.from_dataset_id = ?
            ORDER BY r.started_at DESC
            LIMIT ?"
        params <- list(dataset_id, limit)
    } else {
        # All runs related to this dataset
        query <- "
            SELECT DISTINCT r.*,
                   CASE WHEN r.output_dataset_id = ? THEN 'producer' ELSE 'consumer' END as role
            FROM ont_runs r
            LEFT JOIN ont_lineage_edges le ON r.run_id = le.run_id
            WHERE r.output_dataset_id = ? OR le.from_dataset_id = ?
            ORDER BY r.started_at DESC
            LIMIT ?"
        params <- list(dataset_id, dataset_id, dataset_id, limit)
    }

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Compare Runs
#'
#' Compares two runs, showing differences in inputs, outputs, and execution.
#'
#' @param run_id_1 First run ID.
#' @param run_id_2 Second run ID.
#' @param con Optional DBI connection.
#'
#' @return A list with comparison details.
#'
#' @export
ont_compare_runs <- function(run_id_1, run_id_2, con = NULL) {
    con <- con %||% ont_get_connection()

    run1 <- ont_get_run(run_id_1, con)
    run2 <- ont_get_run(run_id_2, con)

    if (is.null(run1)) cli::cli_abort("Run {.val {run_id_1}} not found.")
    if (is.null(run2)) cli::cli_abort("Run {.val {run_id_2}} not found.")

    # Parse input snapshots
    input1 <- if (!is.na(run1$input_snapshot)) jsonlite::fromJSON(run1$input_snapshot) else list()
    input2 <- if (!is.na(run2$input_snapshot)) jsonlite::fromJSON(run2$input_snapshot) else list()

    # Compare inputs
    all_inputs <- unique(c(names(input1), names(input2)))
    input_changes <- lapply(all_inputs, function(ds) {
        in1 <- input1[[ds]]
        in2 <- input2[[ds]]
        list(
            dataset_id = ds,
            in_run_1 = !is.null(in1),
            in_run_2 = !is.null(in2),
            rows_1 = if (!is.null(in1)) in1$row_count else NA,
            rows_2 = if (!is.null(in2)) in2$row_count else NA
        )
    })

    list(
        run_1 = run1,
        run_2 = run2,
        same_transform = identical(run1$transform_id, run2$transform_id),
        output_row_diff = (run2$output_row_count %||% 0) - (run1$output_row_count %||% 0),
        duration_diff = as.numeric(
            difftime(
                as.POSIXct(run2$ended_at) - as.POSIXct(run2$started_at),
                as.POSIXct(run1$ended_at) - as.POSIXct(run1$started_at),
                units = "secs"
            )
        ),
        input_changes = input_changes
    )
}
