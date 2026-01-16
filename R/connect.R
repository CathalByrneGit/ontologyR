#' @title Connection Management
#' @description Functions for connecting to and managing ontology databases.
#' @name connection
NULL

# Internal environment to store active connection
.ont_env <- new.env(parent = emptyenv())

#' Connect to an Ontology Database
#'
#' Creates or connects to an ontology database. The database stores all
#' ontology metadata: object types, link types, concepts, versions, audits,
#' and governance logs.
#'
#' @param path Path to DuckDB database file. Use `":memory:"` for an
#'   in-memory database (useful for testing). Default creates a file
#'   called `"ontology.duckdb"` in the current working directory.
#' @param read_only Logical. If `TRUE`, connect in read-only mode.
#' @param init Logical. If `TRUE` (default), initialize schema if tables
#'   don't exist.
#'
#' @return Invisibly returns the connection object. The connection is also
#'   stored internally for use by other `ont_*` functions.
#'
#' @examples
#' # In-memory database for testing
#' ont_connect(":memory:")
#'
#' # Persistent database
#' ont_connect("my_ontology.duckdb")
#'
#' # Check connection status
#' ont_status()
#'
#' # Disconnect when done
#' ont_disconnect()
#'
#' @export
ont_connect <- function(path = "ontology.duckdb", read_only = FALSE, init = TRUE) {
    # Check for existing connection
    if (!is.null(.ont_env$con)) {
        cli::cli_warn("Existing connection found. Disconnecting first.")
        ont_disconnect()
    }

    # Connect to DuckDB
    con <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = path,
        read_only = read_only
    )

    # Store connection info
    .ont_env$con <- con
    .ont_env$path <- path
    .ont_env$connected_at <- Sys.time()
    .ont_env$read_only <- read_only

    # Initialize schema if requested

    if (init && !read_only) {
        ont_init_schema(con)
    }

    cli::cli_alert_success("Connected to ontology database: {.path {path}}")
    invisible(con)
}

#' Disconnect from Ontology Database
#'
#' Closes the connection to the ontology database and cleans up resources.
#'
#' @param shutdown Logical. If `TRUE` (default), fully shut down the DuckDB
#'   instance. Set to `FALSE` if you want to reconnect quickly.
#'
#' @return Invisibly returns `TRUE` if disconnection was successful.
#'
#' @export
ont_disconnect <- function(shutdown = TRUE) {
    if (is.null(.ont_env$con)) {
        cli::cli_warn("No active connection to disconnect.")
        return(invisible(FALSE))
    }

    DBI::dbDisconnect(.ont_env$con, shutdown = shutdown)

    .ont_env$con <- NULL
    .ont_env$path <- NULL
    .ont_env$connected_at <- NULL
    .ont_env$read_only <- NULL

    cli::cli_alert_success("Disconnected from ontology database.")
    invisible(TRUE)
}

#' Get Connection Status
#'
#' Returns information about the current ontology database connection.
#'
#' @return A list with connection details, or `NULL` if not connected.
#'
#' @export
ont_status <- function() {
    if (is.null(.ont_env$con)) {
        cli::cli_alert_warning("Not connected to any ontology database.")
        return(NULL)
    }

    # Get table counts
    counts <- list(
        object_types = get_table_count("ont_object_types"),
        link_types = get_table_count("ont_link_types"),
        concepts = get_table_count("ont_concepts"),
        concept_versions = get_table_count("ont_concept_versions"),
        audits = get_table_count("ont_audits"),
        drift_events = get_table_count("ont_drift_events")
    )

    status <- list(
        connected = TRUE,
        path = .ont_env$path,
        connected_at = .ont_env$connected_at,
        read_only = .ont_env$read_only,
        counts = counts
    )

    cli::cli_h2("Ontology Database Status")
    cli::cli_alert_info("Path: {.path {status$path}}")
    cli::cli_alert_info("Connected: {.timestamp {status$connected_at}}")
    cli::cli_alert_info("Read-only: {status$read_only}")
    cli::cli_h3("Contents")
    cli::cli_ul(c(
        "Object types: {counts$object_types}",
        "Link types: {counts$link_types}",
        "Concepts: {counts$concepts}",
        "Concept versions: {counts$concept_versions}",
        "Audits: {counts$audits}",
        "Drift events: {counts$drift_events}"
    ))

    invisible(status)
}

#' Get Active Connection
#'
#' Returns the active database connection. Throws an error if not connected.
#'
#' @return A DBI connection object.
#'
#' @keywords internal
#' @export
ont_get_connection <- function() {
    if (is.null(.ont_env$con)) {
        cli::cli_abort("Not connected. Use {.fn ont_connect} first.")
    }
    .ont_env$con
}

#' Initialize Ontology Schema
#'
#' Creates the ontology tables if they don't exist. Called automatically
#' by `ont_connect()` unless `init = FALSE`.
#'
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @keywords internal
#' @export
ont_init_schema <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    # Read and execute schema SQL
    schema_path <- system.file("sql", "schema.sql", package = "ontologyR")

    # In development (not installed), system.file returns ""
    # Also fall back to inline if schema parsing fails
    if (schema_path == "" || !file.exists(schema_path)) {
        create_tables_inline(con)
        return(invisible(TRUE))
    }
    
    # Try to parse and execute schema file
    schema_executed <- tryCatch({
        schema_lines <- readLines(schema_path, warn = FALSE)
        
        # Remove single-line comments (-- ...) from each line BEFORE joining
        # This handles comments with semicolons in them
        schema_lines <- vapply(schema_lines, function(line) {
            # Find position of -- that's not inside a string
            # Simple approach: just remove everything after --
            pos <- regexpr("--", line, fixed = TRUE)
            if (pos > 0) {
                substr(line, 1, pos - 1)
            } else {
                line
            }
        }, character(1), USE.NAMES = FALSE)
        
        # Join into single string
        schema_sql <- paste(schema_lines, collapse = "\n")
        
        # Remove multi-line comments (/* ... */)
        schema_sql <- gsub("/\\*[^*]*\\*+([^/*][^*]*\\*+)*/", "", schema_sql)

        # Split by semicolon and execute each statement
        statements <- strsplit(schema_sql, ";")[[1]]
        statements <- trimws(statements)
        statements <- statements[nchar(statements) > 0]

        for (stmt in statements) {
            # Only execute if there's actual SQL (not just whitespace)
            clean_stmt <- trimws(stmt)
            if (nchar(clean_stmt) > 0 && !grepl("^\\s*$", clean_stmt)) {
                tryCatch(
                    DBI::dbExecute(con, clean_stmt),
                    error = function(e) {
                        # Ignore "already exists" errors
                        if (!grepl("already exists", e$message, ignore.case = TRUE)) {
                            # Re-throw to trigger fallback
                            stop(e)
                        }
                    }
                )
            }
        }
        TRUE
    }, error = function(e) {
        cli::cli_alert_warning("Schema file parsing failed, using inline creation: {e$message}")
        FALSE
    })
    
    # If schema file failed, fall back to inline
    if (!schema_executed) {
        create_tables_inline(con)
    }

    invisible(TRUE)
}

# Helper: get row count for a table
get_table_count <- function(table_name) {
    con <- ont_get_connection()
    tryCatch(
        DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) as n FROM {table_name}"))$n,
        error = function(e) 0
    )
}

# Helper: create tables inline (for development/testing)
create_tables_inline <- function(con) {
    # Object types
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_object_types (
            object_type TEXT PRIMARY KEY,
            table_name TEXT NOT NULL,
            pk_column TEXT NOT NULL,
            description TEXT,
            owner_domain TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Link types
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_link_types (
            link_type TEXT PRIMARY KEY,
            from_object TEXT NOT NULL,
            to_object TEXT NOT NULL,
            link_table TEXT NOT NULL,
            from_key TEXT NOT NULL,
            to_key TEXT NOT NULL,
            valid_from_col TEXT,
            valid_to_col TEXT,
            cardinality TEXT DEFAULT 'many-to-many',
            description TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Concepts
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_concepts (
            concept_id TEXT PRIMARY KEY,
            object_type TEXT NOT NULL,
            description TEXT,
            owner_domain TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Concept versions
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_concept_versions (
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            sql_expr TEXT NOT NULL,
            status TEXT NOT NULL DEFAULT 'draft',
            rationale TEXT,
            valid_from DATE,
            valid_to DATE,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT,
            approved_at TIMESTAMP,
            approved_by TEXT,
            PRIMARY KEY (concept_id, scope, version)
        )
    ")

    # Audits
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_audits (
            audit_id TEXT PRIMARY KEY,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            object_key TEXT NOT NULL,
            system_value BOOLEAN,
            reviewer_value BOOLEAN,
            reviewer_id TEXT,
            audited_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            notes TEXT
        )
    ")

    # Drift events
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_drift_events (
            drift_id TEXT PRIMARY KEY,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            detected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            detection_type TEXT NOT NULL,
            disagreement_rate REAL,
            window_start TIMESTAMP,
            window_end TIMESTAMP,
            audit_count INTEGER,
            status TEXT DEFAULT 'open',
            resolution TEXT,
            resolved_at TIMESTAMP,
            resolved_by TEXT
        )
    ")

    # Governance log
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_governance_log (
            log_id TEXT PRIMARY KEY,
            action_type TEXT NOT NULL,
            concept_id TEXT NOT NULL,
            scope TEXT,
            version INTEGER,
            actor TEXT NOT NULL,
            action_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            rationale TEXT,
            evidence TEXT,
            blocked_by TEXT
        )
    ")

    # Dashboard registry
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_dashboard_registry (
            dashboard_id TEXT NOT NULL,
            dashboard_name TEXT,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            registered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            registered_by TEXT,
            PRIMARY KEY (dashboard_id, concept_id, scope, version)
        )
    ")

    # Observations
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_observations (
            observation_id TEXT PRIMARY KEY,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            observed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            observation_type TEXT NOT NULL DEFAULT 'snapshot',
            total_objects INTEGER NOT NULL,
            concept_true INTEGER NOT NULL,
            concept_false INTEGER NOT NULL,
            concept_null INTEGER DEFAULT 0,
            prevalence_rate REAL,
            filter_expr TEXT,
            triggered_by TEXT,
            observer_id TEXT,
            notes TEXT
        )
    ")

    # Observation details
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_observation_details (
            observation_id TEXT NOT NULL,
            object_key TEXT NOT NULL,
            concept_value BOOLEAN,
            PRIMARY KEY (observation_id, object_key)
        )
    ")

    # Cohorts
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_cohorts (
            cohort_id TEXT PRIMARY KEY,
            cohort_name TEXT NOT NULL,
            object_type TEXT NOT NULL,
            definition_type TEXT NOT NULL DEFAULT 'sql',
            sql_expr TEXT,
            description TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Cohort members
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_cohort_members (
            cohort_id TEXT NOT NULL,
            object_key TEXT NOT NULL,
            added_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            added_by TEXT,
            PRIMARY KEY (cohort_id, object_key)
        )
    ")

    # Analysis runs
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_analysis_runs (
            analysis_id TEXT PRIMARY KEY,
            analysis_type TEXT NOT NULL,
            concept_id TEXT NOT NULL,
            scope TEXT,
            parameters TEXT,
            results_summary TEXT,
            executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            executed_by TEXT
        )
    ")

    # Datasets (Foundry-inspired)
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_datasets (
            dataset_id TEXT PRIMARY KEY,
            dataset_name TEXT NOT NULL,
            dataset_type TEXT NOT NULL DEFAULT 'source',
            physical_name TEXT NOT NULL,
            object_type TEXT,
            description TEXT,
            schema_json TEXT,
            row_count INTEGER,
            owner TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT,
            updated_at TIMESTAMP,
            source_concept_id TEXT,
            source_scope TEXT,
            source_version INTEGER,
            source_filter TEXT
        )
    ")

    # Transforms
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_transforms (
            transform_id TEXT PRIMARY KEY,
            transform_name TEXT NOT NULL,
            output_dataset_id TEXT NOT NULL,
            transform_type TEXT NOT NULL DEFAULT 'sql',
            code TEXT,
            description TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Transform inputs
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_transform_inputs (
            transform_id TEXT NOT NULL,
            input_dataset_id TEXT NOT NULL,
            input_role TEXT DEFAULT 'primary',
            PRIMARY KEY (transform_id, input_dataset_id)
        )
    ")

    # Runs
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_runs (
            run_id TEXT PRIMARY KEY,
            transform_id TEXT,
            run_type TEXT NOT NULL,
            status TEXT NOT NULL DEFAULT 'running',
            started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            ended_at TIMESTAMP,
            input_snapshot TEXT,
            output_dataset_id TEXT,
            output_row_count INTEGER,
            output_hash TEXT,
            concept_id TEXT,
            scope TEXT,
            version INTEGER,
            sql_executed TEXT,
            filter_expr TEXT,
            triggered_by TEXT,
            executed_by TEXT,
            log TEXT
        )
    ")

    # Lineage edges
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_lineage_edges (
            edge_id TEXT PRIMARY KEY,
            run_id TEXT NOT NULL,
            from_dataset_id TEXT NOT NULL,
            to_dataset_id TEXT NOT NULL,
            edge_type TEXT NOT NULL,
            details_json TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    ")

    # Roles (RBAC)
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_roles (
            role_id TEXT PRIMARY KEY,
            role_name TEXT NOT NULL,
            description TEXT,
            permissions TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    ")

    # Insert default roles
    DBI::dbExecute(con, "
        INSERT OR IGNORE INTO ont_roles (role_id, role_name, description, permissions) VALUES
        ('viewer', 'Viewer', 'Can view concepts and evaluations', '[\"concept:read\", \"dataset:read\", \"audit:read\"]'),
        ('editor', 'Editor', 'Can create and modify draft concepts', '[\"concept:read\", \"concept:write\", \"concept:evaluate\", \"dataset:read\", \"dataset:write\", \"audit:read\", \"audit:write\"]'),
        ('approver', 'Approver', 'Can approve and activate concepts', '[\"concept:read\", \"concept:write\", \"concept:evaluate\", \"concept:approve\", \"concept:activate\", \"dataset:read\", \"dataset:write\", \"audit:read\", \"audit:write\", \"gate:override\"]'),
        ('admin', 'Admin', 'Full access to all operations', '[\"*\"]')
    ")

    # User roles
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_user_roles (
            user_id TEXT NOT NULL,
            role_id TEXT NOT NULL,
            scope_type TEXT DEFAULT 'global',
            scope_value TEXT,
            granted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            granted_by TEXT,
            expires_at TIMESTAMP,
            PRIMARY KEY (user_id, role_id, scope_type, scope_value)
        )
    ")

    # Governance gates
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_governance_gates (
            gate_id TEXT PRIMARY KEY,
            gate_name TEXT NOT NULL,
            gate_type TEXT NOT NULL,
            applies_to TEXT NOT NULL,
            condition_json TEXT NOT NULL,
            severity TEXT DEFAULT 'blocking',
            enabled BOOLEAN DEFAULT TRUE,
            scope_filter TEXT,
            domain_filter TEXT,
            description TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Insert default gates
    DBI::dbExecute(con, "
        INSERT OR IGNORE INTO ont_governance_gates (gate_id, gate_name, gate_type, applies_to, condition_json, severity, description) VALUES
        ('gate_audit_coverage', 'Minimum Audit Coverage', 'audit_coverage', 'activation',
         '{\"min_audits\": 10, \"min_agreement_rate\": 0.9}', 'blocking',
         'Requires minimum audit coverage and agreement rate before activation'),
        ('gate_no_open_drift', 'No Open Drift Events', 'drift_threshold', 'activation',
         '{\"max_open_drift_events\": 0}', 'blocking',
         'Cannot activate if there are open drift events'),
        ('gate_approval_required', 'Approval Required', 'approval_required', 'activation',
         '{\"min_approvals\": 1, \"approver_roles\": [\"approver\", \"admin\"]}', 'blocking',
         'Requires at least one approval from authorized role')
    ")

    # Gate checks
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_gate_checks (
            check_id TEXT PRIMARY KEY,
            gate_id TEXT NOT NULL,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            action_type TEXT NOT NULL,
            check_result TEXT NOT NULL,
            check_details TEXT,
            checked_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            checked_by TEXT,
            override_reason TEXT
        )
    ")

    # Approval requests
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_approval_requests (
            request_id TEXT PRIMARY KEY,
            concept_id TEXT NOT NULL,
            scope TEXT NOT NULL,
            version INTEGER NOT NULL,
            requested_action TEXT NOT NULL,
            status TEXT DEFAULT 'pending',
            requested_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            requested_by TEXT,
            decided_at TIMESTAMP,
            decided_by TEXT,
            decision_notes TEXT
        )
    ")

    # Templates (for concept inheritance)
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_templates (
            template_id TEXT PRIMARY KEY,
            template_name TEXT NOT NULL,
            object_type TEXT NOT NULL,
            base_sql_expr TEXT NOT NULL,
            parameters TEXT,
            description TEXT,
            source_standard TEXT,
            owner_domain TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            created_by TEXT
        )
    ")

    # Template inheritance
    DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS ont_template_inheritance (
            concept_id TEXT NOT NULL,
            template_id TEXT NOT NULL,
            parameter_values TEXT,
            inheritance_type TEXT DEFAULT 'extends',
            deviation_notes TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (concept_id, template_id)
        )
    ")
}
