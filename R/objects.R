#' @title Object and Link Type Registration
#' @description Functions for registering object types (mappings from logical
#'   concepts to physical tables) and link types (relationships between objects).
#' @name objects
NULL

#' Register an Object Type
#'
#' Maps a logical object type name (e.g., "Encounter", "Patient") to its
#' physical storage: the table name and primary key column. This mapping
#' allows concepts to be defined against logical types without hardcoding
#' table names.
#'
#' @param object_type Character. The logical name for this object type.
#' @param table_name Character. The physical table name in the database.
#' @param pk_column Character. The primary key column name.
#' @param description Character. Optional description of this object type.
#' @param owner_domain Character. Optional domain/team that owns this type.
#' @param created_by Character. Optional identifier for who created this.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#'
#' # Register an Encounter object type
#' ont_register_object(
#'     object_type = "Encounter",
#'     table_name = "encounters",
#'     pk_column = "encounter_id",
#'     description = "A patient encounter or visit",
#'     owner_domain = "patient_flow"
#' )
#'
#' # List registered types
#' ont_list_objects()
#'
#' ont_disconnect()
#' }
#'
#' @export
ont_register_object <- function(object_type,
                                 table_name,
                                 pk_column,
                                 description = NULL,
                                 owner_domain = NULL,
                                 created_by = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    # Check if object type already exists
    existing <- DBI::dbGetQuery(
        con,
        "SELECT object_type FROM ont_object_types WHERE object_type = ?",
        params = list(object_type)
    )

    if (nrow(existing) > 0) {
        cli::cli_alert_warning(
            "Object type {.val {object_type}} already exists. Updating."
        )
    }

    DBI::dbExecute(con, "
        INSERT OR REPLACE INTO ont_object_types
        (object_type, table_name, pk_column, description, owner_domain, created_by, created_at)
        VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
        object_type, table_name, pk_column,
        null_to_na(description), null_to_na(owner_domain), null_to_na(created_by)
    ))

    cli::cli_alert_success(
        "Registered object type {.val {object_type}} -> {.val {table_name}}"
    )
    invisible(TRUE)
}

#' List Registered Object Types
#'
#' Returns a tibble of all registered object types with their metadata.
#'
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with columns: object_type, table_name, pk_column,
#'   description, owner_domain, created_at, created_by.
#'
#' @export
ont_list_objects <- function(con = NULL) {
    con <- con %||% ont_get_connection()
    dplyr::tbl(con, "ont_object_types") |>
        dplyr::collect()
}

#' Get Object Type Metadata
#'
#' Retrieves the metadata for a specific object type.
#'
#' @param object_type Character. The object type to look up.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A single-row tibble with object type metadata.
#'
#' @keywords internal
#' @export
ont_get_object <- function(object_type, con = NULL) {
    con <- con %||% ont_get_connection()

    meta <- dplyr::tbl(con, "ont_object_types") |>
        dplyr::filter(.data$object_type == !!object_type) |>
        dplyr::collect()

    if (nrow(meta) != 1) {
        cli::cli_abort("Unknown object type: {.val {object_type}}")
    }

    meta
}

#' Register a Link Type
#'
#' Defines a relationship between two object types. Links are stored in
#' a separate table and can optionally have temporal validity columns.
#'
#' @param link_type Character. Name for this link type.
#' @param from_object Character. Source object type.
#' @param to_object Character. Target object type.
#' @param link_table Character. Physical table containing the link records.
#' @param from_key Character. Column in link_table referencing from_object.
#' @param to_key Character. Column in link_table referencing to_object.
#' @param valid_from_col Character. Optional column for temporal validity start.
#' @param valid_to_col Character. Optional column for temporal validity end.
#' @param cardinality Character. Relationship cardinality: "one-to-one",
#'   "one-to-many", or "many-to-many" (default).
#' @param description Character. Optional description.
#' @param created_by Character. Optional creator identifier.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#'
#' # Register object types first
#' ont_register_object("Encounter", "encounters", "encounter_id")
#' ont_register_object("Patient", "patients", "patient_id")
#'
#' # Register link between them
#' ont_register_link(
#'     link_type = "encounter_patient",
#'     from_object = "Encounter",
#'     to_object = "Patient",
#'     link_table = "encounters",  # Link lives in encounters table
#'     from_key = "encounter_id",
#'     to_key = "patient_id",
#'     cardinality = "many-to-one"
#' )
#'
#' ont_disconnect()
#' }
#'
#' @export
ont_register_link <- function(link_type,
                               from_object,
                               to_object,
                               link_table,
                               from_key,
                               to_key,
                               valid_from_col = NULL,
                               valid_to_col = NULL,
                               cardinality = "many-to-many",
                               description = NULL,
                               created_by = NULL,
                               con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object types exist
    ont_get_object(from_object, con)
    ont_get_object(to_object, con)

    # Validate cardinality
    valid_cardinalities <- c("one-to-one", "one-to-many", "many-to-one", "many-to-many")
    if (!cardinality %in% valid_cardinalities) {
        cli::cli_abort(
            "Invalid cardinality {.val {cardinality}}. Must be one of: {.val {valid_cardinalities}}"
        )
    }

    DBI::dbExecute(con, "
        INSERT OR REPLACE INTO ont_link_types
        (link_type, from_object, to_object, link_table, from_key, to_key,
         valid_from_col, valid_to_col, cardinality, description, created_by, created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    ", params = list(
        link_type, from_object, to_object, link_table, from_key, to_key,
        null_to_na(valid_from_col), null_to_na(valid_to_col), cardinality,
        null_to_na(description), null_to_na(created_by)
    ))

    cli::cli_alert_success(
        "Registered link type {.val {link_type}}: {from_object} -> {to_object}"
    )
    invisible(TRUE)
}

#' List Registered Link Types
#'
#' Returns a tibble of all registered link types with their metadata.
#'
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble with link type metadata.
#'
#' @export
ont_list_links <- function(con = NULL) {
    con <- con %||% ont_get_connection()
    dplyr::tbl(con, "ont_link_types") |>
        dplyr::collect()
}

#' Get Link Type Metadata
#'
#' Retrieves the metadata for a specific link type.
#'
#' @param link_type Character. The link type to look up.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A single-row tibble with link type metadata.
#'
#' @keywords internal
#' @export
ont_get_link <- function(link_type, con = NULL) {
    con <- con %||% ont_get_connection()

    meta <- dplyr::tbl(con, "ont_link_types") |>
        dplyr::filter(.data$link_type == !!link_type) |>
        dplyr::collect()

    if (nrow(meta) != 1) {
        cli::cli_abort("Unknown link type: {.val {link_type}}")
    }

    meta
}

#' Query Objects by Primary Key
#'
#' Retrieves objects of a given type by their primary key values.
#'
#' @param object_type Character. The object type to query.
#' @param ids Vector of primary key values to filter by. If `NULL`, returns all.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A lazy dplyr tbl. Call `collect()` to retrieve data.
#'
#' @export
ont_query_objects <- function(object_type, ids = NULL, con = NULL) {
    con <- con %||% ont_get_connection()
    meta <- ont_get_object(object_type, con)

    tbl_ref <- dplyr::tbl(con, meta$table_name)

    if (!is.null(ids)) {
        pk <- meta$pk_column
        tbl_ref <- tbl_ref |>
            dplyr::filter(.data[[pk]] %in% ids)
    }

    tbl_ref
}

#' Query Linked Objects
#'
#' Starting from objects of one type, traverse a link to get related objects.
#'
#' @param from_type Character. Source object type.
#' @param from_ids Vector of source object primary keys.
#' @param link_type Character. The link type to traverse.
#' @param as_of Date or timestamp. For temporal links, return links valid at
#'   this point in time. If `NULL`, returns all links.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A lazy dplyr tbl with joined target objects.
#'
#' @export
ont_query_linked <- function(from_type, from_ids, link_type, as_of = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    link <- ont_get_link(link_type, con)

    if (link$from_object != from_type) {
        cli::cli_abort(
            "Link type {.val {link_type}} expects from_object = {.val {link$from_object}}, got {.val {from_type}}"
        )
    }

    to_meta <- ont_get_object(link$to_object, con)

    # Get edges from link table
    edges <- dplyr::tbl(con, link$link_table) |>
        dplyr::filter(.data[[link$from_key]] %in% from_ids)

    # Apply temporal filter if specified and link has temporal columns
    if (!is.null(as_of) && !is.na(link$valid_from_col)) {
        edges <- edges |>
            dplyr::filter(
                is.na(.data[[link$valid_from_col]]) | .data[[link$valid_from_col]] <= !!as_of,
                is.na(.data[[link$valid_to_col]]) | .data[[link$valid_to_col]] >= !!as_of
            )
    }

    # Join to target objects
    edges |>
        dplyr::inner_join(
            dplyr::tbl(con, to_meta$table_name),
            by = stats::setNames(to_meta$pk_column, link$to_key)
        )
}
