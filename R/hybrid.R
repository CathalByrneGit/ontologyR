#' @title Hybrid API Layer
#' @description Provides ontologyIndex-style ergonomics (named list access, $ operator)
#'   on top of the table-based storage. This gives you the best of both worlds:
#'   - Database as source of truth (versioning, audit trails, governance)
#'   - R-native feel with $ accessors and cached lookups
#' @name hybrid
NULL

# =============================================================================
# Core S3 Classes
# =============================================================================

#' Create an Ontology Object
#'
#' Returns an `ontology` S3 object that provides convenient access to concepts,
#' versions, and evaluations while keeping the database as source of truth.
#'
#' This is the recommended way to work with ontologyR interactively.
#'
#' @param path Path to DuckDB database, or ":memory:" for in-memory.
#' @param read_only Logical. Connect in read-only mode?
#'
#' @return An object of class `ontology`.
#'
#' @examples
#' \dontrun{
#' ont <- ontology(":memory:")
#'
#' # Register and define concepts
#' ont$register_object("Encounter", "encounters", "encounter_id")
#' ont$define_concept("ready_for_discharge", "Encounter")
#' ont$add_version("ready_for_discharge", "flow", 1, "NOT planned_intervention")
#'
#' # Access like ontologyIndex
#' ont$concepts
#' ont$concepts$ready_for_discharge
#' ont$concepts$ready_for_discharge$versions
#'
#' # Evaluate
#' ont$evaluate("ready_for_discharge", "flow")
#'
#' # Audit
#' ont$sample("ready_for_discharge", "flow", n = 20)
#' ont$record_audit("ready_for_discharge", "flow", 1, "E1", TRUE, FALSE, "reviewer1")
#'
#' # Check drift
#' ont$drift_status()
#' }
#'
#' @export
ontology <- function(path = "ontology.duckdb", read_only = FALSE) {

    # Connect to database
    con <- ont_connect(path, read_only = read_only, init = !read_only)

    # Build the ontology object
    ont <- structure(
        list(
            .con = con,
            .path = path,
            .cache = new.env(parent = emptyenv())
        ),
        class = "ontology"
    )

    # Initialize cache
    ont$.cache$concepts <- NULL
    ont$.cache$objects <- NULL
    ont$.cache$last_refresh <- Sys.time()

    ont
}

#' @export
print.ontology <- function(x, ...) {
    status <- ont_status()
    cat("
")
    cat("<ontology>
")
    cat("
")
    cat("Path:
", x$.path, "
")
    if (!is.null(status)) {
        cat("
")
        cat("Contents:
")
        cat("
  Object types:", status$counts$object_types, "
")
        cat("  Concepts:    ", status$counts$concepts, "
")
        cat("  Versions:    ", status$counts$concept_versions, "
")
        cat("  Audits:      ", status$counts$audits, "
")
        cat("  Drift events:", status$counts$drift_events, "
")
    }
    cat("
")
    cat("Use ont$concepts, ont$objects, ont$drift_status() to explore
")
    invisible(x)
}

#' @export
`$.ontology` <- function(x, name) {
    # Direct slot access

    if (name %in% c(".con", ".path", ".cache")) {
        return(x[[name]])
    }

    # Special accessors
    switch(name,
        # --- Data accessors (return ontology_* objects) ---
        "concepts" = get_concepts_accessor(x),
        "objects" = get_objects_accessor(x),
        "links" = get_links_accessor(x),

        # --- Action methods (return functions) ---
        "register_object" = function(object_type, table_name, pk_column, ...) {
            ont_register_object(object_type, table_name, pk_column, ..., con = x$.con)
            invalidate_cache(x, "objects")
            invisible(x)
        },
        "register_link" = function(link_type, from_object, to_object, link_table, from_key, to_key, ...) {
            ont_register_link(link_type, from_object, to_object, link_table, from_key, to_key, ..., con = x$.con)
            invalidate_cache(x, "links")
            invisible(x)
        },
        "define_concept" = function(concept_id, object_type, ...) {
            ont_define_concept(concept_id, object_type, ..., con = x$.con)
            invalidate_cache(x, "concepts")
            invisible(x)
        },
        "add_version" = function(concept_id, scope, version, sql_expr, ...) {
            ont_add_version(concept_id, scope, version, sql_expr, ..., con = x$.con)
            invalidate_cache(x, "concepts")
            invisible(x)
        },
        "activate" = function(concept_id, scope, version, approved_by, ...) {
            ont_activate_version(concept_id, scope, version, approved_by, ..., con = x$.con)
            invalidate_cache(x, "concepts")
            invisible(x)
        },
        "deprecate" = function(concept_id, scope, version, deprecated_by, ...) {
            ont_deprecate_version(concept_id, scope, version, deprecated_by, ..., con = x$.con)
            invalidate_cache(x, "concepts")
            invisible(x)
        },

        # --- Query methods ---
        "evaluate" = function(concept_id, scope, version = NULL, ...) {
            ont_evaluate(concept_id, scope, version, ..., con = x$.con)
        },
        "compare" = function(concept_id, scope, version_a, version_b, ...) {
            ont_compare_versions(concept_id, scope, version_a, version_b, ..., con = x$.con)
        },

        # --- Audit methods ---
        "sample" = function(concept_id, scope, n = 20, ...) {
            ont_sample_for_audit(concept_id, scope, n = n, ..., con = x$.con)
        },
        "record_audit" = function(concept_id, scope, version, object_key, system_value, reviewer_value, reviewer_id, ...) {
            ont_record_audit(concept_id, scope, version, object_key, system_value, reviewer_value, reviewer_id, ..., con = x$.con)
        },
        "audit_summary" = function(concept_id, scope, version, ...) {
            ont_audit_summary(concept_id, scope, version, ..., con = x$.con)
        },

        # --- Drift methods ---
        "check_drift" = function(concept_id, scope, version = NULL, ...) {
            ont_check_drift(concept_id, scope, version, ..., con = x$.con)
        },
        "detect_drift" = function(concept_id, scope, ...) {
            ont_detect_drift(concept_id, scope, ..., con = x$.con)
        },
        "drift_status" = function(...) {
            ont_drift_status(..., con = x$.con)
        },
        "resolve_drift" = function(drift_id, resolution, resolved_by, ...) {
            ont_resolve_drift(drift_id, resolution, resolved_by, ..., con = x$.con)
        },

        # --- Governance methods ---
        "governance_report" = function() {
            ont_governance_report(con = x$.con)
        },
        "check_policy" = function(action, concept_id, scope, version) {
            ont_check_policy(action, concept_id, scope, version, con = x$.con)
        },

        # --- Utility methods ---
        "sql" = function(sql, params = NULL) {
            ont_sql(sql, params, con = x$.con)
        },
        "refresh" = function() {
            invalidate_cache(x, "all")
            invisible(x)
        },
        "disconnect" = function() {
            ont_disconnect()
            invisible(NULL)
        },

        # Default
        NULL
    )
}

# =============================================================================
# Concepts Accessor (the heart of the ergonomic layer)
# =============================================================================

#' @keywords internal
get_concepts_accessor <- function(ont) {
    # Return cached if fresh
    if (!is.null(ont$.cache$concepts) &&
        difftime(Sys.time(), ont$.cache$last_refresh, units = "secs") < 60) {
        return(ont$.cache$concepts)
    }

    # Fetch from database
    concepts_df <- ont_list_concepts(con = ont$.con)

    if (nrow(concepts_df) == 0) {
        return(structure(list(), class = "ontology_concepts"))
    }

    # Build named list of concept objects
    concepts_list <- lapply(concepts_df$concept_id, function(cid) {
        make_concept_object(ont, cid)
    })
    names(concepts_list) <- concepts_df$concept_id

    # Wrap in S3 class
    result <- structure(
        concepts_list,
        class = "ontology_concepts",
        .ont = ont
    )

    ont$.cache$concepts <- result
    ont$.cache$last_refresh <- Sys.time()

    result
}

#' @export
print.ontology_concepts <- function(x, ...) {
    cat("<ontology_concepts>
")
    cat("
")
    if (length(x) == 0) {
        cat("No concepts defined yet.
")
        cat("Use ont$define_concept() to create one.
")
    } else {
        cat(length(x), "concept(s):
")
        for (nm in names(x)) {
            cat("
  $", nm, "
", sep = "")
        }
        cat("
")
        cat("
Access with: ont$concepts$<name>
")
    }
    invisible(x)
}

#' @export
`$.ontology_concepts` <- function(x, name) {
    x[[name]]
}

#' @export
`[[.ontology_concepts` <- function(x, name) {
    if (name %in% names(x)) {
        # Return the concept object (already built)
        NextMethod()
    } else {
        cli::cli_warn("Unknown concept: {.val {name}}")
        NULL
    }
}

# =============================================================================
# Individual Concept Object
# =============================================================================

#' @keywords internal
make_concept_object <- function(ont, concept_id) {
    # Fetch concept metadata
    concept_meta <- ont_get_concept(concept_id, con = ont$.con)

    # Fetch versions
    versions_df <- ont_list_versions(concept_id, con = ont$.con)

    # Build scopes structure
    scopes <- list()
    if (nrow(versions_df) > 0) {
        for (s in unique(versions_df$scope)) {
            scope_versions <- versions_df[versions_df$scope == s, ]
            scope_versions <- scope_versions[order(-scope_versions$version), ]

            # Make each version accessible by number
            version_list <- lapply(seq_len(nrow(scope_versions)), function(i) {
                make_version_object(ont, scope_versions[i, ])
            })
            names(version_list) <- as.character(scope_versions$version)

            scopes[[s]] <- structure(
                version_list,
                class = "ontology_scope",
                concept_id = concept_id,
                scope = s,
                .ont = ont
            )
        }
    }

    structure(
        c(
            list(
                id = concept_id,
                object_type = concept_meta$object_type,
                description = concept_meta$description,
                owner = concept_meta$owner_domain
            ),
            scopes
        ),
        class = "ontology_concept",
        .ont = ont
    )
}

#' @export
print.ontology_concept <- function(x, ...) {
    cat("<ontology_concept>
")
    cat("
")
    cat("ID:         ", x$id, "
")
    cat("Object type:", x$object_type, "
")
    if (!is.null(x$description) && !is.na(x$description)) {
        cat("Description:", x$description, "
")
    }
    if (!is.null(x$owner) && !is.na(x$owner)) {
        cat("Owner:      ", x$owner, "
")
    }

    # List scopes
    scope_names <- setdiff(names(x), c("id", "object_type", "description", "owner"))
    if (length(scope_names) > 0) {
        cat("
")
        cat("Scopes:
")
        for (s in scope_names) {
            n_versions <- length(x[[s]])
            active <- sapply(x[[s]], function(v) v$status == "active")
            n_active <- sum(active)
            cat("
  $", s, " (", n_versions, " version(s), ", n_active, " active)
", sep = "")
        }
    }
    cat("
")
    invisible(x)
}

#' @export
`$.ontology_concept` <- function(x, name) {
    # Special computed properties
    if (name == "versions") {
        # Return all versions as a flat data frame
        ont <- attr(x, ".ont")
        return(ont_list_versions(x$id, con = ont$.con))
    }
    if (name == "active") {
        # Return the active version (if any)
        ont <- attr(x, ".ont")
        scope_names <- setdiff(names(x), c("id", "object_type", "description", "owner"))
        active_versions <- list()
        for (s in scope_names) {
            tryCatch({
                av <- ont_get_active_version(x$id, s, con = ont$.con)
                active_versions[[s]] <- make_version_object(ont, av)
            }, error = function(e) NULL)
        }
        return(active_versions)
    }

    # Regular slot access
    x[[name]]
}

# =============================================================================
# Scope Object (versions within a scope)
# =============================================================================

#' @export
print.ontology_scope <- function(x, ...) {
    concept_id <- attr(x, "concept_id")
    scope <- attr(x, "scope")

    cat("<ontology_scope>
")
    cat("
")
    cat("Concept:", concept_id, "
")
    cat("Scope:  ", scope, "
")
    cat("
")
    cat("Versions:
")
    for (v in names(x)) {
        status <- x[[v]]$status
        status_icon <- switch(status,
            "active" = cli::col_green("[active]"),
            "draft" = cli::col_yellow("[draft]"),
            "deprecated" = cli::col_grey("[deprecated]"),
            "retired" = cli::col_red("[retired]"),
            paste0("[", status, "]")
        )
        cat("  [[", v, "]] ", status_icon, "
", sep = "")
    }
    cat("
")
    invisible(x)
}

#' @export
`[[.ontology_scope` <- function(x, i) {
    # Allow numeric or character access
    if (is.numeric(i)) {
        i <- as.character(as.integer(i))
    }
    NextMethod()
}

# =============================================================================
# Version Object
# =============================================================================

#' @keywords internal
make_version_object <- function(ont, version_row) {
    structure(
        list(
            concept_id = version_row$concept_id,
            scope = version_row$scope,
            version = version_row$version,
            sql_expr = version_row$sql_expr,
            status = version_row$status,
            rationale = version_row$rationale,
            created_at = version_row$created_at
        ),
        class = "ontology_version",
        .ont = ont
    )
}

#' @export
print.ontology_version <- function(x, ...) {
    cat("<ontology_version>
")
    cat("
")
    cat(x$concept_id, "@", x$scope, " v", x$version, "
", sep = "")
    cat("
")
    cat("Status: ", x$status, "
")
    cat("SQL:    ", x$sql_expr, "
")
    if (!is.null(x$rationale) && !is.na(x$rationale)) {
        cat("
")
        cat("Rationale:
", x$rationale, "
")
    }
    cat("
")
    invisible(x)
}

#' @export
`$.ontology_version` <- function(x, name) {
    ont <- attr(x, ".ont")

    # Computed properties
    if (name == "evaluate") {
        return(function(...) {
            ont_evaluate(x$concept_id, x$scope, x$version, ..., con = ont$.con)
        })
    }
    if (name == "audits") {
        return(ont_get_audits(x$concept_id, x$scope, x$version, con = ont$.con))
    }
    if (name == "audit_summary") {
        return(ont_audit_summary(x$concept_id, x$scope, x$version, con = ont$.con))
    }
    if (name == "drift_check") {
        return(ont_check_drift(x$concept_id, x$scope, x$version, con = ont$.con))
    }

    # Regular slot access
    x[[name]]
}

# =============================================================================
# Objects Accessor
# =============================================================================

#' @keywords internal
get_objects_accessor <- function(ont) {
    objects_df <- ont_list_objects(con = ont$.con)

    if (nrow(objects_df) == 0) {
        return(structure(list(), class = "ontology_objects"))
    }

    objects_list <- lapply(seq_len(nrow(objects_df)), function(i) {
        row <- objects_df[i, ]
        structure(
            list(
                object_type = row$object_type,
                table_name = row$table_name,
                pk_column = row$pk_column,
                description = row$description
            ),
            class = "ontology_object_type"
        )
    })
    names(objects_list) <- objects_df$object_type

    structure(objects_list, class = "ontology_objects")
}

#' @export
print.ontology_objects <- function(x, ...) {
    cat("<ontology_objects>
")
    cat("
")
    if (length(x) == 0) {
        cat("No object types registered.
")
    } else {
        cat(length(x), "object type(s):
")
        for (nm in names(x)) {
            cat("
  $", nm, " -> ", x[[nm]]$table_name, "
", sep = "")
        }
    }
    cat("
")
    invisible(x)
}

#' @keywords internal
get_links_accessor <- function(ont) {
    ont_list_links(con = ont$.con)
}

# =============================================================================
# Cache Management
# =============================================================================

#' @keywords internal
invalidate_cache <- function(ont, what = "all") {
    if (what == "all" || what == "concepts") {
        ont$.cache$concepts <- NULL
    }
    if (what == "all" || what == "objects") {
        ont$.cache$objects <- NULL
    }
    ont$.cache$last_refresh <- Sys.time()
}
