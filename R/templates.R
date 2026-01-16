#' @title Concept Templates
#' @description Functions for creating and managing concept templates that can be
#'   inherited by country/scope-specific variants.
#' @name templates
NULL

#' Define a Concept Template
#'
#' Creates a template that serves as a base definition for related concepts.
#' Templates can contain parameter placeholders (e.g., `{{age_threshold}}`) that
#' variants can customize when inheriting.
#'
#' @param template_id Character. Unique identifier for the template.
#' @param template_name Character. Human-readable name.
#' @param object_type Character. The object type this template applies to.
#' @param base_sql_expr Character. SQL expression with optional `{{placeholder}}`
#'   parameters that variants can customize.
#' @param parameters Named list. Parameter definitions with default values.
#'   Each parameter can be a simple default value or a list with `default`,
#'   `type`, and `description` fields.
#' @param description Character. Description of the template.
#' @param source_standard Character. Source standard (e.g., "ILO", "OECD").
#' @param owner_domain Character. Domain that owns this template.
#' @param created_by Character. Who created this template.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the template_id.
#'
#' @examples
#' \dontrun{
#' ont_connect(":memory:")
#' ont_register_object("Person", "persons", "person_id")
#'
#' # Define ILO unemployed template
#' ont_define_template(
#'     template_id = "ilo_unemployed",
#'     template_name = "ILO Unemployed Definition",
#'     object_type = "Person",
#'     base_sql_expr = "
#'         age >= {{min_age}} AND age <= {{max_age}}
#'         AND NOT employed_last_week
#'         AND actively_seeking_work
#'         AND available_to_start_within_{{availability_weeks}}_weeks
#'     ",
#'     parameters = list(
#'         min_age = list(default = 15, type = "integer", description = "Minimum age"),
#'         max_age = list(default = 74, type = "integer", description = "Maximum age"),
#'         availability_weeks = list(default = 2, type = "integer", description = "Weeks to availability")
#'     ),
#'     source_standard = "ILO",
#'     description = "International Labour Organization standard unemployment definition"
#' )
#' }
#'
#' @export
ont_define_template <- function(template_id,
                                 template_name,
                                 object_type,
                                 base_sql_expr,
                                 parameters = NULL,
                                 description = NULL,
                                 source_standard = NULL,
                                 owner_domain = NULL,
                                 created_by = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    # Verify object type exists
    ont_get_object(object_type, con)

    # Check if template already exists
    existing <- DBI::dbGetQuery(con,
        "SELECT template_id FROM ont_templates WHERE template_id = ?",
        params = list(template_id)
    )

    if (nrow(existing) > 0) {
        cli::cli_abort("Template {.val {template_id}} already exists.")
    }

    # Convert parameters to JSON
    params_json <- if (!is.null(parameters)) {
        jsonlite::toJSON(parameters, auto_unbox = TRUE)
    } else {
        NA_character_
    }

    # Insert template
    DBI::dbExecute(con, "
        INSERT INTO ont_templates
        (template_id, template_name, object_type, base_sql_expr, parameters,
         description, source_standard, owner_domain, created_by)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
        template_id,
        template_name,
        object_type,
        base_sql_expr,
        params_json,
        null_to_na(description),
        null_to_na(source_standard),
        null_to_na(owner_domain),
        null_to_na(created_by)
    ))

    cli::cli_alert_success("Created template: {.val {template_name}} ({.val {template_id}})")

    invisible(template_id)
}

#' Get Template Details
#'
#' Retrieves information about a specific template.
#'
#' @param template_id Character. The template ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A list with template details including parsed parameters.
#'
#' @examples
#' \dontrun{
#' template <- ont_get_template("ilo_unemployed")
#' template$parameters
#' }
#'
#' @export
ont_get_template <- function(template_id, con = NULL) {
    con <- con %||% ont_get_connection()

    template <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_templates WHERE template_id = ?",
        params = list(template_id)
    )

    if (nrow(template) == 0) {
        cli::cli_abort("Template {.val {template_id}} not found.")
    }

    result <- as.list(template[1, ])

    # Parse parameters JSON
    if (!is.na(result$parameters)) {
        result$parameters <- jsonlite::fromJSON(result$parameters)
    } else {
        result$parameters <- list()
    }

    result
}

#' List All Templates
#'
#' Returns a tibble of all defined templates.
#'
#' @param object_type Character. Filter by object type (optional).
#' @param source_standard Character. Filter by source standard (optional).
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of templates.
#'
#' @examples
#' \dontrun{
#' ont_list_templates()
#' ont_list_templates(source_standard = "ILO")
#' }
#'
#' @export
ont_list_templates <- function(object_type = NULL, source_standard = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    sql <- "SELECT template_id, template_name, object_type, source_standard,
                   owner_domain, description, created_at
            FROM ont_templates WHERE 1=1"

    params <- list()

    if (!is.null(object_type)) {
        sql <- paste(sql, "AND object_type = ?")
        params <- c(params, object_type)
    }

    if (!is.null(source_standard)) {
        sql <- paste(sql, "AND source_standard = ?")
        params <- c(params, source_standard)
    }

    sql <- paste(sql, "ORDER BY template_name")

    if (length(params) > 0) {
        DBI::dbGetQuery(con, sql, params = params) |>
            tibble::as_tibble()
    } else {
        DBI::dbGetQuery(con, sql) |>
            tibble::as_tibble()
    }
}

#' Create a Concept from Template
#'
#' Creates a new concept that inherits from a template, optionally customizing
#' parameter values. The SQL expression is generated by substituting parameters
#' in the template's base expression.
#'
#' @param concept_id Character. Unique identifier for the new concept.
#' @param template_id Character. The template to inherit from.
#' @param scope Character. Scope for the initial version.
#' @param parameter_values Named list. Parameter values to override defaults.
#' @param inheritance_type Character. Type of inheritance: "extends" (default),
#'   "implements", or "adapts".
#' @param deviation_notes Character. Notes explaining how this variant differs
#'   from the base template.
#' @param description Character. Description of this specific concept variant.
#' @param owner_domain Character. Domain that owns this concept.
#' @param status Character. Initial status for the version. Default "draft".
#' @param rationale Character. Rationale for this version.
#' @param created_by Character. Who created this.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Invisibly returns the concept_id.
#'
#' @details
#' The function:
#' 1. Loads the template and its parameter definitions
#' 2. Merges provided parameter_values with template defaults
#' 3. Substitutes parameters into the base SQL expression
#' 4. Creates the concept and initial version
#' 5. Records the inheritance relationship
#'
#' @examples
#' \dontrun{
#' # Create Ireland-specific unemployed concept from ILO template
#' ont_inherit_concept(
#'     concept_id = "unemployed_ireland",
#'     template_id = "ilo_unemployed",
#'     scope = "ireland",
#'     parameter_values = list(
#'         min_age = 16,
#'         max_age = 66
#'     ),
#'     deviation_notes = "Ireland uses 16-66 age range per CSO guidelines",
#'     description = "CSO Ireland unemployment definition based on ILO standards"
#' )
#' }
#'
#' @export
ont_inherit_concept <- function(concept_id,
                                 template_id,
                                 scope,
                                 parameter_values = list(),
                                 inheritance_type = "extends",
                                 deviation_notes = NULL,
                                 description = NULL,
                                 owner_domain = NULL,
                                 status = "draft",
                                 rationale = NULL,
                                 created_by = NULL,
                                 con = NULL) {
    con <- con %||% ont_get_connection()

    # Get template
    template <- ont_get_template(template_id, con)

    # Merge parameters with defaults
    final_params <- template$parameters
    for (name in names(parameter_values)) {
        if (is.list(final_params[[name]]) && "default" %in% names(final_params[[name]])) {
            final_params[[name]]$default <- parameter_values[[name]]
        } else {
            final_params[[name]] <- parameter_values[[name]]
        }
    }

    # Build substitution map
    param_map <- list()
    for (name in names(final_params)) {
        if (is.list(final_params[[name]]) && "default" %in% names(final_params[[name]])) {
            param_map[[name]] <- final_params[[name]]$default
        } else {
            param_map[[name]] <- final_params[[name]]
        }
    }

    # Substitute parameters in SQL expression
    sql_expr <- template$base_sql_expr
    for (name in names(param_map)) {
        placeholder <- paste0("{{", name, "}}")
        sql_expr <- gsub(placeholder, as.character(param_map[[name]]), sql_expr, fixed = TRUE)
    }

    # Clean up SQL (remove extra whitespace)
    sql_expr <- trimws(gsub("\\s+", " ", sql_expr))

    # Create the concept
    ont_define_concept(
        concept_id = concept_id,
        object_type = template$object_type,
        description = description %||% paste("Variant of", template$template_name),
        owner_domain = owner_domain,
        created_by = created_by,
        con = con
    )

    # Add the initial version
    ont_add_version(
        concept_id = concept_id,
        scope = scope,
        sql_expr = sql_expr,
        status = status,
        rationale = rationale %||% paste("Inherited from template:", template_id),
        created_by = created_by,
        con = con
    )

    # Record inheritance relationship
    params_json <- jsonlite::toJSON(parameter_values, auto_unbox = TRUE)

    DBI::dbExecute(con, "
        INSERT INTO ont_template_inheritance
        (concept_id, template_id, parameter_values, inheritance_type, deviation_notes)
        VALUES (?, ?, ?, ?, ?)
    ", params = list(
        concept_id,
        template_id,
        params_json,
        inheritance_type,
        null_to_na(deviation_notes)
    ))

    cli::cli_alert_success(
        "Created concept {.val {concept_id}} from template {.val {template_id}}"
    )

    invisible(concept_id)
}

#' Get Template Variants
#'
#' Lists all concepts that inherit from a specific template.
#'
#' @param template_id Character. The template ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of concepts that inherit from the template.
#'
#' @examples
#' \dontrun{
#' ont_get_template_variants("ilo_unemployed")
#' }
#'
#' @export
ont_get_template_variants <- function(template_id, con = NULL) {
    con <- con %||% ont_get_connection()

    # Verify template exists
    ont_get_template(template_id, con)

    DBI::dbGetQuery(con, "
        SELECT
            ti.concept_id,
            c.description,
            c.owner_domain,
            ti.inheritance_type,
            ti.parameter_values,
            ti.deviation_notes,
            ti.created_at AS inherited_at
        FROM ont_template_inheritance ti
        JOIN ont_concepts c ON ti.concept_id = c.concept_id
        WHERE ti.template_id = ?
        ORDER BY ti.created_at
    ", params = list(template_id)) |>
        tibble::as_tibble()
}

#' Get Concept Inheritance
#'
#' Returns the template(s) that a concept inherits from.
#'
#' @param concept_id Character. The concept ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble of templates this concept inherits from.
#'
#' @examples
#' \dontrun{
#' ont_get_concept_inheritance("unemployed_ireland")
#' }
#'
#' @export
ont_get_concept_inheritance <- function(concept_id, con = NULL) {
    con <- con %||% ont_get_connection()

    DBI::dbGetQuery(con, "
        SELECT
            ti.template_id,
            t.template_name,
            t.source_standard,
            ti.inheritance_type,
            ti.parameter_values,
            ti.deviation_notes,
            ti.created_at
        FROM ont_template_inheritance ti
        JOIN ont_templates t ON ti.template_id = t.template_id
        WHERE ti.concept_id = ?
    ", params = list(concept_id)) |>
        tibble::as_tibble()
}

#' Compare Template Variants
#'
#' Compares how different concepts have customized a template's parameters.
#'
#' @param template_id Character. The template ID.
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return A tibble showing parameter values across all variants.
#'
#' @examples
#' \dontrun{
#' ont_compare_template_variants("ilo_unemployed")
#' }
#'
#' @export
ont_compare_template_variants <- function(template_id, con = NULL) {
    con <- con %||% ont_get_connection()

    template <- ont_get_template(template_id, con)
    variants <- ont_get_template_variants(template_id, con)

    if (nrow(variants) == 0) {
        cli::cli_alert_info("No variants found for template {.val {template_id}}")
        return(tibble::tibble())
    }

    # Extract parameter names from template
    param_names <- names(template$parameters)

    # Build comparison data
    comparison <- lapply(seq_len(nrow(variants)), function(i) {
        variant <- variants[i, ]
        row <- list(
            concept_id = variant$concept_id,
            description = variant$description,
            inheritance_type = variant$inheritance_type
        )

        # Parse parameter values
        if (!is.na(variant$parameter_values) && variant$parameter_values != "{}") {
            params <- jsonlite::fromJSON(variant$parameter_values)
        } else {
            params <- list()
        }

        # Add each parameter
        for (pname in param_names) {
            if (pname %in% names(params)) {
                row[[paste0("param_", pname)]] <- as.character(params[[pname]])
            } else {
                # Use template default
                default_val <- template$parameters[[pname]]
                if (is.list(default_val) && "default" %in% names(default_val)) {
                    row[[paste0("param_", pname)]] <- paste0(default_val$default, " (default)")
                } else {
                    row[[paste0("param_", pname)]] <- paste0(default_val, " (default)")
                }
            }
        }

        tibble::as_tibble(row)
    })

    dplyr::bind_rows(comparison)
}

#' Render Template SQL
#'
#' Renders a template's SQL expression with specific parameter values,
#' without creating a concept. Useful for previewing or ad-hoc evaluation.
#'
#' @param template_id Character. The template ID.
#' @param parameter_values Named list. Parameter values (uses defaults for missing).
#' @param con A DBI connection. If `NULL`, uses the active connection.
#'
#' @return Character. The rendered SQL expression.
#'
#' @examples
#' \dontrun{
#' sql <- ont_render_template(
#'     "ilo_unemployed",
#'     list(min_age = 18, max_age = 65)
#' )
#' cat(sql)
#' }
#'
#' @export
ont_render_template <- function(template_id, parameter_values = list(), con = NULL) {
    con <- con %||% ont_get_connection()

    template <- ont_get_template(template_id, con)

    # Merge with defaults
    final_params <- list()
    for (name in names(template$parameters)) {
        param_def <- template$parameters[[name]]
        if (name %in% names(parameter_values)) {
            final_params[[name]] <- parameter_values[[name]]
        } else if (is.list(param_def) && "default" %in% names(param_def)) {
            final_params[[name]] <- param_def$default
        } else {
            final_params[[name]] <- param_def
        }
    }

    # Substitute
    sql_expr <- template$base_sql_expr
    for (name in names(final_params)) {
        placeholder <- paste0("{{", name, "}}")
        sql_expr <- gsub(placeholder, as.character(final_params[[name]]), sql_expr, fixed = TRUE)
    }

    # Clean up
    trimws(gsub("\\s+", " ", sql_expr))
}
