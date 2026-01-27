# =============================================================================
# Composite Scores
# =============================================================================
# Composite scores combine multiple concept evaluations into a single metric.
# This enables "risk scores", "health scores", etc. that aggregate signals
# from multiple boolean concepts into a weighted numeric score.
# =============================================================================

#' Define a Composite Score
#'
#' Creates a composite score that aggregates multiple concepts into a single metric.
#'
#' @param score_id Unique identifier for this score.
#' @param score_name Human-readable name.
#' @param object_type Which object type this score applies to.
#' @param components List of component definitions, each with: concept_id, scope, weight, etc.
#' @param aggregation Aggregation method: "weighted_sum", "weighted_avg", "max", "min",
#'   "any_true", "all_true", "count_true".
#' @param description Optional description.
#' @param score_range_min Minimum possible score (default 0).
#' @param score_range_max Maximum possible score (default 100).
#' @param thresholds Named list of thresholds for tier assignment (e.g., list(low=30, medium=60, high=80)).
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the score_id.
#'
#' @examples
#' \dontrun{
#' ont_define_score(
#'   score_id = "patient_risk_score",
#'   score_name = "Patient Risk Score",
#'   object_type = "Patient",
#'   components = list(
#'     list(concept_id = "fall_risk", scope = "clinical", weight = 0.3),
#'     list(concept_id = "readmission_risk", scope = "clinical", weight = 0.4),
#'     list(concept_id = "medication_complexity", scope = "pharmacy", weight = 0.3)
#'   ),
#'   aggregation = "weighted_sum",
#'   thresholds = list(low = 30, medium = 60, high = 80)
#' )
#' }
#'
#' @export
ont_define_score <- function(score_id,
                              score_name,
                              object_type,
                              components,
                              aggregation = "weighted_sum",
                              description = NULL,
                              score_range_min = 0,
                              score_range_max = 100,
                              thresholds = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type
    obj <- ont_get_object(object_type, con)
    if (is.null(obj)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Validate aggregation method
    valid_aggs <- c("weighted_sum", "weighted_avg", "max", "min", "any_true", "all_true", "count_true")
    if (!aggregation %in% valid_aggs) {
        cli::cli_abort("Invalid aggregation: {.val {aggregation}}. Must be one of: {.val {valid_aggs}}")
    }

    # Validate components
    if (length(components) == 0) {
        cli::cli_abort("At least one component is required.")
    }

    for (i in seq_along(components)) {
        comp <- components[[i]]
        if (is.null(comp$concept_id) || is.null(comp$scope)) {
            cli::cli_abort("Component {i} must have concept_id and scope.")
        }
        # Validate concept exists
        concept <- ont_get_concept(comp$concept_id, con)
        if (is.null(concept)) {
            cli::cli_abort("Concept {.val {comp$concept_id}} not found.")
        }
    }

    # Convert thresholds to JSON
    thresholds_json <- if (!is.null(thresholds)) jsonlite::toJSON(thresholds, auto_unbox = TRUE) else NA

    # Insert score definition
    DBI::dbExecute(
        con,
        "INSERT INTO ont_scores (score_id, score_name, description, object_type, aggregation,
         score_range_min, score_range_max, thresholds)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            score_id,
            score_name,
            null_to_na(description),
            object_type,
            aggregation,
            score_range_min,
            score_range_max,
            thresholds_json
        )
    )

    # Insert components
    for (i in seq_along(components)) {
        comp <- components[[i]]
        component_id <- comp$component_id %||% paste0("comp_", i)
        weight <- comp$weight %||% 1.0
        transform <- comp$transform %||% NA
        invert <- comp$invert %||% FALSE
        required <- comp$required %||% TRUE
        version <- comp$version %||% NA

        DBI::dbExecute(
            con,
            "INSERT INTO ont_score_components (score_id, component_id, concept_id, scope,
             version, weight, transform, invert, required, display_order)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(
                score_id,
                component_id,
                comp$concept_id,
                comp$scope,
                version,
                weight,
                transform,
                invert,
                required,
                i
            )
        )
    }

    cli::cli_alert_success("Defined composite score {.val {score_id}} with {length(components)} components")
    invisible(score_id)
}

#' Evaluate Composite Score
#'
#' Calculates the composite score for objects.
#'
#' @param score_id The score to evaluate.
#' @param object_keys Optional vector of specific object keys to evaluate.
#'   If NULL, evaluates all objects.
#' @param include_components If TRUE, includes individual component values.
#' @param con Optional DBI connection.
#'
#' @return A tibble with object_key, score, tier, and optionally component values.
#'
#' @examples
#' \dontrun{
#' scores <- ont_evaluate_score("patient_risk_score")
#' high_risk <- scores[scores$tier == "high", ]
#' }
#'
#' @export
ont_evaluate_score <- function(score_id,
                                object_keys = NULL,
                                include_components = FALSE,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    # Get score definition
    score <- ont_get_score(score_id, con)
    if (is.null(score)) {
        cli::cli_abort("Score {.val {score_id}} not found.")
    }

    # Get components
    components <- ont_get_score_components(score_id, con)
    if (nrow(components) == 0) {
        cli::cli_abort("Score {.val {score_id}} has no components.")
    }

    # Get object type metadata
    obj_meta <- ont_get_object(score$object_type, con)

    # Build query to evaluate all components
    component_exprs <- character(nrow(components))
    component_names <- character(nrow(components))

    for (i in seq_len(nrow(components))) {
        comp <- components[i, ]

        # Get the concept version
        if (is.na(comp$version)) {
            cv <- ont_get_active_version(comp$concept_id, comp$scope, con)
        } else {
            cv <- ont_get_version(comp$concept_id, comp$scope, comp$version, con)
        }

        if (is.null(cv)) {
            cli::cli_warn("No version found for component {comp$component_id}, using FALSE")
            sql_expr <- "FALSE"
        } else {
            sql_expr <- cv$sql_expr
        }

        # Apply inversion if needed
        if (comp$invert) {
            sql_expr <- glue::glue("NOT ({sql_expr})")
        }

        # Apply transform if defined, otherwise convert boolean to 0/1
        if (!is.na(comp$transform)) {
            component_exprs[i] <- glue::glue("({comp$transform}) AS {comp$component_id}")
        } else {
            component_exprs[i] <- glue::glue("CASE WHEN ({sql_expr}) THEN 1.0 ELSE 0.0 END AS {comp$component_id}")
        }
        component_names[i] <- comp$component_id
    }

    # Build aggregation expression
    agg_expr <- switch(score$aggregation,
        "weighted_sum" = {
            terms <- sapply(seq_len(nrow(components)), function(i) {
                glue::glue("{components$weight[i]} * {component_names[i]}")
            })
            paste(terms, collapse = " + ")
        },
        "weighted_avg" = {
            terms <- sapply(seq_len(nrow(components)), function(i) {
                glue::glue("{components$weight[i]} * {component_names[i]}")
            })
            total_weight <- sum(components$weight)
            glue::glue("({paste(terms, collapse = ' + ')}) / {total_weight}")
        },
        "max" = {
            glue::glue("GREATEST({paste(component_names, collapse = ', ')})")
        },
        "min" = {
            glue::glue("LEAST({paste(component_names, collapse = ', ')})")
        },
        "any_true" = {
            glue::glue("CASE WHEN ({paste(component_names, collapse = ' + ')}) > 0 THEN 1.0 ELSE 0.0 END")
        },
        "all_true" = {
            glue::glue("CASE WHEN ({paste(component_names, collapse = ' * ')}) > 0 THEN 1.0 ELSE 0.0 END")
        },
        "count_true" = {
            paste(component_names, collapse = " + ")
        }
    )

    # Scale to score range
    range_size <- score$score_range_max - score$score_range_min
    if (score$aggregation %in% c("weighted_sum", "weighted_avg")) {
        # Normalize: assume components are 0-1, scale to range
        agg_expr <- glue::glue("{score$score_range_min} + ({agg_expr}) * {range_size}")
    }

    # Build tier expression if thresholds defined
    tier_expr <- "'none'"
    if (!is.na(score$thresholds)) {
        thresholds <- jsonlite::fromJSON(score$thresholds)
        tier_names <- names(thresholds)
        tier_values <- unlist(thresholds)
        # Sort by threshold value
        ord <- order(tier_values)
        tier_names <- tier_names[ord]
        tier_values <- tier_values[ord]

        # Build CASE expression
        tier_cases <- character(length(tier_names))
        for (i in seq_along(tier_names)) {
            if (i == 1) {
                tier_cases[i] <- glue::glue("WHEN score < {tier_values[i]} THEN '{tier_names[i]}'")
            } else {
                tier_cases[i] <- glue::glue("WHEN score < {tier_values[i]} THEN '{tier_names[i]}'")
            }
        }
        tier_expr <- glue::glue("CASE {paste(tier_cases, collapse = ' ')} ELSE '{tier_names[length(tier_names)]}' END")
    }

    # Build final query
    if (include_components) {
        select_clause <- glue::glue("{obj_meta$pk_column} AS object_key, {paste(component_exprs, collapse = ', ')}")
    } else {
        select_clause <- glue::glue("{obj_meta$pk_column} AS object_key")
    }

    query <- glue::glue("
        WITH component_values AS (
            SELECT {obj_meta$pk_column} AS object_key,
                   {paste(component_exprs, collapse = ', ')}
            FROM {obj_meta$table_name}
        ),
        scores AS (
            SELECT object_key,
                   {paste(component_names, collapse = ', ')},
                   ({agg_expr}) AS score
            FROM component_values
        )
        SELECT object_key,
               {if (include_components) paste(component_names, collapse = ', ') else ''},
               score,
               {tier_expr} AS tier
        FROM scores
    ")

    # Clean up query (remove empty comma)
    query <- gsub(",\\s*,", ",", query)
    query <- gsub(",\\s*score", " score", query)

    # Apply object filter if provided
    if (!is.null(object_keys)) {
        placeholders <- paste(rep("?", length(object_keys)), collapse = ", ")
        query <- glue::glue("SELECT * FROM ({query}) sq WHERE object_key IN ({placeholders})")
        result <- DBI::dbGetQuery(con, query, params = as.list(object_keys))
    } else {
        result <- DBI::dbGetQuery(con, query)
    }

    tibble::as_tibble(result)
}

#' Get Score Definition
#'
#' Retrieves a score definition.
#'
#' @param score_id The score ID.
#' @param con Optional DBI connection.
#'
#' @return A single-row data frame, or NULL if not found.
#'
#' @export
ont_get_score <- function(score_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_scores WHERE score_id = ?",
        params = list(score_id)
    )

    if (nrow(result) == 0) return(NULL)
    result[1, ]
}

#' Get Score Components
#'
#' Retrieves the components of a composite score.
#'
#' @param score_id The score ID.
#' @param con Optional DBI connection.
#'
#' @return A tibble of components.
#'
#' @export
ont_get_score_components <- function(score_id, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_score_components WHERE score_id = ? ORDER BY display_order",
        params = list(score_id)
    )

    tibble::as_tibble(result)
}

#' List Scores
#'
#' Lists all defined composite scores.
#'
#' @param object_type Optional filter by object type.
#' @param con Optional DBI connection.
#'
#' @return A tibble of scores with component counts.
#'
#' @export
ont_list_scores <- function(object_type = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT s.*, COUNT(sc.component_id) AS component_count
              FROM ont_scores s
              LEFT JOIN ont_score_components sc ON s.score_id = sc.score_id
              WHERE s.enabled = TRUE"
    params <- list()

    if (!is.null(object_type)) {
        query <- paste(query, "AND s.object_type = ?")
        params <- c(params, object_type)
    }

    query <- paste(query, "GROUP BY s.score_id ORDER BY s.score_name")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Observe Score
#'
#' Records a point-in-time observation of score distribution for trend analysis.
#'
#' @param score_id The score to observe.
#' @param observer_id Optional identifier for who/what initiated the observation.
#' @param con Optional DBI connection.
#'
#' @return A list with observation_id and summary statistics.
#'
#' @export
ont_observe_score <- function(score_id, observer_id = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    # Evaluate scores for all objects
    scores <- ont_evaluate_score(score_id, con = con)

    if (nrow(scores) == 0) {
        cli::cli_warn("No objects to score.")
        return(NULL)
    }

    # Calculate statistics
    observation_id <- paste0("SOBS-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))
    total_objects <- nrow(scores)
    avg_score <- mean(scores$score, na.rm = TRUE)
    min_score <- min(scores$score, na.rm = TRUE)
    max_score <- max(scores$score, na.rm = TRUE)
    std_dev <- sd(scores$score, na.rm = TRUE)

    # Count by tier
    tier_counts <- table(scores$tier)
    tier_counts_json <- jsonlite::toJSON(as.list(tier_counts), auto_unbox = TRUE)

    # Record observation
    DBI::dbExecute(
        con,
        "INSERT INTO ont_score_observations (observation_id, score_id, total_objects,
         avg_score, min_score, max_score, std_dev, tier_counts, observer_id)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            observation_id,
            score_id,
            total_objects,
            avg_score,
            min_score,
            max_score,
            std_dev,
            tier_counts_json,
            null_to_na(observer_id)
        )
    )

    cli::cli_alert_success("Recorded score observation {.val {observation_id}}")

    list(
        observation_id = observation_id,
        total_objects = total_objects,
        avg_score = avg_score,
        min_score = min_score,
        max_score = max_score,
        std_dev = std_dev,
        tier_counts = as.list(tier_counts)
    )
}

#' Get Score Trend
#'
#' Retrieves historical observations of a score for trend analysis.
#'
#' @param score_id The score ID.
#' @param since Optional datetime to filter observations after.
#' @param limit Maximum number of observations to return.
#' @param con Optional DBI connection.
#'
#' @return A tibble of score observations.
#'
#' @export
ont_score_trend <- function(score_id, since = NULL, limit = 100, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_score_observations WHERE score_id = ?"
    params <- list(score_id)

    if (!is.null(since)) {
        query <- paste(query, "AND observed_at >= ?")
        params <- c(params, since)
    }

    query <- paste(query, "ORDER BY observed_at DESC LIMIT ?")
    params <- c(params, limit)

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Add Score Component
#'
#' Adds a new component to an existing score.
#'
#' @param score_id The score to add to.
#' @param concept_id The concept for this component.
#' @param scope The concept scope.
#' @param component_id Optional component identifier (auto-generated if NULL).
#' @param weight Component weight (default 1.0).
#' @param version Optional specific version (NULL = use active).
#' @param transform Optional SQL transform expression.
#' @param invert If TRUE, use NOT concept_value.
#' @param required If TRUE, NULL concept_value fails the score.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the component_id.
#'
#' @export
ont_add_score_component <- function(score_id,
                                     concept_id,
                                     scope,
                                     component_id = NULL,
                                     weight = 1.0,
                                     version = NULL,
                                     transform = NULL,
                                     invert = FALSE,
                                     required = TRUE,
                                     con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate score exists
    score <- ont_get_score(score_id, con)
    if (is.null(score)) {
        cli::cli_abort("Score {.val {score_id}} not found.")
    }

    # Validate concept exists
    concept <- ont_get_concept(concept_id, con)
    if (is.null(concept)) {
        cli::cli_abort("Concept {.val {concept_id}} not found.")
    }

    # Generate component_id if not provided
    if (is.null(component_id)) {
        existing <- ont_get_score_components(score_id, con)
        component_id <- paste0("comp_", nrow(existing) + 1)
    }

    # Get max display order
    max_order <- DBI::dbGetQuery(con,
        "SELECT COALESCE(MAX(display_order), 0) as max_order FROM ont_score_components WHERE score_id = ?",
        params = list(score_id)
    )$max_order

    DBI::dbExecute(
        con,
        "INSERT INTO ont_score_components (score_id, component_id, concept_id, scope,
         version, weight, transform, invert, required, display_order)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            score_id,
            component_id,
            concept_id,
            scope,
            null_to_na(version),
            weight,
            null_to_na(transform),
            invert,
            required,
            max_order + 1
        )
    )

    cli::cli_alert_success("Added component {.val {component_id}} to score {.val {score_id}}")
    invisible(component_id)
}

#' Remove Score Component
#'
#' Removes a component from a score.
#'
#' @param score_id The score ID.
#' @param component_id The component to remove.
#' @param con Optional DBI connection.
#'
#' @return TRUE if successful.
#'
#' @export
ont_remove_score_component <- function(score_id, component_id, con = NULL) {
    con <- con %||% ont_get_connection()

    n <- DBI::dbExecute(
        con,
        "DELETE FROM ont_score_components WHERE score_id = ? AND component_id = ?",
        params = list(score_id, component_id)
    )

    if (n == 0) {
        cli::cli_warn("Component {.val {component_id}} not found in score {.val {score_id}}")
        return(FALSE)
    }

    cli::cli_alert_success("Removed component {.val {component_id}} from score {.val {score_id}}")
    TRUE
}

#' Score Distribution Summary
#'
#' Returns distribution statistics for a score.
#'
#' @param score_id The score ID.
#' @param con Optional DBI connection.
#'
#' @return A list with distribution statistics and tier breakdown.
#'
#' @export
ont_score_distribution <- function(score_id, con = NULL) {
    con <- con %||% ont_get_connection()

    scores <- ont_evaluate_score(score_id, con = con)

    if (nrow(scores) == 0) {
        return(list(
            total = 0,
            mean = NA,
            median = NA,
            sd = NA,
            min = NA,
            max = NA,
            quantiles = NULL,
            tier_counts = NULL
        ))
    }

    list(
        total = nrow(scores),
        mean = mean(scores$score, na.rm = TRUE),
        median = stats::median(scores$score, na.rm = TRUE),
        sd = sd(scores$score, na.rm = TRUE),
        min = min(scores$score, na.rm = TRUE),
        max = max(scores$score, na.rm = TRUE),
        quantiles = stats::quantile(scores$score, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE),
        tier_counts = as.list(table(scores$tier))
    )
}
