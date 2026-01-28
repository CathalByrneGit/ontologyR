# =============================================================================
# Spatial / Geospatial Support
# =============================================================================
# Functions for exporting ontology data to geospatial formats (GeoJSON, CZML)
# for visualization in Cesium, Leaflet, and other mapping tools.
# =============================================================================

#' Register Object Geometry
#'
#' Associates spatial column information with an object type, enabling
#' geospatial exports and visualization.
#'
#' @param object_type The object type to add geometry to.
#' @param geometry_type Type of geometry: "point", "polygon", "linestring", "wkt", "geojson".
#' @param lon_column Column containing longitude (for point geometry).
#' @param lat_column Column containing latitude (for point geometry).
#' @param alt_column Optional column containing altitude/elevation.
#' @param geometry_column Column containing WKT or GeoJSON (for complex geometries).
#' @param srid Spatial Reference ID (default 4326 = WGS84).
#' @param default_style Optional list of default visualization styles.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the object_type.
#'
#' @examples
#' \dontrun{
#' # Register point geometry
#' ont_register_geometry("Transformer", "point",
#'   lon_column = "longitude",
#'   lat_column = "latitude",
#'   alt_column = "elevation")
#'
#' # Register polygon geometry from WKT column
#' ont_register_geometry("ServiceArea", "wkt",
#'   geometry_column = "boundary_wkt")
#' }
#'
#' @export
ont_register_geometry <- function(object_type,
                                   geometry_type = "point",
                                   lon_column = NULL,
                                   lat_column = NULL,
                                   alt_column = NULL,
                                   geometry_column = NULL,
                                   srid = 4326,
                                   default_style = NULL,
                                   con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type exists
    obj <- ont_get_object(object_type, con)
    if (is.null(obj)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Validate geometry type
    valid_types <- c("point", "polygon", "linestring", "wkt", "geojson")
    if (!geometry_type %in% valid_types) {
        cli::cli_abort("Invalid geometry_type. Must be one of: {.val {valid_types}}")
    }

    # Validate required columns based on geometry type
    if (geometry_type == "point") {
        if (is.null(lon_column) || is.null(lat_column)) {
            cli::cli_abort("Point geometry requires lon_column and lat_column.")
        }
    } else if (geometry_type %in% c("wkt", "geojson", "polygon", "linestring")) {
        if (is.null(geometry_column)) {
            cli::cli_abort("Complex geometry types require geometry_column.")
        }
    }

    # Convert style to JSON
    style_json <- if (!is.null(default_style)) {
        jsonlite::toJSON(default_style, auto_unbox = TRUE)
    } else {
        NA
    }

    # Upsert geometry record
    DBI::dbExecute(con, "DELETE FROM ont_object_geometry WHERE object_type = ?",
                   params = list(object_type))

    DBI::dbExecute(
        con,
        "INSERT INTO ont_object_geometry (object_type, geometry_type, lon_column,
         lat_column, alt_column, geometry_column, srid, default_style)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            object_type,
            geometry_type,
            null_to_na(lon_column),
            null_to_na(lat_column),
            null_to_na(alt_column),
            null_to_na(geometry_column),
            srid,
            style_json
        )
    )

    cli::cli_alert_success("Registered {geometry_type} geometry for {.val {object_type}}")
    invisible(object_type)
}

#' Get Object Geometry
#'
#' Retrieves geometry configuration for an object type.
#'
#' @param object_type The object type.
#' @param con Optional DBI connection.
#'
#' @return A single-row data frame with geometry info, or NULL if not found.
#'
#' @export
ont_get_geometry <- function(object_type, con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_object_geometry WHERE object_type = ?",
        params = list(object_type)
    )

    if (nrow(result) == 0) return(NULL)
    result[1, ]
}

#' Export to GeoJSON
#'
#' Exports objects with their concept evaluations to GeoJSON format
#' for visualization in Cesium, Leaflet, or other mapping tools.
#'
#' @param object_type The object type to export.
#' @param concept_id Optional concept to evaluate and include.
#' @param scope Scope for the concept.
#' @param version Version of the concept (NULL = active).
#' @param score_id Optional score to evaluate and include.
#' @param filter_expr Optional SQL filter expression.
#' @param style List of styling options: color_by, size_by, color_true, color_false, etc.
#' @param properties Character vector of additional columns to include as properties.
#' @param file Optional file path to save GeoJSON. If NULL, returns as list.
#' @param con Optional DBI connection.
#'
#' @return A GeoJSON-compatible list (FeatureCollection), or file path if saved.
#'
#' @examples
#' \dontrun{
#' # Export transformers colored by risk
#' geojson <- ont_export_geojson(
#'   object_type = "Transformer",
#'   concept_id = "high_risk",
#'   scope = "predictive",
#'   style = list(
#'     color_true = "#FF0000",
#'     color_false = "#00FF00"
#'   )
#' )
#'
#' # Export with score-based coloring
#' geojson <- ont_export_geojson(
#'   object_type = "Asset",
#'   score_id = "health_score",
#'   style = list(
#'     color_by = "score",
#'     color_scale = c("#00FF00", "#FFFF00", "#FF0000")
#'   ),
#'   file = "assets.geojson"
#' )
#' }
#'
#' @export
ont_export_geojson <- function(object_type,
                                concept_id = NULL,
                                scope = NULL,
                                version = NULL,
                                score_id = NULL,
                                filter_expr = NULL,
                                style = list(),
                                properties = NULL,
                                file = NULL,
                                con = NULL) {
    con <- con %||% ont_get_connection()

    # Get object metadata
    obj_meta <- ont_get_object(object_type, con)
    if (is.null(obj_meta)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Get geometry configuration
    geom <- ont_get_geometry(object_type, con)
    if (is.null(geom)) {
        cli::cli_abort("No geometry registered for {.val {object_type}}. Use ont_register_geometry() first.")
    }

    # Build SELECT clause
    select_cols <- c(
        glue::glue("{obj_meta$pk_column} AS object_key")
    )

    # Add geometry columns
    if (geom$geometry_type == "point") {
        select_cols <- c(select_cols,
            glue::glue("{geom$lon_column} AS _lon"),
            glue::glue("{geom$lat_column} AS _lat")
        )
        if (!is.na(geom$alt_column)) {
            select_cols <- c(select_cols, glue::glue("{geom$alt_column} AS _alt"))
        }
    } else {
        select_cols <- c(select_cols, glue::glue("{geom$geometry_column} AS _geometry"))
    }

    # Add concept evaluation if requested
    if (!is.null(concept_id)) {
        if (is.null(version)) {
            cv <- ont_get_active_version(concept_id, scope, con)
        } else {
            cv <- ont_get_version(concept_id, scope, version, con)
        }
        if (!is.null(cv)) {
            select_cols <- c(select_cols, glue::glue("({cv$sql_expr}) AS concept_value"))
        }
    }

    # Add score evaluation if requested
    if (!is.null(score_id)) {
        # For scores, we need to evaluate separately and join
        # For now, add placeholder - will compute after
        select_cols <- c(select_cols, "NULL AS score_value")
    }

    # Add additional properties
    if (!is.null(properties)) {
        select_cols <- c(select_cols, properties)
    }

    # Build query
    query <- glue::glue("SELECT {paste(select_cols, collapse = ', ')} FROM {obj_meta$table_name}")

    if (!is.null(filter_expr)) {
        query <- glue::glue("{query} WHERE {filter_expr}")
    }

    # Execute query
    data <- DBI::dbGetQuery(con, query)

    # If score requested, evaluate and merge
    if (!is.null(score_id)) {
        scores <- ont_evaluate_score(score_id, con = con)
        if (nrow(scores) > 0) {
            data <- merge(data, scores[, c("object_key", "score", "tier")],
                          by = "object_key", all.x = TRUE)
            data$score_value <- data$score
        }
    }

    # Build GeoJSON features
    features <- lapply(seq_len(nrow(data)), function(i) {
        row <- data[i, ]

        # Build geometry
        if (geom$geometry_type == "point") {
            coords <- c(row$`_lon`, row$`_lat`)
            if ("_alt" %in% names(row) && !is.na(row$`_alt`)) {
                coords <- c(coords, row$`_alt`)
            }
            geometry <- list(type = "Point", coordinates = coords)
        } else if (geom$geometry_type == "geojson") {
            geometry <- jsonlite::fromJSON(row$`_geometry`)
        } else {
            # WKT - would need conversion library, simplified for now
            geometry <- list(type = "Unknown", wkt = row$`_geometry`)
        }

        # Build properties
        prop_cols <- setdiff(names(row), c("_lon", "_lat", "_alt", "_geometry"))
        props <- as.list(row[prop_cols])

        # Add styling based on concept/score values
        if (!is.null(concept_id) && "concept_value" %in% names(row)) {
            if (isTRUE(row$concept_value)) {
                props$`marker-color` <- style$color_true %||% "#FF0000"
            } else {
                props$`marker-color` <- style$color_false %||% "#00FF00"
            }
        }

        if (!is.null(score_id) && "score_value" %in% names(row)) {
            # Simple color interpolation based on score
            props$`marker-color` <- score_to_color(row$score_value, style)
        }

        list(
            type = "Feature",
            geometry = geometry,
            properties = props
        )
    })

    # Build FeatureCollection
    geojson <- list(
        type = "FeatureCollection",
        features = features,
        metadata = list(
            object_type = object_type,
            concept_id = concept_id,
            score_id = score_id,
            exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
            record_count = nrow(data)
        )
    )

    # Record export
    export_id <- paste0("EXP-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))
    DBI::dbExecute(
        con,
        "INSERT INTO ont_spatial_exports (export_id, export_type, object_type,
         concept_id, scope, score_id, record_count, file_path, parameters)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            export_id, "geojson", object_type,
            null_to_na(concept_id), null_to_na(scope), null_to_na(score_id),
            nrow(data), null_to_na(file),
            jsonlite::toJSON(style, auto_unbox = TRUE)
        )
    )

    # Save to file if requested
    if (!is.null(file)) {
        jsonlite::write_json(geojson, file, auto_unbox = TRUE, pretty = TRUE)
        cli::cli_alert_success("Exported {nrow(data)} features to {.file {file}}")
        return(invisible(file))
    }

    cli::cli_alert_success("Generated GeoJSON with {nrow(data)} features")
    geojson
}

#' Score to Color
#'
#' Internal function to convert a score value to a color.
#'
#' @param score The score value (0-100 typically).
#' @param style Style options including color_scale.
#'
#' @return A hex color string.
#'
#' @keywords internal
score_to_color <- function(score, style) {
    if (is.na(score)) return("#808080")  # Gray for NA

    # Default color scale: green -> yellow -> red
    scale <- style$color_scale %||% c("#00FF00", "#FFFF00", "#FF0000")
    min_score <- style$score_min %||% 0
    max_score <- style$score_max %||% 100

    # Normalize score to 0-1
    normalized <- (score - min_score) / (max_score - min_score)
    normalized <- max(0, min(1, normalized))

    # Interpolate colors
    n <- length(scale)
    idx <- normalized * (n - 1) + 1
    lower_idx <- floor(idx)
    upper_idx <- ceiling(idx)
    frac <- idx - lower_idx

    if (lower_idx == upper_idx || upper_idx > n) {
        return(scale[min(lower_idx, n)])
    }

    # Simple RGB interpolation
    col1 <- grDevices::col2rgb(scale[lower_idx])
    col2 <- grDevices::col2rgb(scale[upper_idx])
    result <- col1 + frac * (col2 - col1)

    grDevices::rgb(result[1], result[2], result[3], maxColorValue = 255)
}

#' Export to CZML
#'
#' Exports objects to CZML (Cesium Markup Language) for time-dynamic
#' visualization in CesiumJS.
#'
#' @param object_type The object type to export.
#' @param score_id Optional score for time-series visualization.
#' @param concept_id Optional concept to track over time.
#' @param scope Scope for the concept.
#' @param time_column Column containing timestamps.
#' @param time_range Character vector of start and end times (ISO 8601).
#' @param style List of styling options.
#' @param file Optional file path to save CZML.
#' @param con Optional DBI connection.
#'
#' @return A CZML-compatible list, or file path if saved.
#'
#' @examples
#' \dontrun{
#' czml <- ont_export_czml(
#'   object_type = "Sensor",
#'   score_id = "health_score",
#'   time_column = "reading_time",
#'   time_range = c("2024-01-01", "2024-12-31"),
#'   file = "sensor_health.czml"
#' )
#' }
#'
#' @export
ont_export_czml <- function(object_type,
                             score_id = NULL,
                             concept_id = NULL,
                             scope = NULL,
                             time_column = NULL,
                             time_range = NULL,
                             style = list(),
                             file = NULL,
                             con = NULL) {
    con <- con %||% ont_get_connection()

    # Get object metadata
    obj_meta <- ont_get_object(object_type, con)
    if (is.null(obj_meta)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Get geometry configuration
    geom <- ont_get_geometry(object_type, con)
    if (is.null(geom)) {
        cli::cli_abort("No geometry registered for {.val {object_type}}.")
    }

    if (geom$geometry_type != "point") {
        cli::cli_abort("CZML export currently only supports point geometry.")
    }

    # Build query for objects with positions
    query <- glue::glue("
        SELECT {obj_meta$pk_column} AS id,
               {geom$lon_column} AS lon,
               {geom$lat_column} AS lat
               {if (!is.na(geom$alt_column)) paste0(', ', geom$alt_column, ' AS alt') else ''}
        FROM {obj_meta$table_name}
        WHERE {geom$lon_column} IS NOT NULL
          AND {geom$lat_column} IS NOT NULL
    ")

    objects <- DBI::dbGetQuery(con, query)

    # Build CZML document
    czml <- list()

    # Document packet (required first element)
    czml[[1]] <- list(
        id = "document",
        name = paste("ontologyR Export:", object_type),
        version = "1.0"
    )

    # Create packet for each object
    for (i in seq_len(nrow(objects))) {
        obj <- objects[i, ]

        position <- c(obj$lon, obj$lat)
        if ("alt" %in% names(obj) && !is.na(obj$alt)) {
            position <- c(position, obj$alt)
        } else {
            position <- c(position, 0)
        }

        packet <- list(
            id = as.character(obj$id),
            name = as.character(obj$id),
            position = list(
                cartographicDegrees = position
            ),
            point = list(
                color = list(rgba = c(255, 255, 0, 255)),  # Yellow default
                pixelSize = 10,
                outlineColor = list(rgba = c(0, 0, 0, 255)),
                outlineWidth = 2
            )
        )

        # Add label
        packet$label <- list(
            text = as.character(obj$id),
            font = "12pt sans-serif",
            style = "FILL_AND_OUTLINE",
            outlineWidth = 2,
            verticalOrigin = "BOTTOM",
            pixelOffset = list(cartesian2 = c(0, -15))
        )

        czml[[length(czml) + 1]] <- packet
    }

    # Record export
    export_id <- paste0("EXP-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", substr(uuid::UUIDgenerate(), 1, 8))
    DBI::dbExecute(
        con,
        "INSERT INTO ont_spatial_exports (export_id, export_type, object_type,
         concept_id, scope, score_id, record_count, file_path)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            export_id, "czml", object_type,
            null_to_na(concept_id), null_to_na(scope), null_to_na(score_id),
            nrow(objects), null_to_na(file)
        )
    )

    # Save to file if requested
    if (!is.null(file)) {
        jsonlite::write_json(czml, file, auto_unbox = TRUE, pretty = TRUE)
        cli::cli_alert_success("Exported {nrow(objects)} objects to {.file {file}}")
        return(invisible(file))
    }

    cli::cli_alert_success("Generated CZML with {nrow(objects)} objects")
    czml
}

#' Define Spatial Layer
#'
#' Creates a named spatial layer for visualization.
#'
#' @param layer_id Unique identifier for the layer.
#' @param layer_name Human-readable name.
#' @param object_type The object type to visualize.
#' @param concept_id Optional concept for filtering/coloring.
#' @param scope Scope for the concept.
#' @param score_id Optional score for coloring.
#' @param style_rules List of styling rules.
#' @param description Optional description.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the layer_id.
#'
#' @export
ont_define_layer <- function(layer_id,
                              layer_name,
                              object_type,
                              concept_id = NULL,
                              scope = NULL,
                              score_id = NULL,
                              style_rules = list(),
                              description = NULL,
                              con = NULL) {
    con <- con %||% ont_get_connection()

    # Validate object type
    obj <- ont_get_object(object_type, con)
    if (is.null(obj)) {
        cli::cli_abort("Object type {.val {object_type}} not found.")
    }

    # Convert style rules to JSON
    style_json <- jsonlite::toJSON(style_rules, auto_unbox = TRUE)

    DBI::dbExecute(
        con,
        "INSERT INTO ont_spatial_layers (layer_id, layer_name, description,
         object_type, concept_id, scope, score_id, style_rules)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            layer_id, layer_name, null_to_na(description),
            object_type, null_to_na(concept_id), null_to_na(scope),
            null_to_na(score_id), style_json
        )
    )

    cli::cli_alert_success("Defined spatial layer {.val {layer_id}}")
    invisible(layer_id)
}

#' List Spatial Layers
#'
#' Lists all defined spatial layers.
#'
#' @param object_type Optional filter by object type.
#' @param con Optional DBI connection.
#'
#' @return A tibble of layers.
#'
#' @export
ont_list_layers <- function(object_type = NULL, con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_spatial_layers WHERE 1=1"
    params <- list()

    if (!is.null(object_type)) {
        query <- paste(query, "AND object_type = ?")
        params <- c(params, object_type)
    }

    query <- paste(query, "ORDER BY layer_order, layer_name")

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}

#' Define Spatial Region
#'
#' Creates a named geographic region for filtering.
#'
#' @param region_id Unique identifier for the region.
#' @param region_name Human-readable name.
#' @param bbox Optional bounding box: c(west, south, east, north).
#' @param geometry_wkt Optional WKT polygon.
#' @param description Optional description.
#' @param con Optional DBI connection.
#'
#' @return Invisibly returns the region_id.
#'
#' @export
ont_define_region <- function(region_id,
                               region_name,
                               bbox = NULL,
                               geometry_wkt = NULL,
                               description = NULL,
                               con = NULL) {
    con <- con %||% ont_get_connection()

    region_type <- if (!is.null(bbox)) "bbox" else if (!is.null(geometry_wkt)) "polygon" else "reference"

    DBI::dbExecute(
        con,
        "INSERT INTO ont_spatial_regions (region_id, region_name, region_type,
         bbox_west, bbox_south, bbox_east, bbox_north, geometry_wkt, description)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
            region_id, region_name, region_type,
            if (!is.null(bbox)) bbox[1] else NA,
            if (!is.null(bbox)) bbox[2] else NA,
            if (!is.null(bbox)) bbox[3] else NA,
            if (!is.null(bbox)) bbox[4] else NA,
            null_to_na(geometry_wkt),
            null_to_na(description)
        )
    )

    cli::cli_alert_success("Defined spatial region {.val {region_id}}")
    invisible(region_id)
}

#' List Spatial Regions
#'
#' Lists all defined spatial regions.
#'
#' @param con Optional DBI connection.
#'
#' @return A tibble of regions.
#'
#' @export
ont_list_regions <- function(con = NULL) {
    con <- con %||% ont_get_connection()

    result <- DBI::dbGetQuery(con, "SELECT * FROM ont_spatial_regions ORDER BY region_name")
    tibble::as_tibble(result)
}

#' Filter by Region
#'
#' Returns objects within a spatial region.
#'
#' @param object_type The object type to filter.
#' @param region_id The region to filter by.
#' @param con Optional DBI connection.
#'
#' @return A tibble of objects within the region.
#'
#' @export
ont_filter_by_region <- function(object_type, region_id, con = NULL) {
    con <- con %||% ont_get_connection()

    # Get object metadata and geometry
    obj_meta <- ont_get_object(object_type, con)
    geom <- ont_get_geometry(object_type, con)

    if (is.null(geom) || geom$geometry_type != "point") {
        cli::cli_abort("Region filtering requires point geometry.")
    }

    # Get region
    region <- DBI::dbGetQuery(con,
        "SELECT * FROM ont_spatial_regions WHERE region_id = ?",
        params = list(region_id)
    )

    if (nrow(region) == 0) {
        cli::cli_abort("Region {.val {region_id}} not found.")
    }

    region <- region[1, ]

    # Build filter based on region type
    if (region$region_type == "bbox") {
        filter <- glue::glue("
            {geom$lon_column} >= {region$bbox_west} AND
            {geom$lon_column} <= {region$bbox_east} AND
            {geom$lat_column} >= {region$bbox_south} AND
            {geom$lat_column} <= {region$bbox_north}
        ")
    } else {
        cli::cli_abort("Only bbox regions are currently supported for filtering.")
    }

    query <- glue::glue("SELECT * FROM {obj_meta$table_name} WHERE {filter}")
    result <- DBI::dbGetQuery(con, query)

    tibble::as_tibble(result)
}

#' List Spatial Exports
#'
#' Lists recorded spatial exports.
#'
#' @param object_type Optional filter by object type.
#' @param export_type Optional filter by export type.
#' @param limit Maximum number of records.
#' @param con Optional DBI connection.
#'
#' @return A tibble of export records.
#'
#' @export
ont_list_spatial_exports <- function(object_type = NULL,
                                      export_type = NULL,
                                      limit = 100,
                                      con = NULL) {
    con <- con %||% ont_get_connection()

    query <- "SELECT * FROM ont_spatial_exports WHERE 1=1"
    params <- list()

    if (!is.null(object_type)) {
        query <- paste(query, "AND object_type = ?")
        params <- c(params, object_type)
    }

    if (!is.null(export_type)) {
        query <- paste(query, "AND export_type = ?")
        params <- c(params, export_type)
    }

    query <- paste(query, "ORDER BY exported_at DESC LIMIT ?")
    params <- c(params, limit)

    result <- DBI::dbGetQuery(con, query, params = params)
    tibble::as_tibble(result)
}
