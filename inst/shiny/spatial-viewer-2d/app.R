# =============================================================================
# ontologyR 2D Spatial Viewer (Leaflet)
# =============================================================================
# Interactive 2D map viewer using Leaflet to visualize ontology data spatially.
# A lighter alternative to the CesiumJS 3D viewer - no WebGL or tokens required.
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(leaflet)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_sidebar(
    title = "ontologyR Spatial Viewer (2D)",
    theme = bs_theme(version = 5, bootswatch = "flatly"),

    sidebar = sidebar(
        width = 300,

        # Connection info
        card(
            card_header("Database"),
            textOutput("db_status"),
            hr(),
            textOutput("geometry_count")
        ),

        # Layer configuration
        card(
            card_header("Layer Settings"),

            selectInput("object_type", "Object Type",
                        choices = NULL, selected = NULL),

            selectInput("color_by", "Color By",
                        choices = c("None" = "none",
                                    "Concept" = "concept",
                                    "Score" = "score"),
                        selected = "none"),

            conditionalPanel(
                condition = "input.color_by == 'concept'",
                selectInput("concept_id", "Concept", choices = NULL),
                selectInput("scope", "Scope", choices = NULL)
            ),

            conditionalPanel(
                condition = "input.color_by == 'score'",
                selectInput("score_id", "Score", choices = NULL)
            ),

            hr(),

            # Style options
            textInput("color_true", "Color (TRUE/High)", value = "#e74c3c"),
            textInput("color_false", "Color (FALSE/Low)", value = "#27ae60"),

            sliderInput("point_radius", "Point Size", min = 3, max = 20, value = 8),
            sliderInput("opacity", "Opacity", min = 0.1, max = 1, value = 0.8, step = 0.1),

            hr(),

            actionButton("refresh_map", "Load Data",
                         class = "btn-primary w-100")
        ),

        # Region filter
        card(
            card_header("Region Filter"),
            selectInput("region_id", "Filter by Region",
                        choices = c("(All)" = ""), selected = ""),
            textOutput("region_info")
        ),

        # Base map options
        card(
            card_header("Map Options"),
            selectInput("basemap", "Base Map",
                        choices = c(
                            "OpenStreetMap" = "OpenStreetMap",
                            "CartoDB Positron" = "CartoDB.Positron",
                            "CartoDB Dark" = "CartoDB.DarkMatter",
                            "Esri WorldImagery" = "Esri.WorldImagery",
                            "Esri Topo" = "Esri.WorldTopoMap"
                        ),
                        selected = "CartoDB.Positron"),
            checkboxInput("show_clusters", "Cluster points", value = FALSE)
        )
    ),

    # Main content
    layout_columns(
        col_widths = c(8, 4),

        # Map container
        card(
            card_header(
                class = "d-flex justify-content-between align-items-center",
                "Map View",
                span(textOutput("point_count", inline = TRUE), class = "badge bg-secondary")
            ),
            card_body(
                class = "p-0",
                style = "height: 550px;",
                leafletOutput("map", height = "100%")
            )
        ),

        # Info panel
        card(
            card_header("Feature Details"),
            card_body(
                uiOutput("feature_info"),
                hr(),
                h6("Legend"),
                uiOutput("legend")
            )
        )
    ),

    # Data table
    card(
        card_header(
            class = "d-flex justify-content-between align-items-center",
            "Data Table",
            downloadButton("download_geojson", "Export GeoJSON", class = "btn-sm btn-outline-primary")
        ),
        card_body(
            DTOutput("data_table")
        )
    )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

    # Check if already connected (e.g., from an example script)
    existing_con <- tryCatch(
        ontologyR::ont_get_connection(),
        error = function(e) NULL
    )

    # Track if we created the connection (so we know to clean it up)
    created_connection <- FALSE

    if (is.null(existing_con)) {
        # No existing connection - create one
        db_path <- Sys.getenv("ONTOLOGYR_DB", "")
        if (db_path == "") {
            db_path <- getOption("ontologyR.shiny.db_path", ":memory:")
        }

        tryCatch({
            ontologyR::ont_connect(db_path)
            created_connection <- TRUE
        }, error = function(e) {
            showNotification(paste("Database error:", e$message), type = "error")
        })
    }

    # Get the active connection path for display
    db_path <- tryCatch({
        con <- ontologyR::ont_get_connection()
        if (inherits(con, "duckdb_connection")) {
            dbinfo <- DBI::dbGetInfo(con)
            dbinfo$dbname
        } else {
            "connected"
        }
    }, error = function(e) "unknown")

    # Cleanup on session end - only if we created the connection
    onSessionEnded(function() {
        if (created_connection) {
            tryCatch(ontologyR::ont_disconnect(), error = function(e) NULL)
        }
    })

    # --- Reactive values ---
    rv <- reactiveValues(
        geojson = NULL,
        map_data = NULL,
        selected_feature = NULL
    )

    # --- Database status ---
    output$db_status <- renderText({
        tryCatch({
            con <- ontologyR::ont_get_connection()
            if (inherits(con, "duckdb_connection")) {
                dbinfo <- DBI::dbGetInfo(con)
                if (dbinfo$dbname == ":memory:") {
                    "In-memory database"
                } else {
                    paste("Connected:", basename(dbinfo$dbname))
                }
            } else {
                "Connected"
            }
        }, error = function(e) "Not connected")
    })

    # --- Geometry count ---
    output$geometry_count <- renderText({
        con <- ontologyR::ont_get_connection()
        count <- tryCatch(
            DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM ont_object_geometry")$n,
            error = function(e) 0
        )
        paste(count, "object types with geometry")
    })

    # --- Populate object type dropdown ---
    observe({
        con <- ontologyR::ont_get_connection()
        geom_types <- tryCatch(
            DBI::dbGetQuery(con,
                "SELECT og.object_type, ot.description
                 FROM ont_object_geometry og
                 JOIN ont_object_types ot ON og.object_type = ot.object_type"
            ),
            error = function(e) data.frame()
        )

        if (nrow(geom_types) > 0) {
            choices <- setNames(geom_types$object_type,
                                paste(geom_types$object_type,
                                      ifelse(is.na(geom_types$description), "",
                                             paste0(" - ", geom_types$description))))
            updateSelectInput(session, "object_type", choices = choices)
        }
    })

    # --- Populate concept dropdown based on selected object type ---
    observe({
        req(input$object_type)
        con <- ontologyR::ont_get_connection()

        concepts <- tryCatch(
            DBI::dbGetQuery(con,
                "SELECT concept_id FROM ont_concepts WHERE object_type = ?",
                params = list(input$object_type)
            ),
            error = function(e) data.frame()
        )

        if (nrow(concepts) > 0) {
            updateSelectInput(session, "concept_id", choices = concepts$concept_id)
        } else {
            updateSelectInput(session, "concept_id", choices = c("(No concepts)" = ""))
        }
    })

    # --- Populate scope dropdown based on selected concept ---
    observe({
        req(input$concept_id, input$concept_id != "")
        con <- ontologyR::ont_get_connection()

        scopes <- tryCatch(
            DBI::dbGetQuery(con,
                "SELECT DISTINCT scope FROM ont_concept_versions WHERE concept_id = ?",
                params = list(input$concept_id)
            ),
            error = function(e) data.frame()
        )

        if (nrow(scopes) > 0) {
            updateSelectInput(session, "scope", choices = scopes$scope)
        }
    })

    # --- Populate score dropdown ---
    observe({
        req(input$object_type)
        con <- ontologyR::ont_get_connection()

        scores <- tryCatch(
            DBI::dbGetQuery(con,
                "SELECT score_id, score_name FROM ont_scores WHERE object_type = ? AND enabled = TRUE",
                params = list(input$object_type)
            ),
            error = function(e) data.frame()
        )

        if (nrow(scores) > 0) {
            choices <- setNames(scores$score_id, scores$score_name)
            updateSelectInput(session, "score_id", choices = choices)
        } else {
            updateSelectInput(session, "score_id", choices = c("(No scores)" = ""))
        }
    })

    # --- Populate region dropdown ---
    observe({
        con <- ontologyR::ont_get_connection()
        regions <- tryCatch(
            DBI::dbGetQuery(con, "SELECT region_id, region_name FROM ont_spatial_regions"),
            error = function(e) data.frame()
        )

        choices <- c("(All)" = "")
        if (nrow(regions) > 0) {
            choices <- c(choices, setNames(regions$region_id, regions$region_name))
        }
        updateSelectInput(session, "region_id", choices = choices)
    })

    # --- Load data when refresh is clicked ---
    observeEvent(input$refresh_map, {
        req(input$object_type)

        concept_id <- if (input$color_by == "concept" && !is.null(input$concept_id) && input$concept_id != "") {
            input$concept_id
        } else {
            NULL
        }

        scope <- if (!is.null(concept_id)) input$scope else NULL

        score_id <- if (input$color_by == "score" && !is.null(input$score_id) && input$score_id != "") {
            input$score_id
        } else {
            NULL
        }

        style <- list(
            color_true = input$color_true,
            color_false = input$color_false,
            color_scale = c(input$color_false, "#f1c40f", input$color_true)
        )

        tryCatch({
            geojson <- ontologyR::ont_export_geojson(
                object_type = input$object_type,
                concept_id = concept_id,
                scope = scope,
                score_id = score_id,
                style = style
            )
            rv$geojson <- geojson

            # Convert to data frame for map
            if (!is.null(geojson) && length(geojson$features) > 0) {
                rv$map_data <- geojson_to_df(geojson)
            } else {
                rv$map_data <- NULL
            }

            showNotification("Data loaded successfully", type = "message")
        }, error = function(e) {
            showNotification(paste("Export error:", e$message), type = "error")
            rv$geojson <- NULL
            rv$map_data <- NULL
        })
    })

    # --- Helper: Convert GeoJSON to data frame ---
    geojson_to_df <- function(geojson) {
        if (is.null(geojson$features) || length(geojson$features) == 0) {
            return(NULL)
        }

        df_list <- lapply(geojson$features, function(f) {
            coords <- f$geometry$coordinates
            props <- f$properties
            props[sapply(props, is.null)] <- NA

            data.frame(
                lon = coords[1],
                lat = coords[2],
                as.data.frame(props, stringsAsFactors = FALSE),
                stringsAsFactors = FALSE
            )
        })

        do.call(rbind, df_list)
    }

    # --- Point count ---
    output$point_count <- renderText({
        if (is.null(rv$map_data)) {
            "0 points"
        } else {
            paste(nrow(rv$map_data), "points")
        }
    })

    # --- Render Leaflet map ---
    output$map <- renderLeaflet({
        # Create base map
        leaflet() %>%
            addProviderTiles(input$basemap) %>%
            setView(lng = 0, lat = 20, zoom = 2)
    })

    # --- Update map with data ---
    observe({
        df <- rv$map_data

        leafletProxy("map") %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearShapes()

        if (is.null(df) || nrow(df) == 0) {
            return()
        }

        # Get colors
        colors <- if ("marker-color" %in% names(df)) {
            df$`marker-color`
        } else {
            rep("#3388ff", nrow(df))
        }

        # Build popup content
        popup_cols <- setdiff(names(df), c("lon", "lat", "marker-color"))
        popups <- apply(df[, popup_cols, drop = FALSE], 1, function(row) {
            items <- paste0("<b>", popup_cols, ":</b> ", row)
            paste(items, collapse = "<br/>")
        })

        # Add to map
        proxy <- leafletProxy("map")

        if (input$show_clusters) {
            proxy %>%
                addCircleMarkers(
                    data = df,
                    lng = ~lon,
                    lat = ~lat,
                    radius = input$point_radius,
                    color = "#333",
                    weight = 1,
                    fillColor = colors,
                    fillOpacity = input$opacity,
                    popup = popups,
                    layerId = df$object_key,
                    clusterOptions = markerClusterOptions()
                )
        } else {
            proxy %>%
                addCircleMarkers(
                    data = df,
                    lng = ~lon,
                    lat = ~lat,
                    radius = input$point_radius,
                    color = "#333",
                    weight = 1,
                    fillColor = colors,
                    fillOpacity = input$opacity,
                    popup = popups,
                    layerId = df$object_key
                )
        }

        # Fit bounds to data
        proxy %>%
            fitBounds(
                lng1 = min(df$lon, na.rm = TRUE),
                lat1 = min(df$lat, na.rm = TRUE),
                lng2 = max(df$lon, na.rm = TRUE),
                lat2 = max(df$lat, na.rm = TRUE)
            )
    })

    # --- Update base map ---
    observeEvent(input$basemap, {
        leafletProxy("map") %>%
            clearTiles() %>%
            addProviderTiles(input$basemap)
    })

    # --- Handle marker click ---
    observeEvent(input$map_marker_click, {
        click <- input$map_marker_click
        if (!is.null(click) && !is.null(rv$map_data)) {
            # Find the clicked feature
            feature <- rv$map_data[rv$map_data$object_key == click$id, ]
            if (nrow(feature) > 0) {
                rv$selected_feature <- as.list(feature[1, ])
            }
        }
    })

    # --- Feature info panel ---
    output$feature_info <- renderUI({
        feature <- rv$selected_feature

        if (is.null(feature)) {
            return(tags$p(class = "text-muted", "Click a point on the map to see details"))
        }

        # Build info display
        info_items <- lapply(names(feature), function(name) {
            if (!name %in% c("marker-color", "lon", "lat")) {
                tags$tr(
                    tags$td(tags$strong(name)),
                    tags$td(as.character(feature[[name]]))
                )
            }
        })
        info_items <- info_items[!sapply(info_items, is.null)]

        tags$table(
            class = "table table-sm table-striped",
            tags$tbody(info_items)
        )
    })

    # --- Legend ---
    output$legend <- renderUI({
        if (input$color_by == "concept") {
            tags$div(
                tags$div(
                    class = "d-flex align-items-center mb-2",
                    tags$div(
                        style = paste0("width: 20px; height: 20px; background-color: ",
                                       input$color_true, "; margin-right: 8px; border: 1px solid #333; border-radius: 50%;")
                    ),
                    "TRUE (matches concept)"
                ),
                tags$div(
                    class = "d-flex align-items-center",
                    tags$div(
                        style = paste0("width: 20px; height: 20px; background-color: ",
                                       input$color_false, "; margin-right: 8px; border: 1px solid #333; border-radius: 50%;")
                    ),
                    "FALSE (does not match)"
                )
            )
        } else if (input$color_by == "score") {
            tags$div(
                tags$div(
                    style = paste0("width: 100%; height: 20px; ",
                                   "background: linear-gradient(to right, ",
                                   input$color_false, ", #f1c40f, ", input$color_true, "); ",
                                   "border: 1px solid #333; border-radius: 4px;")
                ),
                tags$div(
                    class = "d-flex justify-content-between mt-1",
                    tags$small("Low"),
                    tags$small("Medium"),
                    tags$small("High")
                )
            )
        } else {
            tags$p(class = "text-muted", "Select a concept or score to color by")
        }
    })

    # --- Data table ---
    output$data_table <- renderDT({
        df <- rv$map_data

        if (is.null(df) || nrow(df) == 0) {
            return(data.frame(Message = "No data loaded. Select an object type and click 'Load Data'."))
        }

        # Remove internal columns for display
        display_cols <- setdiff(names(df), c("marker-color"))
        df[, display_cols, drop = FALSE]
    }, options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
    ), selection = 'single')

    # --- Handle table row selection ---
    observeEvent(input$data_table_rows_selected, {
        idx <- input$data_table_rows_selected
        if (!is.null(idx) && !is.null(rv$map_data)) {
            feature <- rv$map_data[idx, ]
            rv$selected_feature <- as.list(feature)

            # Pan map to selected point
            leafletProxy("map") %>%
                setView(lng = feature$lon, lat = feature$lat, zoom = 12)
        }
    })

    # --- Region info ---
    output$region_info <- renderText({
        if (is.null(input$region_id) || input$region_id == "") {
            return("Showing all objects")
        }

        con <- ontologyR::ont_get_connection()
        region <- tryCatch(
            DBI::dbGetQuery(con,
                "SELECT * FROM ont_spatial_regions WHERE region_id = ?",
                params = list(input$region_id)
            ),
            error = function(e) data.frame()
        )

        if (nrow(region) > 0) {
            paste("Filtering by:", region$region_name[1])
        } else {
            "Region not found"
        }
    })

    # --- Download GeoJSON ---
    output$download_geojson <- downloadHandler(
        filename = function() {
            paste0(input$object_type, "_", Sys.Date(), ".geojson")
        },
        content = function(file) {
            if (!is.null(rv$geojson)) {
                jsonlite::write_json(rv$geojson, file, auto_unbox = TRUE, pretty = TRUE)
            }
        }
    )
}

# Run the app
shinyApp(ui = ui, server = server)
