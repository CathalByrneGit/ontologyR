# =============================================================================
# ontologyR Spatial Viewer
# =============================================================================
# Interactive map viewer using CesiumJS to visualize ontology data spatially.
# Objects are colored by concept evaluations or composite scores.
# =============================================================================

library(shiny)
library(bslib)
library(DT)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_sidebar(
    title = "ontologyR Spatial Viewer",
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
            colourInput("color_true", "Color (TRUE/High)", value = "#FF4444"),
            colourInput("color_false", "Color (FALSE/Low)", value = "#44FF44"),

            hr(),

            actionButton("refresh_map", "Refresh Map",
                         class = "btn-primary w-100")
        ),

        # Region filter
        card(
            card_header("Region Filter"),
            selectInput("region_id", "Filter by Region",
                        choices = c("(All)" = ""), selected = ""),
            textOutput("region_info")
        )
    ),

    # Main content - Cesium map
    layout_columns(
        col_widths = c(8, 4),

        # Map container
        card(
            card_header("Map View"),
            card_body(
                class = "p-0",
                style = "height: 600px;",
                htmlOutput("cesium_viewer")
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
        card_header("Data"),
        card_body(
            DTOutput("data_table")
        )
    )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

    # Get database path from environment or use default
    db_path <- Sys.getenv("ONTOLOGYR_DB", "")
    if (db_path == "") {
        db_path <- ":memory:"
    }

    # Connect to database
    tryCatch({
        ontologyR::ont_connect(db_path)
    }, error = function(e) {
        showNotification(paste("Database error:", e$message), type = "error")
    })

    # Cleanup on session end
    onSessionEnded(function() {
        tryCatch(ontologyR::ont_disconnect(), error = function(e) NULL)
    })

    # --- Reactive values ---
    rv <- reactiveValues(
        geojson = NULL,
        selected_feature = NULL
    )

    # --- Database status ---
    output$db_status <- renderText({
        if (db_path == ":memory:") {
            "In-memory database"
        } else {
            paste("Connected:", basename(db_path))
        }
    })

    # --- Geometry count ---
    output$geometry_count <- renderText({
        con <- ontologyR::ont_get_connection()
        count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM ont_object_geometry")$n
        paste(count, "object types with geometry")
    })

    # --- Populate object type dropdown ---
    observe({
        con <- ontologyR::ont_get_connection()
        geom_types <- DBI::dbGetQuery(con,
            "SELECT og.object_type, ot.description
             FROM ont_object_geometry og
             JOIN ont_object_types ot ON og.object_type = ot.object_type"
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

        concepts <- DBI::dbGetQuery(con,
            "SELECT concept_id FROM ont_concepts WHERE object_type = ?",
            params = list(input$object_type)
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

        scopes <- DBI::dbGetQuery(con,
            "SELECT DISTINCT scope FROM ont_concept_versions WHERE concept_id = ?",
            params = list(input$concept_id)
        )

        if (nrow(scopes) > 0) {
            updateSelectInput(session, "scope", choices = scopes$scope)
        }
    })

    # --- Populate score dropdown ---
    observe({
        req(input$object_type)
        con <- ontologyR::ont_get_connection()

        scores <- DBI::dbGetQuery(con,
            "SELECT score_id, score_name FROM ont_scores WHERE object_type = ? AND enabled = TRUE",
            params = list(input$object_type)
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
        regions <- DBI::dbGetQuery(con, "SELECT region_id, region_name FROM ont_spatial_regions")

        choices <- c("(All)" = "")
        if (nrow(regions) > 0) {
            choices <- c(choices, setNames(regions$region_id, regions$region_name))
        }
        updateSelectInput(session, "region_id", choices = choices)
    })

    # --- Generate GeoJSON when refresh is clicked ---
    geojson_data <- eventReactive(input$refresh_map, {
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
            color_scale = c(input$color_false, "#FFFF00", input$color_true)
        )

        tryCatch({
            ontologyR::ont_export_geojson(
                object_type = input$object_type,
                concept_id = concept_id,
                scope = scope,
                score_id = score_id,
                style = style
            )
        }, error = function(e) {
            showNotification(paste("Export error:", e$message), type = "error")
            NULL
        })
    }, ignoreNULL = FALSE)

    # --- Render Cesium viewer ---
    output$cesium_viewer <- renderUI({
        geojson <- geojson_data()

        # Convert GeoJSON to JavaScript-friendly format
        geojson_js <- if (!is.null(geojson)) {
            jsonlite::toJSON(geojson, auto_unbox = TRUE)
        } else {
            "null"
        }

        # Cesium Ion access token (users should set their own)
        cesium_token <- Sys.getenv("CESIUM_ION_TOKEN", "")

        tags$div(
            style = "width: 100%; height: 600px;",
            id = "cesiumContainer",

            # Include Cesium
            tags$script(src = "https://cesium.com/downloads/cesiumjs/releases/1.114/Build/Cesium/Cesium.js"),
            tags$link(href = "https://cesium.com/downloads/cesiumjs/releases/1.114/Build/Cesium/Widgets/widgets.css",
                      rel = "stylesheet"),

            # Initialize Cesium viewer
            tags$script(HTML(sprintf('
                document.addEventListener("DOMContentLoaded", function() {
                    // Set access token if provided
                    var token = "%s";
                    if (token) {
                        Cesium.Ion.defaultAccessToken = token;
                    }

                    // Initialize viewer
                    var viewer = new Cesium.Viewer("cesiumContainer", {
                        terrain: Cesium.Terrain.fromWorldTerrain(),
                        baseLayerPicker: true,
                        geocoder: true,
                        homeButton: true,
                        sceneModePicker: true,
                        navigationHelpButton: false,
                        animation: false,
                        timeline: false
                    });

                    // Load GeoJSON data
                    var geojsonData = %s;

                    if (geojsonData && geojsonData.features && geojsonData.features.length > 0) {
                        // Add GeoJSON as data source
                        Cesium.GeoJsonDataSource.load(geojsonData, {
                            stroke: Cesium.Color.BLACK,
                            fill: Cesium.Color.YELLOW.withAlpha(0.7),
                            strokeWidth: 2,
                            markerSize: 20,
                            clampToGround: true
                        }).then(function(dataSource) {
                            viewer.dataSources.add(dataSource);

                            // Style entities based on properties
                            var entities = dataSource.entities.values;
                            for (var i = 0; i < entities.length; i++) {
                                var entity = entities[i];
                                var props = entity.properties;

                                // Get marker color from properties
                                var color = Cesium.Color.YELLOW;
                                if (props && props["marker-color"]) {
                                    var hexColor = props["marker-color"].getValue();
                                    if (hexColor) {
                                        color = Cesium.Color.fromCssColorString(hexColor);
                                    }
                                }

                                // Style as point
                                if (entity.point) {
                                    entity.point.color = color;
                                    entity.point.pixelSize = 12;
                                    entity.point.outlineColor = Cesium.Color.BLACK;
                                    entity.point.outlineWidth = 2;
                                } else if (entity.billboard) {
                                    entity.billboard = undefined;
                                    entity.point = new Cesium.PointGraphics({
                                        color: color,
                                        pixelSize: 12,
                                        outlineColor: Cesium.Color.BLACK,
                                        outlineWidth: 2
                                    });
                                }
                            }

                            // Fly to data extent
                            viewer.flyTo(dataSource);
                        });

                        // Handle click events
                        var handler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);
                        handler.setInputAction(function(click) {
                            var pickedObject = viewer.scene.pick(click.position);
                            if (Cesium.defined(pickedObject) && pickedObject.id) {
                                var entity = pickedObject.id;
                                var props = entity.properties;
                                if (props) {
                                    var info = {};
                                    var names = props.propertyNames;
                                    for (var i = 0; i < names.length; i++) {
                                        info[names[i]] = props[names[i]].getValue();
                                    }
                                    // Send to Shiny
                                    Shiny.setInputValue("selected_feature", JSON.stringify(info));
                                }
                            }
                        }, Cesium.ScreenSpaceEventType.LEFT_CLICK);
                    } else {
                        // No data - show world view
                        console.log("No GeoJSON data to display");
                    }
                });
            ', cesium_token, geojson_js)))
        )
    })

    # --- Feature info panel ---
    output$feature_info <- renderUI({
        req(input$selected_feature)

        feature <- tryCatch(
            jsonlite::fromJSON(input$selected_feature),
            error = function(e) NULL
        )

        if (is.null(feature)) {
            return(tags$p("Click a feature on the map to see details"))
        }

        # Build info display
        info_items <- lapply(names(feature), function(name) {
            if (name != "marker-color") {
                tags$tr(
                    tags$td(tags$strong(name)),
                    tags$td(as.character(feature[[name]]))
                )
            }
        })

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
                    style = paste0("display: inline-block; width: 20px; height: 20px; background-color: ",
                                   input$color_true, "; margin-right: 8px; border: 1px solid black;")
                ),
                "TRUE (matches concept)",
                tags$br(),
                tags$div(
                    style = paste0("display: inline-block; width: 20px; height: 20px; background-color: ",
                                   input$color_false, "; margin-right: 8px; border: 1px solid black;")
                ),
                "FALSE (does not match)"
            )
        } else if (input$color_by == "score") {
            tags$div(
                tags$div(
                    style = paste0("display: inline-block; width: 100px; height: 20px; ",
                                   "background: linear-gradient(to right, ",
                                   input$color_false, ", #FFFF00, ", input$color_true, "); ",
                                   "border: 1px solid black;")
                ),
                tags$br(),
                tags$small("Low", style = "float: left;"),
                tags$small("High", style = "float: right;")
            )
        } else {
            tags$p("Select a concept or score to see legend")
        }
    })

    # --- Data table ---
    output$data_table <- renderDT({
        geojson <- geojson_data()
        if (is.null(geojson) || length(geojson$features) == 0) {
            return(data.frame(Message = "No data loaded"))
        }

        # Extract properties from features
        props <- lapply(geojson$features, function(f) f$properties)
        df <- do.call(rbind, lapply(props, function(p) {
            p[sapply(p, is.null)] <- NA
            as.data.frame(p, stringsAsFactors = FALSE)
        }))

        # Remove marker-color column
        df$`marker-color` <- NULL

        df
    }, options = list(pageLength = 10, scrollX = TRUE))

    # --- Region info ---
    output$region_info <- renderText({
        if (is.null(input$region_id) || input$region_id == "") {
            return("Showing all objects")
        }

        con <- ontologyR::ont_get_connection()
        region <- DBI::dbGetQuery(con,
            "SELECT * FROM ont_spatial_regions WHERE region_id = ?",
            params = list(input$region_id)
        )

        if (nrow(region) > 0) {
            paste("Filtering by:", region$region_name[1])
        } else {
            "Region not found"
        }
    })
}

# colourInput polyfill if colourpicker not installed
if (!requireNamespace("colourpicker", quietly = TRUE)) {
    colourInput <- function(inputId, label, value = "#000000", ...) {
        shiny::textInput(inputId, label, value = value, ...)
    }
}

# Run the app
shinyApp(ui = ui, server = server)
