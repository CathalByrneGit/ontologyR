# =============================================================================
# Lineage Visualizer Shiny App
# =============================================================================
# Interactive visualization of data lineage, transforms, and datasets
# =============================================================================

library(shiny)
library(bslib)
library(visNetwork)
library(dplyr)
library(tibble)
library(DT)

# =============================================================================
# UI
# =============================================================================

# Get db_path from option if set by launcher
initial_db_path <- getOption("ontologyR.shiny.db_path", "ontology.duckdb")

ui <- page_sidebar(
    title = "ontologyR Lineage Visualizer",
    theme = bs_theme(version = 5, bootswatch = "flatly"),

    sidebar = sidebar(
        width = 300,
        title = "Controls",

        # Connection
        card(
            card_header("Database Connection"),
            textInput("db_path", "Database Path", value = initial_db_path),
            actionButton("connect_btn", "Connect", class = "btn-primary w-100"),
            textOutput("connection_status")
        ),

        # View options
        card(
            card_header("View Options"),
            selectInput("view_type", "View",
                choices = c(
                    "Full DAG" = "full",
                    "Upstream from selected" = "upstream",
                    "Downstream from selected" = "downstream"
                ),
                selected = "full"
            ),
            numericInput("depth", "Traversal Depth", value = 5, min = 1, max = 20),
            actionButton("refresh_btn", "Refresh Graph", class = "btn-secondary w-100")
        ),

        # Selected node info
        card(
            card_header("Selected Node"),
            uiOutput("selected_node_info")
        )
    ),

    # Main content
    navset_card_tab(
        nav_panel(
            "Lineage Graph",
            visNetworkOutput("lineage_graph", height = "600px")
        ),
        nav_panel(
            "Datasets",
            DTOutput("datasets_table")
        ),
        nav_panel(
            "Transforms",
            DTOutput("transforms_table")
        ),
        nav_panel(
            "Runs",
            DTOutput("runs_table")
        ),
        nav_panel(
            "Impact Analysis",
            card(
                card_header("Impact Analysis"),
                selectInput("impact_dataset", "Select Dataset", choices = NULL),
                actionButton("analyze_impact", "Analyze Impact", class = "btn-warning"),
                hr(),
                verbatimTextOutput("impact_results")
            )
        )
    )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
        connected = FALSE,
        con = NULL,
        selected_node = NULL,
        datasets = NULL,
        transforms = NULL,
        graph_data = NULL
    )

    # Connect to database
    observeEvent(input$connect_btn, {
        tryCatch({
            if (!is.null(rv$con)) {
                DBI::dbDisconnect(rv$con)
            }

            rv$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = input$db_path, read_only = TRUE)
            rv$connected <- TRUE

            # Load data
            rv$datasets <- DBI::dbGetQuery(rv$con, "SELECT * FROM ont_datasets") |> as_tibble()
            rv$transforms <- tryCatch(
                DBI::dbGetQuery(rv$con, "SELECT * FROM ont_transforms") |> as_tibble(),
                error = function(e) tibble()
            )

            # Update dataset selector
            if (nrow(rv$datasets) > 0) {
                updateSelectInput(session, "impact_dataset",
                    choices = setNames(rv$datasets$dataset_id, rv$datasets$dataset_name))
            }

            showNotification("Connected successfully!", type = "message")
        }, error = function(e) {
            rv$connected <- FALSE
            showNotification(paste("Connection failed:", e$message), type = "error")
        })
    })

    # Connection status
    output$connection_status <- renderText({
        if (rv$connected) {
            paste0("Connected (", nrow(rv$datasets), " datasets)")
        } else {
            "Not connected"
        }
    })

    # Build graph data
    build_graph <- reactive({
        req(rv$connected)

        # Get datasets
        datasets <- rv$datasets
        if (is.null(datasets) || nrow(datasets) == 0) {
            return(list(nodes = tibble(), edges = tibble()))
        }

        # Get transforms
        transforms <- rv$transforms

        # Get lineage edges
        edges_raw <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_lineage_edges") |> as_tibble(),
            error = function(e) tibble()
        )

        # Get transform inputs
        transform_inputs <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_transform_inputs") |> as_tibble(),
            error = function(e) tibble()
        )

        # Build nodes
        dataset_nodes <- datasets |>
            transmute(
                id = dataset_id,
                label = dataset_name,
                title = paste0(
                    "<b>", dataset_name, "</b><br>",
                    "Type: ", dataset_type, "<br>",
                    "Table: ", physical_name, "<br>",
                    "Rows: ", ifelse(is.na(row_count), "?", format(row_count, big.mark = ",")), "<br>",
                    ifelse(!is.na(source_concept_id),
                           paste0("Concept: ", source_concept_id, "@", source_scope), "")
                ),
                group = dataset_type,
                shape = case_when(
                    dataset_type == "source" ~ "database",
                    dataset_type == "materialized" ~ "diamond",
                    dataset_type == "derived" ~ "box",
                    TRUE ~ "ellipse"
                )
            )

        transform_nodes <- tibble()
        if (!is.null(transforms) && nrow(transforms) > 0) {
            transform_nodes <- transforms |>
                transmute(
                    id = transform_id,
                    label = transform_name,
                    title = paste0(
                        "<b>", transform_name, "</b><br>",
                        "Type: ", transform_type, "<br>",
                        "Output: ", output_dataset_id
                    ),
                    group = "transform",
                    shape = "square"
                )
        }

        nodes <- bind_rows(dataset_nodes, transform_nodes)

        # Build edges
        edges <- tibble()

        # Lineage edges (dataset to dataset via run)
        if (nrow(edges_raw) > 0) {
            lineage_edges <- edges_raw |>
                transmute(
                    from = from_dataset_id,
                    to = to_dataset_id,
                    label = edge_type,
                    arrows = "to",
                    color = case_when(
                        edge_type == "materialization" ~ "#28a745",
                        edge_type == "primary" ~ "#007bff",
                        edge_type == "join" ~ "#ffc107",
                        TRUE ~ "#6c757d"
                    ),
                    dashes = FALSE
                )
            edges <- bind_rows(edges, lineage_edges)
        }

        # Transform input edges
        if (nrow(transform_inputs) > 0) {
            input_edges <- transform_inputs |>
                transmute(
                    from = input_dataset_id,
                    to = transform_id,
                    label = input_role,
                    arrows = "to",
                    color = "#17a2b8",
                    dashes = TRUE
                )
            edges <- bind_rows(edges, input_edges)
        }

        # Transform output edges
        if (!is.null(transforms) && nrow(transforms) > 0) {
            output_edges <- transforms |>
                transmute(
                    from = transform_id,
                    to = output_dataset_id,
                    label = "output",
                    arrows = "to",
                    color = "#28a745",
                    dashes = TRUE
                )
            edges <- bind_rows(edges, output_edges)
        }

        list(nodes = nodes, edges = edges)
    })

    # Filter graph based on selection
    filtered_graph <- reactive({
        graph <- build_graph()
        req(nrow(graph$nodes) > 0)

        if (input$view_type == "full" || is.null(rv$selected_node)) {
            return(graph)
        }

        selected <- rv$selected_node

        if (input$view_type == "upstream") {
            # Get upstream nodes
            upstream_ids <- get_connected_nodes(graph$edges, selected, "upstream", input$depth)
            upstream_ids <- c(selected, upstream_ids)

            nodes <- graph$nodes |> filter(id %in% upstream_ids)
            edges <- graph$edges |> filter(from %in% upstream_ids & to %in% upstream_ids)

        } else if (input$view_type == "downstream") {
            # Get downstream nodes
            downstream_ids <- get_connected_nodes(graph$edges, selected, "downstream", input$depth)
            downstream_ids <- c(selected, downstream_ids)

            nodes <- graph$nodes |> filter(id %in% downstream_ids)
            edges <- graph$edges |> filter(from %in% downstream_ids & to %in% downstream_ids)
        }

        list(nodes = nodes, edges = edges)
    })

    # Helper function to get connected nodes
    get_connected_nodes <- function(edges, start_node, direction = "upstream", max_depth = 5) {
        if (nrow(edges) == 0) return(character())

        visited <- character()
        current <- start_node
        depth <- 0

        while (length(current) > 0 && depth < max_depth) {
            depth <- depth + 1

            if (direction == "upstream") {
                next_nodes <- edges |>
                    filter(to %in% current & !(from %in% visited)) |>
                    pull(from) |>
                    unique()
            } else {
                next_nodes <- edges |>
                    filter(from %in% current & !(to %in% visited)) |>
                    pull(to) |>
                    unique()
            }

            visited <- c(visited, current)
            current <- setdiff(next_nodes, visited)
        }

        unique(visited)
    }

    # Render lineage graph
    output$lineage_graph <- renderVisNetwork({
        graph <- filtered_graph()
        req(nrow(graph$nodes) > 0)

        visNetwork(graph$nodes, graph$edges) |>
            visOptions(
                highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
                nodesIdSelection = TRUE,
                selectedBy = "group"
            ) |>
            visGroups(groupname = "source", color = "#6c757d", shape = "database") |>
            visGroups(groupname = "materialized", color = "#28a745", shape = "diamond") |>
            visGroups(groupname = "derived", color = "#007bff", shape = "box") |>
            visGroups(groupname = "transform", color = "#ffc107", shape = "square") |>
            visEdges(smooth = list(type = "cubicBezier")) |>
            visLayout(randomSeed = 42) |>
            visPhysics(
                solver = "forceAtlas2Based",
                forceAtlas2Based = list(gravitationalConstant = -50)
            ) |>
            visInteraction(navigationButtons = TRUE, keyboard = TRUE) |>
            visLegend(width = 0.1, position = "right") |>
            visEvents(selectNode = "function(nodes) {
                Shiny.setInputValue('selected_node_id', nodes.nodes[0]);
            }")
    })

    # Handle node selection
    observeEvent(input$selected_node_id, {
        rv$selected_node <- input$selected_node_id
    })

    # Refresh graph
    observeEvent(input$refresh_btn, {
        if (rv$connected) {
            rv$datasets <- DBI::dbGetQuery(rv$con, "SELECT * FROM ont_datasets") |> as_tibble()
            rv$transforms <- tryCatch(
                DBI::dbGetQuery(rv$con, "SELECT * FROM ont_transforms") |> as_tibble(),
                error = function(e) tibble()
            )
        }
    })

    # Selected node info
    output$selected_node_info <- renderUI({
        if (is.null(rv$selected_node) || !rv$connected) {
            return(p("Click a node to see details", class = "text-muted"))
        }

        node_id <- rv$selected_node

        # Check if it's a dataset
        ds <- rv$datasets |> filter(dataset_id == node_id)
        if (nrow(ds) > 0) {
            ds <- ds[1, ]
            return(tagList(
                h6(ds$dataset_name),
                tags$dl(
                    tags$dt("ID"), tags$dd(ds$dataset_id),
                    tags$dt("Type"), tags$dd(ds$dataset_type),
                    tags$dt("Table"), tags$dd(ds$physical_name),
                    tags$dt("Rows"), tags$dd(format(ds$row_count, big.mark = ",")),
                    if (!is.na(ds$source_concept_id)) {
                        tagList(
                            tags$dt("Concept"), tags$dd(paste0(ds$source_concept_id, "@", ds$source_scope))
                        )
                    }
                )
            ))
        }

        # Check if it's a transform
        tf <- rv$transforms |> filter(transform_id == node_id)
        if (nrow(tf) > 0) {
            tf <- tf[1, ]
            return(tagList(
                h6(tf$transform_name),
                tags$dl(
                    tags$dt("ID"), tags$dd(tf$transform_id),
                    tags$dt("Type"), tags$dd(tf$transform_type),
                    tags$dt("Output"), tags$dd(tf$output_dataset_id)
                ),
                if (!is.na(tf$code)) {
                    tagList(
                        tags$dt("Code"),
                        tags$pre(style = "font-size: 10px; max-height: 150px; overflow: auto;",
                                 tf$code)
                    )
                }
            ))
        }

        p("Node not found", class = "text-muted")
    })

    # Datasets table
    output$datasets_table <- renderDT({
        req(rv$connected)
        rv$datasets |>
            select(dataset_id, dataset_name, dataset_type, physical_name, row_count, owner) |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                selection = "single",
                rownames = FALSE
            )
    })

    # Transforms table
    output$transforms_table <- renderDT({
        req(rv$connected)
        rv$transforms |>
            select(transform_id, transform_name, transform_type, output_dataset_id) |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                selection = "single",
                rownames = FALSE
            )
    })

    # Runs table
    output$runs_table <- renderDT({
        req(rv$connected)

        runs <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_runs ORDER BY started_at DESC LIMIT 100") |>
                as_tibble(),
            error = function(e) tibble()
        )

        if (nrow(runs) == 0) {
            return(datatable(tibble(message = "No runs found")))
        }

        runs |>
            select(run_id, run_type, status, started_at, output_row_count, concept_id, triggered_by) |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                selection = "single",
                rownames = FALSE
            )
    })

    # Impact analysis
    observeEvent(input$analyze_impact, {
        req(rv$connected, input$impact_dataset)

        output$impact_results <- renderPrint({
            ds_id <- input$impact_dataset

            cat("=== Impact Analysis for", ds_id, "===\n\n")

            # Get downstream datasets
            edges <- tryCatch(
                DBI::dbGetQuery(rv$con, "SELECT * FROM ont_lineage_edges") |> as_tibble(),
                error = function(e) tibble()
            )

            if (nrow(edges) == 0) {
                cat("No lineage edges found.\n")
                return()
            }

            # Find downstream
            downstream <- get_connected_nodes(edges, ds_id, "downstream", 10)

            if (length(downstream) == 0) {
                cat("No downstream dependencies found.\n")
            } else {
                cat("Downstream Datasets (", length(downstream), "):\n", sep = "")
                for (d in downstream) {
                    ds_info <- rv$datasets |> filter(dataset_id == d)
                    if (nrow(ds_info) > 0) {
                        cat("  -", ds_info$dataset_name[1], "(", d, ")\n")
                    } else {
                        cat("  -", d, "\n")
                    }
                }
            }

            # Find upstream
            upstream <- get_connected_nodes(edges, ds_id, "upstream", 10)

            cat("\nUpstream Sources (", length(upstream), "):\n", sep = "")
            if (length(upstream) == 0) {
                cat("  (This is a source dataset)\n")
            } else {
                for (u in upstream) {
                    ds_info <- rv$datasets |> filter(dataset_id == u)
                    if (nrow(ds_info) > 0) {
                        cat("  -", ds_info$dataset_name[1], "(", u, ")\n")
                    } else {
                        cat("  -", u, "\n")
                    }
                }
            }

            # Find transforms that use this dataset
            transform_inputs <- tryCatch(
                DBI::dbGetQuery(rv$con,
                    "SELECT * FROM ont_transform_inputs WHERE input_dataset_id = ?",
                    params = list(ds_id)) |> as_tibble(),
                error = function(e) tibble()
            )

            if (nrow(transform_inputs) > 0) {
                cat("\nTransforms using this dataset (", nrow(transform_inputs), "):\n", sep = "")
                for (i in seq_len(nrow(transform_inputs))) {
                    tf_id <- transform_inputs$transform_id[i]
                    tf_info <- rv$transforms |> filter(transform_id == tf_id)
                    if (nrow(tf_info) > 0) {
                        cat("  -", tf_info$transform_name[1], "(", tf_id, ") as", transform_inputs$input_role[i], "\n")
                    }
                }
            }
        })
    })

    # Cleanup on session end
    onSessionEnded(function() {
        if (!is.null(rv$con)) {
            try(DBI::dbDisconnect(rv$con), silent = TRUE)
        }
    })
}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui = ui, server = server)
