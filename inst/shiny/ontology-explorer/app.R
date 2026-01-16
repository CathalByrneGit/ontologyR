# =============================================================================
# Ontology Explorer Shiny App
# =============================================================================
# Interactive exploration of concepts, versions, templates, and relationships
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(DT)

# Get db_path from option if set by launcher
initial_db_path <- getOption("ontologyR.shiny.db_path", "ontology.duckdb")

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
    title = "ontologyR Explorer",
    theme = bs_theme(version = 5, bootswatch = "cosmo"),

    # Concepts Tab
    nav_panel(
        "Concepts",
        layout_sidebar(
            sidebar = sidebar(
                width = 300,
                card(
                    card_header("Database"),
                    textInput("db_path", "Database Path", value = initial_db_path),
                    actionButton("connect_btn", "Connect", class = "btn-primary w-100"),
                    textOutput("connection_status")
                ),
                card(
                    card_header("Filters"),
                    selectInput("filter_object_type", "Object Type",
                        choices = c("All" = ""),
                        selected = ""
                    ),
                    selectInput("filter_status", "Version Status",
                        choices = c("All" = "", "active", "draft", "deprecated", "retired"),
                        selected = ""
                    ),
                    selectInput("filter_scope", "Scope",
                        choices = c("All" = ""),
                        selected = ""
                    )
                )
            ),
            navset_card_tab(
                nav_panel(
                    "Concept List",
                    DTOutput("concepts_table"),
                    hr(),
                    uiOutput("concept_details_panel")
                ),
                nav_panel(
                    "Version Comparison",
                    fluidRow(
                        column(6, selectInput("compare_concept", "Concept", choices = NULL)),
                        column(6, actionButton("compare_btn", "Compare Versions", class = "btn-info"))
                    ),
                    DTOutput("versions_compare_table"),
                    hr(),
                    verbatimTextOutput("sql_diff_output")
                ),
                nav_panel(
                    "Search",
                    textInput("search_sql", "Search SQL Expressions", placeholder = "Enter keyword..."),
                    actionButton("search_btn", "Search", class = "btn-secondary"),
                    DTOutput("search_results")
                )
            )
        )
    ),

    # Templates Tab
    nav_panel(
        "Templates",
        layout_sidebar(
            sidebar = sidebar(
                width = 300,
                card(
                    card_header("Template Filters"),
                    selectInput("template_object_type", "Object Type",
                        choices = c("All" = ""),
                        selected = ""
                    ),
                    selectInput("template_standard", "Source Standard",
                        choices = c("All" = ""),
                        selected = ""
                    )
                )
            ),
            navset_card_tab(
                nav_panel(
                    "Template List",
                    DTOutput("templates_table"),
                    hr(),
                    uiOutput("template_details_panel")
                ),
                nav_panel(
                    "Variant Comparison",
                    selectInput("variant_template", "Select Template", choices = NULL),
                    actionButton("compare_variants_btn", "Compare Variants", class = "btn-info"),
                    DTOutput("variant_comparison_table")
                )
            )
        )
    ),

    # Audits & Drift Tab
    nav_panel(
        "Audits & Drift",
        layout_sidebar(
            sidebar = sidebar(
                width = 300,
                card(
                    card_header("Audit Filters"),
                    selectInput("audit_concept", "Concept",
                        choices = c("All" = ""),
                        selected = ""
                    ),
                    dateRangeInput("audit_date_range", "Date Range",
                        start = Sys.Date() - 30,
                        end = Sys.Date()
                    )
                )
            ),
            navset_card_tab(
                nav_panel(
                    "Audits",
                    DTOutput("audits_table")
                ),
                nav_panel(
                    "Drift Summary",
                    DTOutput("drift_summary_table"),
                    hr(),
                    plotOutput("drift_chart", height = "300px")
                ),
                nav_panel(
                    "Drift Events",
                    DTOutput("drift_events_table")
                )
            )
        )
    ),

    # Object Types Tab
    nav_panel(
        "Object Types",
        card(
            card_header("Registered Object Types"),
            DTOutput("object_types_table")
        ),
        card(
            card_header("Link Types"),
            DTOutput("link_types_table")
        )
    ),

    # Governance Tab
    nav_panel(
        "Governance",
        navset_card_tab(
            nav_panel(
                "Governance Log",
                DTOutput("governance_log_table")
            ),
            nav_panel(
                "Pending Approvals",
                DTOutput("pending_approvals_table")
            ),
            nav_panel(
                "Gates",
                DTOutput("gates_table")
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
        concepts = NULL,
        templates = NULL,
        selected_concept = NULL,
        selected_template = NULL
    )

    # Connect to database
    observeEvent(input$connect_btn, {
        tryCatch({
            if (!is.null(rv$con)) {
                DBI::dbDisconnect(rv$con)
            }

            rv$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = input$db_path, read_only = TRUE)
            rv$connected <- TRUE

            # Load initial data
            refresh_data()

            showNotification("Connected successfully!", type = "message")
        }, error = function(e) {
            rv$connected <- FALSE
            showNotification(paste("Connection failed:", e$message), type = "error")
        })
    })

    # Refresh data from database
    refresh_data <- function() {
        if (!rv$connected) return()

        # Load concepts
        rv$concepts <- tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT c.*, cv.scope, cv.version, cv.status, cv.sql_expr
                FROM ont_concepts c
                LEFT JOIN ont_concept_versions cv ON c.concept_id = cv.concept_id
                ORDER BY c.concept_id, cv.scope, cv.version
            ") |> as_tibble(),
            error = function(e) tibble()
        )

        # Load templates
        rv$templates <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_templates") |> as_tibble(),
            error = function(e) tibble()
        )

        # Update filter dropdowns
        update_filters()
    }

    # Update filter dropdowns
    update_filters <- function() {
        if (!rv$connected) return()

        # Object types
        object_types <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT DISTINCT object_type FROM ont_object_types")$object_type,
            error = function(e) character()
        )
        updateSelectInput(session, "filter_object_type",
            choices = c("All" = "", object_types))
        updateSelectInput(session, "template_object_type",
            choices = c("All" = "", object_types))

        # Scopes
        scopes <- tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT DISTINCT scope FROM ont_concept_versions")$scope,
            error = function(e) character()
        )
        updateSelectInput(session, "filter_scope", choices = c("All" = "", scopes))

        # Concepts for comparison
        concepts <- unique(rv$concepts$concept_id)
        updateSelectInput(session, "compare_concept", choices = concepts)
        updateSelectInput(session, "audit_concept", choices = c("All" = "", concepts))

        # Templates for variant comparison
        if (nrow(rv$templates) > 0) {
            updateSelectInput(session, "variant_template",
                choices = setNames(rv$templates$template_id, rv$templates$template_name))
        }

        # Source standards
        standards <- unique(rv$templates$source_standard[!is.na(rv$templates$source_standard)])
        updateSelectInput(session, "template_standard", choices = c("All" = "", standards))
    }

    # Connection status
    output$connection_status <- renderText({
        if (rv$connected) {
            n_concepts <- length(unique(rv$concepts$concept_id))
            n_templates <- nrow(rv$templates)
            paste0("Connected (", n_concepts, " concepts, ", n_templates, " templates)")
        } else {
            "Not connected"
        }
    })

    # --------------------------------------------------------------------------
    # Concepts Tab
    # --------------------------------------------------------------------------

    # Filtered concepts
    filtered_concepts <- reactive({
        if (!rv$connected || is.null(rv$concepts)) return(tibble())

        df <- rv$concepts

        if (input$filter_object_type != "") {
            df <- df |> filter(object_type == input$filter_object_type)
        }

        if (input$filter_status != "") {
            df <- df |> filter(status == input$filter_status)
        }

        if (input$filter_scope != "") {
            df <- df |> filter(scope == input$filter_scope)
        }

        df
    })

    # Concepts table
    output$concepts_table <- renderDT({
        req(rv$connected)

        filtered_concepts() |>
            select(concept_id, object_type, scope, version, status, description) |>
            distinct() |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                selection = "single",
                rownames = FALSE
            )
    })

    # Handle concept selection
    observeEvent(input$concepts_table_rows_selected, {
        row_idx <- input$concepts_table_rows_selected
        if (length(row_idx) > 0) {
            df <- filtered_concepts() |>
                select(concept_id, object_type, scope, version, status, description) |>
                distinct()
            rv$selected_concept <- df[row_idx, ]
        }
    })

    # Concept details panel
    output$concept_details_panel <- renderUI({
        if (is.null(rv$selected_concept)) {
            return(p("Select a concept to view details", class = "text-muted"))
        }

        concept <- rv$selected_concept

        # Get SQL expression
        sql_expr <- rv$concepts |>
            filter(
                concept_id == concept$concept_id,
                scope == concept$scope,
                version == concept$version
            ) |>
            pull(sql_expr) |>
            first()

        card(
            card_header(paste("Details:", concept$concept_id)),
            card_body(
                tags$dl(class = "row",
                    tags$dt(class = "col-sm-3", "Object Type"),
                    tags$dd(class = "col-sm-9", concept$object_type),
                    tags$dt(class = "col-sm-3", "Scope"),
                    tags$dd(class = "col-sm-9", concept$scope),
                    tags$dt(class = "col-sm-3", "Version"),
                    tags$dd(class = "col-sm-9", concept$version),
                    tags$dt(class = "col-sm-3", "Status"),
                    tags$dd(class = "col-sm-9",
                        span(class = paste0("badge bg-",
                            switch(concept$status,
                                "active" = "success",
                                "draft" = "warning",
                                "deprecated" = "secondary",
                                "retired" = "dark",
                                "info"
                            )), concept$status)
                    ),
                    tags$dt(class = "col-sm-3", "Description"),
                    tags$dd(class = "col-sm-9", concept$description)
                ),
                tags$h6("SQL Expression"),
                tags$pre(style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
                    sql_expr)
            )
        )
    })

    # Version comparison
    output$versions_compare_table <- renderDT({
        req(rv$connected, input$compare_concept)

        rv$concepts |>
            filter(concept_id == input$compare_concept) |>
            select(scope, version, status, sql_expr, created_at) |>
            datatable(
                options = list(pageLength = 10, scrollX = TRUE),
                selection = "multiple",
                rownames = FALSE
            )
    })

    # SQL diff output
    output$sql_diff_output <- renderPrint({
        req(input$versions_compare_table_rows_selected)

        rows <- input$versions_compare_table_rows_selected

        if (length(rows) < 2) {
            cat("Select at least 2 versions to compare")
            return()
        }

        versions <- rv$concepts |>
            filter(concept_id == input$compare_concept) |>
            select(scope, version, status, sql_expr)

        selected <- versions[rows, ]

        cat("=== Version Comparison ===\n\n")
        for (i in seq_len(nrow(selected))) {
            cat(paste0("--- ", selected$scope[i], " v", selected$version[i],
                       " (", selected$status[i], ") ---\n"))
            cat(selected$sql_expr[i], "\n\n")
        }
    })

    # Search functionality
    output$search_results <- renderDT({
        req(rv$connected, input$search_sql, nchar(input$search_sql) > 0)

        keyword <- input$search_sql

        rv$concepts |>
            filter(grepl(keyword, sql_expr, ignore.case = TRUE)) |>
            select(concept_id, scope, version, status, sql_expr) |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE
            )
    })

    # --------------------------------------------------------------------------
    # Templates Tab
    # --------------------------------------------------------------------------

    # Filtered templates
    filtered_templates <- reactive({
        if (!rv$connected || is.null(rv$templates)) return(tibble())

        df <- rv$templates

        if (input$template_object_type != "") {
            df <- df |> filter(object_type == input$template_object_type)
        }

        if (input$template_standard != "") {
            df <- df |> filter(source_standard == input$template_standard)
        }

        df
    })

    # Templates table
    output$templates_table <- renderDT({
        req(rv$connected)

        filtered_templates() |>
            select(template_id, template_name, object_type, source_standard, description) |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                selection = "single",
                rownames = FALSE
            )
    })

    # Handle template selection
    observeEvent(input$templates_table_rows_selected, {
        row_idx <- input$templates_table_rows_selected
        if (length(row_idx) > 0) {
            rv$selected_template <- filtered_templates()[row_idx, ]
        }
    })

    # Template details panel
    output$template_details_panel <- renderUI({
        if (is.null(rv$selected_template)) {
            return(p("Select a template to view details", class = "text-muted"))
        }

        template <- rv$selected_template

        # Parse parameters
        params_text <- if (!is.na(template$parameters)) {
            jsonlite::prettify(template$parameters)
        } else {
            "(no parameters)"
        }

        card(
            card_header(paste("Template:", template$template_name)),
            card_body(
                tags$dl(class = "row",
                    tags$dt(class = "col-sm-3", "ID"),
                    tags$dd(class = "col-sm-9", template$template_id),
                    tags$dt(class = "col-sm-3", "Object Type"),
                    tags$dd(class = "col-sm-9", template$object_type),
                    tags$dt(class = "col-sm-3", "Source Standard"),
                    tags$dd(class = "col-sm-9", template$source_standard %||% "(none)"),
                    tags$dt(class = "col-sm-3", "Description"),
                    tags$dd(class = "col-sm-9", template$description)
                ),
                tags$h6("Base SQL Expression"),
                tags$pre(style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
                    template$base_sql_expr),
                tags$h6("Parameters"),
                tags$pre(style = "background: #f5f5f5; padding: 10px; border-radius: 4px;",
                    params_text)
            )
        )
    })

    # Variant comparison
    output$variant_comparison_table <- renderDT({
        req(rv$connected, input$variant_template)

        variants <- tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT
                    ti.concept_id,
                    c.description,
                    ti.inheritance_type,
                    ti.parameter_values,
                    ti.deviation_notes
                FROM ont_template_inheritance ti
                JOIN ont_concepts c ON ti.concept_id = c.concept_id
                WHERE ti.template_id = ?
            ", params = list(input$variant_template)) |> as_tibble(),
            error = function(e) tibble()
        )

        if (nrow(variants) == 0) {
            return(datatable(tibble(message = "No variants found")))
        }

        variants |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE
            )
    })

    # --------------------------------------------------------------------------
    # Audits & Drift Tab
    # --------------------------------------------------------------------------

    # Audits table
    output$audits_table <- renderDT({
        req(rv$connected)

        sql <- "SELECT * FROM ont_audits WHERE 1=1"
        params <- list()

        if (input$audit_concept != "") {
            sql <- paste(sql, "AND concept_id = ?")
            params <- c(params, input$audit_concept)
        }

        sql <- paste(sql, "AND audited_at BETWEEN ? AND ?")
        params <- c(params, as.character(input$audit_date_range[1]),
                    as.character(input$audit_date_range[2]))

        sql <- paste(sql, "ORDER BY audited_at DESC LIMIT 500")

        audits <- tryCatch(
            if (length(params) > 0) {
                DBI::dbGetQuery(rv$con, sql, params = params)
            } else {
                DBI::dbGetQuery(rv$con, sql)
            } |> as_tibble(),
            error = function(e) tibble()
        )

        audits |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE
            )
    })

    # Drift summary table
    output$drift_summary_table <- renderDT({
        req(rv$connected)

        drift_summary <- tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT
                    concept_id,
                    scope,
                    version,
                    COUNT(*) AS audit_count,
                    SUM(CASE WHEN system_value = reviewer_value THEN 1 ELSE 0 END) AS agreements,
                    SUM(CASE WHEN system_value != reviewer_value THEN 1 ELSE 0 END) AS disagreements,
                    ROUND(1.0 * SUM(CASE WHEN system_value != reviewer_value THEN 1 ELSE 0 END) / COUNT(*), 4) AS disagreement_rate
                FROM ont_audits
                GROUP BY concept_id, scope, version
                HAVING COUNT(*) > 0
                ORDER BY disagreement_rate DESC
            ") |> as_tibble(),
            error = function(e) tibble()
        )

        drift_summary |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE
            ) |>
            formatPercentage("disagreement_rate", digits = 1)
    })

    # Drift chart
    output$drift_chart <- renderPlot({
        req(rv$connected)

        drift_summary <- tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT concept_id, scope,
                       ROUND(1.0 * SUM(CASE WHEN system_value != reviewer_value THEN 1 ELSE 0 END) / COUNT(*), 4) AS disagreement_rate
                FROM ont_audits
                GROUP BY concept_id, scope
                HAVING COUNT(*) >= 5
            ") |> as_tibble(),
            error = function(e) tibble()
        )

        if (nrow(drift_summary) == 0) {
            plot.new()
            text(0.5, 0.5, "No drift data available", cex = 1.5)
            return()
        }

        drift_summary$label <- paste(drift_summary$concept_id, drift_summary$scope, sep = "@")

        par(mar = c(8, 4, 2, 2))
        barplot(
            drift_summary$disagreement_rate * 100,
            names.arg = drift_summary$label,
            las = 2,
            col = ifelse(drift_summary$disagreement_rate > 0.1, "coral", "steelblue"),
            ylab = "Disagreement Rate (%)",
            main = "Drift by Concept/Scope"
        )
        abline(h = 10, col = "red", lty = 2)
    })

    # Drift events table
    output$drift_events_table <- renderDT({
        req(rv$connected)

        drift_events <- tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT * FROM ont_drift_events ORDER BY detected_at DESC LIMIT 100
            ") |> as_tibble(),
            error = function(e) tibble()
        )

        drift_events |>
            datatable(
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE
            )
    })

    # --------------------------------------------------------------------------
    # Object Types Tab
    # --------------------------------------------------------------------------

    output$object_types_table <- renderDT({
        req(rv$connected)

        tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_object_types") |>
                as_tibble() |>
                datatable(
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE
                ),
            error = function(e) datatable(tibble(message = "Could not load object types"))
        )
    })

    output$link_types_table <- renderDT({
        req(rv$connected)

        tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_link_types") |>
                as_tibble() |>
                datatable(
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE
                ),
            error = function(e) datatable(tibble(message = "Could not load link types"))
        )
    })

    # --------------------------------------------------------------------------
    # Governance Tab
    # --------------------------------------------------------------------------

    output$governance_log_table <- renderDT({
        req(rv$connected)

        tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT * FROM ont_governance_log ORDER BY action_at DESC LIMIT 200
            ") |>
                as_tibble() |>
                datatable(
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE
                ),
            error = function(e) datatable(tibble(message = "Could not load governance log"))
        )
    })

    output$pending_approvals_table <- renderDT({
        req(rv$connected)

        tryCatch(
            DBI::dbGetQuery(rv$con, "
                SELECT * FROM ont_approval_requests WHERE status = 'pending' ORDER BY requested_at DESC
            ") |>
                as_tibble() |>
                datatable(
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE
                ),
            error = function(e) datatable(tibble(message = "Could not load pending approvals"))
        )
    })

    output$gates_table <- renderDT({
        req(rv$connected)

        tryCatch(
            DBI::dbGetQuery(rv$con, "SELECT * FROM ont_governance_gates") |>
                as_tibble() |>
                datatable(
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE
                ),
            error = function(e) datatable(tibble(message = "Could not load governance gates"))
        )
    })

    # --------------------------------------------------------------------------
    # Cleanup
    # --------------------------------------------------------------------------

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
