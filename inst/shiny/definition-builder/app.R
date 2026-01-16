# =============================================================================
# Definition Builder Shiny App
# =============================================================================
# Visual SQL builder for non-technical users to create concept definitions
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
    title = "ontologyR Definition Builder",
    theme = bs_theme(version = 5, bootswatch = "minty"),

    # Build Tab
    nav_panel(
        "Build Definition",
        layout_sidebar(
            sidebar = sidebar(
                width = 350,
                card(
                    card_header("Database"),
                    textInput("db_path", "Database Path", value = initial_db_path),
                    actionButton("connect_btn", "Connect", class = "btn-primary w-100"),
                    textOutput("connection_status")
                ),
                card(
                    card_header("1. Select Object Type"),
                    selectInput("object_type", "Object Type", choices = NULL),
                    textOutput("table_info")
                ),
                card(
                    card_header("2. Available Columns"),
                    DTOutput("columns_table", height = "200px")
                )
            ),

            # Main content
            card(
                card_header(
                    class = "d-flex justify-content-between align-items-center",
                    "3. Build Conditions",
                    actionButton("add_condition", "Add Condition", class = "btn-success btn-sm")
                ),
                card_body(
                    div(id = "conditions_container",
                        uiOutput("conditions_ui")
                    ),
                    hr(),
                    h6("Combine Conditions With:"),
                    radioButtons("combine_logic", NULL,
                        choices = c("AND (all must be true)" = "AND",
                                    "OR (any can be true)" = "OR"),
                        inline = TRUE
                    )
                )
            ),
            card(
                card_header("4. Generated SQL"),
                card_body(
                    verbatimTextOutput("generated_sql"),
                    actionButton("copy_sql", "Copy SQL", class = "btn-secondary btn-sm")
                )
            ),
            card(
                card_header("5. Test & Preview"),
                card_body(
                    fluidRow(
                        column(6, actionButton("test_btn", "Test Definition", class = "btn-info w-100")),
                        column(6, textOutput("test_summary"))
                    ),
                    DTOutput("preview_table")
                )
            )
        )
    ),

    # Save Tab
    nav_panel(
        "Save Concept",
        card(
            card_header("Save as New Concept"),
            card_body(
                textInput("concept_id", "Concept ID", placeholder = "e.g., high_risk_patient"),
                textInput("concept_description", "Description",
                    placeholder = "Human-readable description of what this concept means"),
                textInput("scope", "Scope", value = "default",
                    placeholder = "e.g., clinical, regulatory, operational"),
                selectInput("status", "Initial Status",
                    choices = c("draft", "active"),
                    selected = "draft"),
                textAreaInput("rationale", "Rationale",
                    placeholder = "Why is this definition appropriate?",
                    rows = 3),
                textInput("created_by", "Your Name", placeholder = "Who is creating this?"),
                hr(),
                fluidRow(
                    column(6, actionButton("save_btn", "Save Concept", class = "btn-primary w-100")),
                    column(6, actionButton("clear_btn", "Clear Form", class = "btn-warning w-100"))
                ),
                hr(),
                uiOutput("save_result")
            )
        )
    ),

    # Templates Tab
    nav_panel(
        "Use Template",
        layout_sidebar(
            sidebar = sidebar(
                width = 300,
                card(
                    card_header("Available Templates"),
                    selectInput("template_select", "Select Template", choices = NULL),
                    actionButton("load_template", "Load Template", class = "btn-info w-100")
                )
            ),
            card(
                card_header("Template Details"),
                uiOutput("template_details")
            ),
            card(
                card_header("Customize Parameters"),
                uiOutput("template_params_ui")
            ),
            card(
                card_header("Generated SQL from Template"),
                verbatimTextOutput("template_sql")
            )
        )
    ),

    # Help Tab
    nav_panel(
        "Help",
        card(
            card_header("How to Use the Definition Builder"),
            card_body(
                h5("Step 1: Connect to Database"),
                p("Enter the path to your ontologyR database and click Connect."),

                h5("Step 2: Select Object Type"),
                p("Choose the type of object you want to define a concept for (e.g., Patient, Encounter)."),

                h5("Step 3: Build Conditions"),
                p("Add conditions that define your concept:"),
                tags$ul(
                    tags$li(tags$strong("Column:"), " The field to check"),
                    tags$li(tags$strong("Operator:"), " How to compare (equals, greater than, etc.)"),
                    tags$li(tags$strong("Value:"), " The value to compare against")
                ),

                h5("Step 4: Test Your Definition"),
                p("Click 'Test Definition' to see how many records match and preview the results."),

                h5("Step 5: Save"),
                p("Go to the 'Save Concept' tab to save your definition with metadata."),

                hr(),
                h5("Operators Reference"),
                tags$table(class = "table table-sm",
                    tags$tr(tags$th("Operator"), tags$th("Meaning"), tags$th("Example")),
                    tags$tr(tags$td("="), tags$td("Equals"), tags$td("status = 'active'")),
                    tags$tr(tags$td("!="), tags$td("Not equals"), tags$td("status != 'closed'")),
                    tags$tr(tags$td(">"), tags$td("Greater than"), tags$td("age > 18")),
                    tags$tr(tags$td(">="), tags$td("Greater or equal"), tags$td("score >= 80")),
                    tags$tr(tags$td("<"), tags$td("Less than"), tags$td("days < 30")),
                    tags$tr(tags$td("<="), tags$td("Less or equal"), tags$td("priority <= 2")),
                    tags$tr(tags$td("LIKE"), tags$td("Pattern match"), tags$td("name LIKE 'Dr.%'")),
                    tags$tr(tags$td("IN"), tags$td("In list"), tags$td("dept IN ('ICU','ER')")),
                    tags$tr(tags$td("IS NULL"), tags$td("Is missing"), tags$td("end_date IS NULL")),
                    tags$tr(tags$td("IS NOT NULL"), tags$td("Has value"), tags$td("email IS NOT NULL"))
                )
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
        conditions = list(),
        condition_counter = 0,
        columns = NULL,
        object_types = NULL,
        templates = NULL
    )

    # -------------------------------------------------------------------------
    # Connection
    # -------------------------------------------------------------------------

    observeEvent(input$connect_btn, {
        tryCatch({
            if (!is.null(rv$con)) {
                DBI::dbDisconnect(rv$con)
            }

            rv$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = input$db_path)
            rv$connected <- TRUE

            # Load object types
            rv$object_types <- tryCatch(
                DBI::dbGetQuery(rv$con, "SELECT * FROM ont_object_types") |> as_tibble(),
                error = function(e) tibble()
            )

            # Load templates
            rv$templates <- tryCatch(
                DBI::dbGetQuery(rv$con, "SELECT * FROM ont_templates") |> as_tibble(),
                error = function(e) tibble()
            )

            # Update dropdowns
            if (nrow(rv$object_types) > 0) {
                choices <- setNames(rv$object_types$object_type, rv$object_types$object_type)
                updateSelectInput(session, "object_type", choices = choices)
            }

            if (nrow(rv$templates) > 0) {
                choices <- setNames(rv$templates$template_id, rv$templates$template_name)
                updateSelectInput(session, "template_select", choices = choices)
            }

            showNotification("Connected!", type = "message")
        }, error = function(e) {
            rv$connected <- FALSE
            showNotification(paste("Error:", e$message), type = "error")
        })
    })

    output$connection_status <- renderText({
        if (rv$connected) "Connected" else "Not connected"
    })

    # -------------------------------------------------------------------------
    # Object Type Selection
    # -------------------------------------------------------------------------

    observeEvent(input$object_type, {
        req(rv$connected, input$object_type)

        # Get table info
        obj_info <- rv$object_types |> filter(object_type == input$object_type)

        if (nrow(obj_info) > 0) {
            table_name <- obj_info$table_name[1]

            # Get columns
            tryCatch({
                rv$columns <- DBI::dbGetQuery(rv$con,
                    paste0("PRAGMA table_info('", table_name, "')")
                ) |> as_tibble()
            }, error = function(e) {
                rv$columns <- tibble()
            })
        }
    })

    output$table_info <- renderText({
        req(input$object_type)
        obj_info <- rv$object_types |> filter(object_type == input$object_type)
        if (nrow(obj_info) > 0) {
            paste("Table:", obj_info$table_name[1])
        }
    })

    output$columns_table <- renderDT({
        req(rv$columns)
        rv$columns |>
            select(name, type) |>
            datatable(
                options = list(
                    dom = 't',
                    pageLength = 50,
                    scrollY = "150px"
                ),
                selection = "none",
                rownames = FALSE
            )
    })

    # -------------------------------------------------------------------------
    # Condition Builder
    # -------------------------------------------------------------------------

    observeEvent(input$add_condition, {
        rv$condition_counter <- rv$condition_counter + 1
        new_id <- paste0("cond_", rv$condition_counter)
        rv$conditions[[new_id]] <- list(
            id = new_id,
            column = "",
            operator = "=",
            value = ""
        )
    })

    # Remove condition
    observe({
        # Listen for any remove button
        lapply(names(rv$conditions), function(cond_id) {
            remove_id <- paste0("remove_", cond_id)
            observeEvent(input[[remove_id]], {
                rv$conditions[[cond_id]] <- NULL
            }, ignoreInit = TRUE)
        })
    })

    output$conditions_ui <- renderUI({
        if (length(rv$conditions) == 0) {
            return(p("No conditions added yet. Click 'Add Condition' to start.",
                class = "text-muted"))
        }

        # Get column choices
        col_choices <- if (!is.null(rv$columns) && nrow(rv$columns) > 0) {
            c("Select column..." = "", rv$columns$name)
        } else {
            c("Connect first" = "")
        }

        operator_choices <- c(
            "= (equals)" = "=",
            "!= (not equals)" = "!=",
            "> (greater than)" = ">",
            ">= (greater or equal)" = ">=",
            "< (less than)" = "<",
            "<= (less or equal)" = "<=",
            "LIKE (pattern)" = "LIKE",
            "IN (list)" = "IN",
            "IS NULL" = "IS NULL",
            "IS NOT NULL" = "IS NOT NULL"
        )

        # Build UI for each condition
        condition_uis <- lapply(names(rv$conditions), function(cond_id) {
            cond <- rv$conditions[[cond_id]]

            div(class = "card mb-2", style = "background: #f8f9fa;",
                div(class = "card-body p-2",
                    fluidRow(
                        column(4,
                            selectInput(
                                paste0("col_", cond_id),
                                NULL,
                                choices = col_choices,
                                selected = cond$column
                            )
                        ),
                        column(3,
                            selectInput(
                                paste0("op_", cond_id),
                                NULL,
                                choices = operator_choices,
                                selected = cond$operator
                            )
                        ),
                        column(4,
                            textInput(
                                paste0("val_", cond_id),
                                NULL,
                                value = cond$value,
                                placeholder = "Value"
                            )
                        ),
                        column(1,
                            actionButton(
                                paste0("remove_", cond_id),
                                icon("trash"),
                                class = "btn-danger btn-sm"
                            )
                        )
                    )
                )
            )
        })

        do.call(tagList, condition_uis)
    })

    # -------------------------------------------------------------------------
    # SQL Generation
    # -------------------------------------------------------------------------

    generated_sql <- reactive({
        if (length(rv$conditions) == 0) {
            return("-- Add conditions to build SQL")
        }

        # Collect current values from inputs
        clauses <- lapply(names(rv$conditions), function(cond_id) {
            col <- input[[paste0("col_", cond_id)]]
            op <- input[[paste0("op_", cond_id)]]
            val <- input[[paste0("val_", cond_id)]]

            if (is.null(col) || col == "") return(NULL)

            # Handle different operators
            if (op %in% c("IS NULL", "IS NOT NULL")) {
                paste(col, op)
            } else if (op == "IN") {
                # Expect comma-separated values
                paste0(col, " IN (", val, ")")
            } else if (op == "LIKE") {
                paste0(col, " LIKE '", val, "'")
            } else {
                # Check if value looks numeric
                if (!is.na(suppressWarnings(as.numeric(val)))) {
                    paste(col, op, val)
                } else if (val %in% c("TRUE", "FALSE")) {
                    paste(col, op, val)
                } else {
                    paste0(col, " ", op, " '", val, "'")
                }
            }
        })

        clauses <- Filter(Negate(is.null), clauses)

        if (length(clauses) == 0) {
            return("-- Select columns and values")
        }

        combiner <- if (input$combine_logic == "AND") " AND " else " OR "
        paste(clauses, collapse = combiner)
    })

    output$generated_sql <- renderText({
        generated_sql()
    })

    # -------------------------------------------------------------------------
    # Test & Preview
    # -------------------------------------------------------------------------

    test_results <- reactiveVal(NULL)

    observeEvent(input$test_btn, {
        req(rv$connected, input$object_type)

        sql_expr <- generated_sql()
        if (grepl("^--", sql_expr)) {
            showNotification("Add conditions first", type = "warning")
            return()
        }

        obj_info <- rv$object_types |> filter(object_type == input$object_type)
        table_name <- obj_info$table_name[1]
        pk_col <- obj_info$pk_column[1]

        tryCatch({
            # Build test query
            test_sql <- paste0(
                "SELECT *, CASE WHEN (", sql_expr, ") THEN TRUE ELSE FALSE END AS concept_match ",
                "FROM ", table_name
            )

            result <- DBI::dbGetQuery(rv$con, test_sql) |> as_tibble()
            test_results(result)

            showNotification("Test complete!", type = "message")
        }, error = function(e) {
            showNotification(paste("SQL Error:", e$message), type = "error")
            test_results(NULL)
        })
    })

    output$test_summary <- renderText({
        result <- test_results()
        if (is.null(result)) return("")

        matches <- sum(result$concept_match, na.rm = TRUE)
        total <- nrow(result)
        pct <- round(100 * matches / total, 1)

        paste0(matches, " of ", total, " match (", pct, "%)")
    })

    output$preview_table <- renderDT({
        result <- test_results()
        req(result)

        result |>
            datatable(
                options = list(
                    pageLength = 10,
                    scrollX = TRUE
                ),
                rownames = FALSE
            ) |>
            formatStyle(
                "concept_match",
                backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
            )
    })

    # -------------------------------------------------------------------------
    # Save Concept
    # -------------------------------------------------------------------------

    observeEvent(input$save_btn, {
        req(rv$connected, input$object_type, input$concept_id)

        sql_expr <- generated_sql()
        if (grepl("^--", sql_expr)) {
            showNotification("Build a definition first", type = "warning")
            return()
        }

        tryCatch({
            # Check if concept exists
            existing <- DBI::dbGetQuery(rv$con,
                "SELECT concept_id FROM ont_concepts WHERE concept_id = ?",
                params = list(input$concept_id)
            )

            if (nrow(existing) > 0) {
                showNotification("Concept ID already exists. Choose a different ID.",
                    type = "error")
                return()
            }

            # Create concept
            DBI::dbExecute(rv$con, "
                INSERT INTO ont_concepts (concept_id, object_type, description, created_by)
                VALUES (?, ?, ?, ?)
            ", params = list(
                input$concept_id,
                input$object_type,
                input$concept_description,
                input$created_by
            ))

            # Create version
            DBI::dbExecute(rv$con, "
                INSERT INTO ont_concept_versions
                (concept_id, scope, version, sql_expr, status, rationale, created_by)
                VALUES (?, ?, 1, ?, ?, ?, ?)
            ", params = list(
                input$concept_id,
                input$scope,
                sql_expr,
                input$status,
                input$rationale,
                input$created_by
            ))

            showNotification(paste("Saved concept:", input$concept_id), type = "message")

            output$save_result <- renderUI({
                div(class = "alert alert-success",
                    h5("Concept Saved Successfully!"),
                    p("ID: ", tags$strong(input$concept_id)),
                    p("Scope: ", input$scope, " | Version: 1"),
                    p("Status: ", input$status)
                )
            })

        }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
        })
    })

    observeEvent(input$clear_btn, {
        updateTextInput(session, "concept_id", value = "")
        updateTextInput(session, "concept_description", value = "")
        updateTextInput(session, "scope", value = "default")
        updateTextAreaInput(session, "rationale", value = "")
        updateTextInput(session, "created_by", value = "")
        output$save_result <- renderUI({})
    })

    # -------------------------------------------------------------------------
    # Templates
    # -------------------------------------------------------------------------

    selected_template <- reactiveVal(NULL)

    observeEvent(input$load_template, {
        req(rv$connected, input$template_select)

        template <- rv$templates |> filter(template_id == input$template_select)
        if (nrow(template) > 0) {
            selected_template(as.list(template[1, ]))
        }
    })

    output$template_details <- renderUI({
        template <- selected_template()
        if (is.null(template)) {
            return(p("Select a template and click 'Load Template'", class = "text-muted"))
        }

        div(
            tags$dl(class = "row",
                tags$dt(class = "col-sm-3", "Name"),
                tags$dd(class = "col-sm-9", template$template_name),
                tags$dt(class = "col-sm-3", "Object Type"),
                tags$dd(class = "col-sm-9", template$object_type),
                tags$dt(class = "col-sm-3", "Standard"),
                tags$dd(class = "col-sm-9", template$source_standard %||% "(none)"),
                tags$dt(class = "col-sm-3", "Description"),
                tags$dd(class = "col-sm-9", template$description)
            ),
            h6("Base SQL:"),
            tags$pre(style = "background: #f5f5f5; padding: 10px;", template$base_sql_expr)
        )
    })

    output$template_params_ui <- renderUI({
        template <- selected_template()
        if (is.null(template) || is.na(template$parameters)) {
            return(p("No parameters to customize", class = "text-muted"))
        }

        params <- tryCatch(
            jsonlite::fromJSON(template$parameters),
            error = function(e) list()
        )

        if (length(params) == 0) {
            return(p("No parameters defined", class = "text-muted"))
        }

        # Build input for each parameter
        param_inputs <- lapply(names(params), function(pname) {
            pdef <- params[[pname]]
            default_val <- if (is.list(pdef)) pdef$default else pdef
            desc <- if (is.list(pdef) && !is.null(pdef$description)) pdef$description else pname

            textInput(
                paste0("tparam_", pname),
                label = paste0(pname, " (", desc, ")"),
                value = as.character(default_val)
            )
        })

        do.call(tagList, c(param_inputs, list(
            actionButton("apply_template", "Apply Parameters", class = "btn-success w-100")
        )))
    })

    output$template_sql <- renderText({
        template <- selected_template()
        if (is.null(template)) return("")

        sql <- template$base_sql_expr

        # Get parameters
        params <- tryCatch(
            jsonlite::fromJSON(template$parameters),
            error = function(e) list()
        )

        # Substitute parameter values
        for (pname in names(params)) {
            input_id <- paste0("tparam_", pname)
            val <- input[[input_id]]
            if (!is.null(val)) {
                placeholder <- paste0("{{", pname, "}}")
                sql <- gsub(placeholder, val, sql, fixed = TRUE)
            }
        }

        # Clean up
        trimws(gsub("\\s+", " ", sql))
    })

    # -------------------------------------------------------------------------
    # Cleanup
    # -------------------------------------------------------------------------

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
