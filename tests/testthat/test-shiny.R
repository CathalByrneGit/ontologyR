# =============================================================================
# Tests for Shiny App Functions
# =============================================================================

test_that("ont_list_apps returns character vector", {
    # This may return empty if run outside installed package context
    # but should not error
    result <- ont_list_apps()
    expect_type(result, "character")
})

test_that("lineage viewer app file exists in inst", {
    # Check the app file exists in the source package
    app_path <- file.path(
        testthat::test_path(), "..", "..", "inst", "shiny", "lineage-viewer", "app.R"
    )

    # Normalize path
    app_path <- normalizePath(app_path, mustWork = FALSE)

    expect_true(
        file.exists(app_path),
        info = paste("Expected app at:", app_path)
    )
})

test_that("app.R contains required UI elements", {
    app_path <- file.path(
        testthat::test_path(), "..", "..", "inst", "shiny", "lineage-viewer", "app.R"
    )
    app_path <- normalizePath(app_path, mustWork = FALSE)

    skip_if_not(file.exists(app_path), "App file not found")

    app_code <- readLines(app_path)
    app_text <- paste(app_code, collapse = "\n")

    # Check for required UI components
    expect_true(grepl("page_sidebar", app_text), info = "Missing page_sidebar")
    expect_true(grepl("visNetworkOutput", app_text), info = "Missing visNetworkOutput")
    expect_true(grepl("DTOutput", app_text), info = "Missing DTOutput")
    expect_true(grepl("shinyApp", app_text), info = "Missing shinyApp call")
})
test_that("app.R contains required server elements", {
    app_path <- file.path(
        testthat::test_path(), "..", "..", "inst", "shiny", "lineage-viewer", "app.R"
    )
    app_path <- normalizePath(app_path, mustWork = FALSE)

    skip_if_not(file.exists(app_path), "App file not found")

    app_code <- readLines(app_path)
    app_text <- paste(app_code, collapse = "\n")

    # Check for required server components
    expect_true(grepl("renderVisNetwork", app_text), info = "Missing renderVisNetwork")
    expect_true(grepl("renderDT", app_text), info = "Missing renderDT")
    expect_true(grepl("reactiveValues", app_text), info = "Missing reactiveValues")
    expect_true(grepl("observeEvent", app_text), info = "Missing observeEvent")
})

test_that("app.R loads required libraries", {
    app_path <- file.path(
        testthat::test_path(), "..", "..", "inst", "shiny", "lineage-viewer", "app.R"
    )
    app_path <- normalizePath(app_path, mustWork = FALSE)

    skip_if_not(file.exists(app_path), "App file not found")

    app_code <- readLines(app_path)
    app_text <- paste(app_code, collapse = "\n")

    # Check required library calls
    expect_true(grepl("library\\(shiny\\)", app_text), info = "Missing library(shiny)")
    expect_true(grepl("library\\(bslib\\)", app_text), info = "Missing library(bslib)")
    expect_true(grepl("library\\(visNetwork\\)", app_text), info = "Missing library(visNetwork)")
    expect_true(grepl("library\\(DT\\)", app_text), info = "Missing library(DT)")
})

test_that("ont_run_lineage_viewer checks for required packages", {
    # Skip if all required packages are installed (can't test the error path)
    required_pkgs <- c("shiny", "bslib", "visNetwork", "DT")
    all_installed <- all(sapply(required_pkgs, requireNamespace, quietly = TRUE))

    skip_if(all_installed, "All required packages installed, cannot test missing package error")

    # If some packages are missing, the function should error with helpful message
    expect_error(
        ont_run_lineage_viewer(),
        "Missing required packages"
    )
})

test_that("app supports db_path option", {
    app_path <- file.path(
        testthat::test_path(), "..", "..", "inst", "shiny", "lineage-viewer", "app.R"
    )
    app_path <- normalizePath(app_path, mustWork = FALSE)

    skip_if_not(file.exists(app_path), "App file not found")

    app_code <- readLines(app_path)
    app_text <- paste(app_code, collapse = "\n")

    # Check that app reads from ontologyR.shiny.db_path option
    expect_true(
        grepl("ontologyR\\.shiny\\.db_path", app_text),
        info = "App should read ontologyR.shiny.db_path option"
    )
})

test_that("R/shiny.R exports expected functions", {
    # Check that the functions are exported
    expect_true(
        "ont_run_lineage_viewer" %in% getNamespaceExports("ontologyR") ||
        exists("ont_run_lineage_viewer", envir = asNamespace("ontologyR")),
        info = "ont_run_lineage_viewer should be exported"
    )

    expect_true(
        "ont_list_apps" %in% getNamespaceExports("ontologyR") ||
        exists("ont_list_apps", envir = asNamespace("ontologyR")),
        info = "ont_list_apps should be exported"
    )
})

# =============================================================================
# Graph building helper tests (extracted from app logic)
# =============================================================================

test_that("upstream/downstream traversal logic is correct", {
    # Mock edges data frame
    edges <- tibble::tibble(
        from = c("A", "B", "C", "D"),
        to = c("B", "C", "D", "E")
    )
    # A -> B -> C -> D -> E

    # Helper function (copied from app for testing)
    get_connected_nodes <- function(edges, start_node, direction = "upstream", max_depth = 5) {
        if (nrow(edges) == 0) return(character())

        visited <- character()
        current <- start_node
        depth <- 0

        while (length(current) > 0 && depth < max_depth) {
            depth <- depth + 1

            if (direction == "upstream") {
                next_nodes <- edges |>
                    dplyr::filter(.data$to %in% current & !(.data$from %in% visited)) |>
                    dplyr::pull(.data$from) |>
                    unique()
            } else {
                next_nodes <- edges |>
                    dplyr::filter(.data$from %in% current & !(.data$to %in% visited)) |>
                    dplyr::pull(.data$to) |>
                    unique()
            }

            visited <- c(visited, current)
            current <- setdiff(next_nodes, visited)
        }

        unique(visited)
    }

    # Test downstream from A
    downstream <- get_connected_nodes(edges, "A", "downstream", 10)
    expect_true("A" %in% downstream)
    expect_true("B" %in% downstream)
    expect_true("C" %in% downstream)
    expect_true("D" %in% downstream)
    expect_true("E" %in% downstream)

    # Test upstream from E
    upstream <- get_connected_nodes(edges, "E", "upstream", 10)
    expect_true("E" %in% upstream)
    expect_true("D" %in% upstream)
    expect_true("C" %in% upstream)
    expect_true("B" %in% upstream)
    expect_true("A" %in% upstream)

    # Test with depth limit
    limited <- get_connected_nodes(edges, "A", "downstream", 2)
    expect_true("A" %in% limited)
    expect_true("B" %in% limited)
    expect_true("C" %in% limited)
    # D and E may or may not be included depending on how depth is counted

    # Test empty edges
    empty_result <- get_connected_nodes(tibble::tibble(from = character(), to = character()), "A", "downstream")
    expect_equal(empty_result, character())
})

test_that("branching graph traversal works", {
    # Create a branching graph:
    #   A -> B -> D
    #   A -> C -> D
    #   D -> E
    edges <- tibble::tibble(
        from = c("A", "A", "B", "C", "D"),
        to = c("B", "C", "D", "D", "E")
    )

    get_connected_nodes <- function(edges, start_node, direction = "upstream", max_depth = 5) {
        if (nrow(edges) == 0) return(character())

        visited <- character()
        current <- start_node
        depth <- 0

        while (length(current) > 0 && depth < max_depth) {
            depth <- depth + 1

            if (direction == "upstream") {
                next_nodes <- edges |>
                    dplyr::filter(.data$to %in% current & !(.data$from %in% visited)) |>
                    dplyr::pull(.data$from) |>
                    unique()
            } else {
                next_nodes <- edges |>
                    dplyr::filter(.data$from %in% current & !(.data$to %in% visited)) |>
                    dplyr::pull(.data$to) |>
                    unique()
            }

            visited <- c(visited, current)
            current <- setdiff(next_nodes, visited)
        }

        unique(visited)
    }

    # Downstream from A should include all nodes
    downstream <- get_connected_nodes(edges, "A", "downstream", 10)
    expect_setequal(downstream, c("A", "B", "C", "D", "E"))

    # Upstream from D should include A, B, C
    upstream <- get_connected_nodes(edges, "D", "upstream", 10)
    expect_true(all(c("A", "B", "C", "D") %in% upstream))

    # Upstream from E should include everything
    upstream_e <- get_connected_nodes(edges, "E", "upstream", 10)
    expect_setequal(upstream_e, c("A", "B", "C", "D", "E"))
})
