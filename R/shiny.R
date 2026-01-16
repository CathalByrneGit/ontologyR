#' @title Shiny Applications
#' @description Launch interactive Shiny applications for ontologyR.
#' @name shiny-apps
NULL

#' Run Lineage Visualizer Shiny App
#'
#' Launches an interactive Shiny application for visualizing data lineage,
#' datasets, transforms, and their relationships in a directed acyclic graph.
#'
#' @param db_path Character. Path to the DuckDB database file. If NULL, the app
#'   will prompt for connection details.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default TRUE.
#'
#' @details
#' The Lineage Visualizer provides:
#' \itemize{
#'   \item Interactive DAG visualization of datasets and transforms
#'   \item Upstream/downstream dependency exploration
#'   \item Searchable tables for datasets, transforms, and runs
#'   \item Impact analysis to understand change propagation
#' }
#'
#' Node types are distinguished by shape and color:
#' \itemize{
#'   \item Source datasets: Gray database icons
#'   \item Materialized datasets: Green diamonds
#'   \item Derived datasets: Blue boxes
#'   \item Transforms: Yellow squares
#' }
#'
#' @return This function runs the Shiny app and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' ont_run_lineage_viewer()
#'
#' # Launch pointing to a specific database
#' ont_run_lineage_viewer(db_path = "my_ontology.duckdb")
#' }
#'
#' @export
ont_run_lineage_viewer <- function(db_path = NULL, launch.browser = TRUE) {
    # Check for required packages
    required_pkgs <- c("shiny", "bslib", "visNetwork", "DT")
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

    if (length(missing) > 0) {
        cli::cli_abort(c(
            "Missing required packages for Lineage Visualizer:",
            "i" = "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
        ))
    }

    # Find app directory
    app_dir <- system.file("shiny", "lineage-viewer", package = "ontologyR")

    if (app_dir == "") {
        cli::cli_abort("Cannot find Lineage Visualizer app. Try reinstalling ontologyR.")
    }

    # If db_path provided, set it as an option for the app to pick up
    if (!is.null(db_path)) {
        options(ontologyR.shiny.db_path = db_path)
        on.exit(options(ontologyR.shiny.db_path = NULL), add = TRUE)
    }

    cli::cli_alert_info("Launching Lineage Visualizer...")
    cli::cli_alert_info("Press Ctrl+C or close browser window to stop")

    shiny::runApp(
        appDir = app_dir,
        launch.browser = launch.browser,
        display.mode = "normal"
    )
}

#' Check Available Shiny Apps
#'
#' Lists all available Shiny applications in the ontologyR package.
#'
#' @return A character vector of app names.
#'
#' @examples
#' \dontrun{
#' ont_list_apps()
#' }
#'
#' @export
ont_list_apps <- function() {
    apps_dir <- system.file("shiny", package = "ontologyR")

    if (apps_dir == "") {
        cli::cli_alert_warning("No Shiny apps found in this installation")
        return(character())
    }

    apps <- list.dirs(apps_dir, full.names = FALSE, recursive = FALSE)

    if (length(apps) == 0) {
        cli::cli_alert_warning("No Shiny apps found")
        return(character())
    }

    cli::cli_h2("Available ontologyR Shiny Apps")
    cli::cli_ul(apps)
    cli::cli_alert_info("Run with: ont_run_lineage_viewer()")

    invisible(apps)
}
