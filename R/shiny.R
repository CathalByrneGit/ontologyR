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

#' Run Ontology Explorer Shiny App
#'
#' Launches an interactive Shiny application for exploring concepts, versions,
#' templates, audits, drift events, and governance information.
#'
#' @param db_path Character. Path to the DuckDB database file. If NULL, the app
#'   will prompt for connection details.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default TRUE.
#'
#' @details
#' The Ontology Explorer provides:
#' \itemize{
#'   \item Browse and filter concepts by object type, status, and scope
#'   \item Compare different versions of a concept
#'   \item Search SQL expressions across all concepts
#'   \item Explore templates and compare variants
#'   \item View audits and drift summaries
#'   \item Review governance logs and pending approvals
#' }
#'
#' @return This function runs the Shiny app and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' ont_run_explorer()
#'
#' # Launch pointing to a specific database
#' ont_run_explorer(db_path = "my_ontology.duckdb")
#' }
#'
#' @export
ont_run_explorer <- function(db_path = NULL, launch.browser = TRUE) {
    # Check for required packages
    required_pkgs <- c("shiny", "bslib", "DT")
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

    if (length(missing) > 0) {
        cli::cli_abort(c(
            "Missing required packages for Ontology Explorer:",
            "i" = "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
        ))
    }

    # Find app directory
    app_dir <- system.file("shiny", "ontology-explorer", package = "ontologyR")

    if (app_dir == "") {
        cli::cli_abort("Cannot find Ontology Explorer app. Try reinstalling ontologyR.")
    }

    # If db_path provided, set it as an option for the app to pick up
    if (!is.null(db_path)) {
        options(ontologyR.shiny.db_path = db_path)
        on.exit(options(ontologyR.shiny.db_path = NULL), add = TRUE)
    }

    cli::cli_alert_info("Launching Ontology Explorer...")
    cli::cli_alert_info("Press Ctrl+C or close browser window to stop")

    shiny::runApp(
        appDir = app_dir,
        launch.browser = launch.browser,
        display.mode = "normal"
    )
}

#' Run Definition Builder Shiny App
#'
#' Launches an interactive Shiny application for building concept definitions
#' visually without writing SQL. Designed for non-technical users.
#'
#' @param db_path Character. Path to the DuckDB database file. If NULL, the app
#'   will prompt for connection details.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default TRUE.
#'
#' @details
#' The Definition Builder provides:
#' \itemize{
#'   \item Visual condition builder with column selection and operators
#'   \item Real-time SQL preview
#'   \item Test definitions against actual data
#'   \item Save concepts directly to the database
#'   \item Template support for creating variants
#' }
#'
#' @return This function runs the Shiny app and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' ont_run_definition_builder()
#'
#' # Launch pointing to a specific database
#' ont_run_definition_builder(db_path = "my_ontology.duckdb")
#' }
#'
#' @export
ont_run_definition_builder <- function(db_path = NULL, launch.browser = TRUE) {
    # Check for required packages
    required_pkgs <- c("shiny", "bslib", "DT")
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

    if (length(missing) > 0) {
        cli::cli_abort(c(
            "Missing required packages for Definition Builder:",
            "i" = "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
        ))
    }

    # Find app directory
    app_dir <- system.file("shiny", "definition-builder", package = "ontologyR")

    if (app_dir == "") {
        cli::cli_abort("Cannot find Definition Builder app. Try reinstalling ontologyR.")
    }

    # If db_path provided, set it as an option for the app to pick up
    if (!is.null(db_path)) {
        options(ontologyR.shiny.db_path = db_path)
        on.exit(options(ontologyR.shiny.db_path = NULL), add = TRUE)
    }

    cli::cli_alert_info("Launching Definition Builder...")
    cli::cli_alert_info("Press Ctrl+C or close browser window to stop")

    shiny::runApp(
        appDir = app_dir,
        launch.browser = launch.browser,
        display.mode = "normal"
    )
}

#' Run Spatial Viewer Shiny App (3D - CesiumJS)
#'
#' Launches an interactive Shiny application for visualizing ontology data
#' on a CesiumJS 3D globe. Objects are colored by concept evaluations or
#' composite scores.
#'
#' @param db_path Character. Path to the DuckDB database file. If NULL, the app
#'   will prompt for connection details.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default TRUE.
#'
#' @details
#' The 3D Spatial Viewer provides:
#' \itemize{
#'   \item Interactive 3D globe powered by CesiumJS
#'   \item Visualize objects with registered geometry on the map
#'   \item Color objects by concept evaluation (TRUE/FALSE) or score value
#'   \item Click features to see property details
#'   \item Filter by spatial regions
#'   \item Export data to GeoJSON
#' }
#'
#' Before using, register geometry for your object types:
#' \code{ont_register_geometry("Asset", "point", lon_column = "lon", lat_column = "lat")}
#'
#' For best results, set a Cesium Ion access token:
#' \code{Sys.setenv(CESIUM_ION_TOKEN = "your-token-here")}
#'
#' For a lighter 2D alternative that doesn't require WebGL or tokens,
#' see \code{\link{ont_run_spatial_viewer_2d}}.
#'
#' @return This function runs the Shiny app and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' ont_run_spatial_viewer()
#'
#' # Launch pointing to a specific database
#' ont_run_spatial_viewer(db_path = "my_ontology.duckdb")
#' }
#'
#' @seealso \code{\link{ont_run_spatial_viewer_2d}} for 2D Leaflet-based viewer
#' @export
ont_run_spatial_viewer <- function(db_path = NULL, launch.browser = TRUE) {
    # Check for required packages
    required_pkgs <- c("shiny", "bslib", "DT")
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

    if (length(missing) > 0) {
        cli::cli_abort(c(
            "Missing required packages for Spatial Viewer:",
            "i" = "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
        ))
    }

    # Find app directory
    app_dir <- system.file("shiny", "spatial-viewer", package = "ontologyR")

    if (app_dir == "") {
        cli::cli_abort("Cannot find Spatial Viewer app. Try reinstalling ontologyR.")
    }

    # If db_path provided, set environment variable for the app
    if (!is.null(db_path)) {
        Sys.setenv(ONTOLOGYR_DB = db_path)
        on.exit(Sys.unsetenv("ONTOLOGYR_DB"), add = TRUE)
    }

    cli::cli_alert_info("Launching Spatial Viewer (CesiumJS)...")
    cli::cli_alert_info("Press Ctrl+C or close browser window to stop")

    if (Sys.getenv("CESIUM_ION_TOKEN") == "") {
        cli::cli_alert_warning("No CESIUM_ION_TOKEN set. Some features may be limited.")
        cli::cli_alert_info("Get a free token at https://cesium.com/ion/")
    }

    shiny::runApp(
        appDir = app_dir,
        launch.browser = launch.browser,
        display.mode = "normal"
    )
}

#' Run 2D Spatial Viewer Shiny App (Leaflet)
#'
#' Launches an interactive Shiny application for visualizing ontology data
#' on a 2D Leaflet map. A lighter alternative to the CesiumJS 3D viewer that
#' doesn't require WebGL or external tokens.
#'
#' @param db_path Character. Path to the DuckDB database file. If NULL, the app
#'   will prompt for connection details.
#' @param launch.browser Logical. Whether to open the app in a browser.
#'   Default TRUE.
#'
#' @details
#' The 2D Spatial Viewer provides:
#' \itemize{
#'   \item Interactive 2D map powered by Leaflet
#'   \item Multiple base map options (OpenStreetMap, CartoDB, Esri)
#'   \item Point clustering for large datasets
#'   \item Color objects by concept evaluation (TRUE/FALSE) or score value
#'   \item Click features to see property details
#'   \item Filter by spatial regions
#'   \item Export data to GeoJSON
#'   \item Linked table selection (click row to pan to point)
#' }
#'
#' Before using, register geometry for your object types:
#' \code{ont_register_geometry("Asset", "point", lon_column = "lon", lat_column = "lat")}
#'
#' @return This function runs the Shiny app and does not return a value.
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' ont_run_spatial_viewer_2d()
#'
#' # Launch pointing to a specific database
#' ont_run_spatial_viewer_2d(db_path = "my_ontology.duckdb")
#' }
#'
#' @seealso \code{\link{ont_run_spatial_viewer}} for 3D CesiumJS-based viewer
#' @export
ont_run_spatial_viewer_2d <- function(db_path = NULL, launch.browser = TRUE) {
    # Check for required packages
    required_pkgs <- c("shiny", "bslib", "DT", "leaflet")
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

    if (length(missing) > 0) {
        cli::cli_abort(c(
            "Missing required packages for 2D Spatial Viewer:",
            "i" = "Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))"
        ))
    }

    # Find app directory
    app_dir <- system.file("shiny", "spatial-viewer-2d", package = "ontologyR")

    if (app_dir == "") {
        cli::cli_abort("Cannot find 2D Spatial Viewer app. Try reinstalling ontologyR.")
    }

    # If db_path provided, set option for the app to pick up
    if (!is.null(db_path)) {
        options(ontologyR.shiny.db_path = db_path)
        on.exit(options(ontologyR.shiny.db_path = NULL), add = TRUE)
    }

    cli::cli_alert_info("Launching 2D Spatial Viewer (Leaflet)...")
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
    cli::cli_ul(c(
        "lineage-viewer: Data lineage DAG visualization (ont_run_lineage_viewer())",
        "ontology-explorer: Browse concepts, templates, audits (ont_run_explorer())",
        "definition-builder: Visual SQL builder for concepts (ont_run_definition_builder())",
        "spatial-viewer: CesiumJS 3D map visualization (ont_run_spatial_viewer())",
        "spatial-viewer-2d: Leaflet 2D map visualization (ont_run_spatial_viewer_2d())"
    ))

    invisible(apps)
}
