# =============================================================================
# Example Runner
# =============================================================================
# Functions for listing and running packaged examples.
# =============================================================================

#' Run an ontologyR Example
#'
#' Runs a bundled example script that demonstrates ontologyR features.
#' Examples are self-contained and create their own in-memory databases.
#'
#' @param name Character. Name of the example to run (without .R extension).
#'   Use \code{ont_list_examples()} to see available examples.
#' @param echo Logical. Whether to echo the script lines as they run.
#'   Default TRUE.
#'
#' @return Invisibly returns the path to the example script.
#'
#' @examples
#' \dontrun{
#' # List available examples
#' ont_list_examples()
#'
#' # Run the 2D spatial viewer example
#' ont_run_example("spatial-2d")
#' }
#'
#' @seealso \code{\link{ont_list_examples}} to see available examples
#' @export
ont_run_example <- function(name, echo = TRUE) {
    # Find examples directory
    examples_dir <- system.file("examples", package = "ontologyR")

    if (examples_dir == "") {
        cli::cli_abort("Cannot find examples directory. Try reinstalling ontologyR.")
    }

    # Find the example file
    example_file <- file.path(examples_dir, paste0(name, ".R"))

    if (!file.exists(example_file)) {
        available <- ont_list_examples(quiet = TRUE)
        cli::cli_abort(c(
            "Example {.val {name}} not found.",
            "i" = "Available examples: {.val {available}}"
        ))
    }

    cli::cli_h1("Running example: {name}")
    cli::cli_alert_info("Script: {example_file}")
    cli::cli_rule()

    # Source the example
    source(example_file, echo = echo, max.deparse.length = Inf)

    invisible(example_file)
}

#' List Available Examples
#'
#' Lists all available example scripts bundled with ontologyR.
#'
#' @param quiet Logical. If TRUE, returns names without printing.
#'   Default FALSE.
#'
#' @return A character vector of example names (without .R extension).
#'
#' @examples
#' \dontrun{
#' ont_list_examples()
#' }
#'
#' @seealso \code{\link{ont_run_example}} to run an example
#' @export
ont_list_examples <- function(quiet = FALSE) {
    examples_dir <- system.file("examples", package = "ontologyR")

    if (examples_dir == "") {
        if (!quiet) {
            cli::cli_alert_warning("No examples found in this installation")
        }
        return(character())
    }

    # Find all .R files
    files <- list.files(examples_dir, pattern = "\\.R$", full.names = FALSE)
    names <- tools::file_path_sans_ext(files)

    if (length(names) == 0) {
        if (!quiet) {
            cli::cli_alert_warning("No examples found")
        }
        return(character())
    }

    if (!quiet) {
        cli::cli_h2("Available ontologyR Examples")

        # Read first line of each file to get description
        descriptions <- vapply(files, function(f) {
            lines <- readLines(file.path(examples_dir, f), n = 5, warn = FALSE)
            # Look for "Example:" line
            desc_line <- grep("^# Example:", lines, value = TRUE)
            if (length(desc_line) > 0) {
                sub("^# Example:\\s*", "", desc_line[1])
            } else {
                ""
            }
        }, character(1))

        for (i in seq_along(names)) {
            if (descriptions[i] != "") {
                cli::cli_li("{.strong {names[i]}}: {descriptions[i]}")
            } else {
                cli::cli_li("{.strong {names[i]}}")
            }
        }

        cli::cli_rule()
        cli::cli_alert_info("Run with: {.code ont_run_example(\"{names[1]}\")}")
    }

    invisible(names)
}
