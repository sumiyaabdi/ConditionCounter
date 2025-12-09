#' Launch Shiny Application
#'
#' Connects to Eunomia database. Plots occurrences of selected conditions of interest.
#'
#' @returns Runs the shiny application
#' @export
launchShinyApp <- function() {

  appDir <- system.file("shiny", package = utils::packageName())

  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
