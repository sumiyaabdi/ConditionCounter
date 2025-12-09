#' Launch Shiny Application
#'
#' Connects to Eunomia database. Plots occurrences of selected conditions of interest.
#'
#' @returns Runs the shiny application
#' @export
launchShinyApp <- function() {
  appDir <- system.file("shiny", package = "ConditionCounter")

  shiny::runApp(appDir, display.mode = "normal")
}
